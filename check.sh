#!/usr/bin/env bash

# The project's check command: lint the shell scripts, run the tests,
# verify the systemd units and check the structural rules. Every step
# runs, and the command exits non-zero if any of them failed.

set -uo pipefail

cd "$(dirname "$0")" || exit 1

main () {
  local failed=0
  lint_shell_scripts || failed=1
  run_tests || failed=1
  check_units || failed=1
  check_structure || failed=1
  return "$failed"
}

# The shell scripts are every *.sh file and every file in bin/.
lint_shell_scripts () {
  local scripts
  mapfile -d '' scripts < <(find . -path ./.git -prune -o \
                                 \( -name '*.sh' -o -path './bin/*' \) \
                                 -type f -print0 | sort -z)
  shellcheck "${scripts[@]}"
}

# A test is a file named *_test.sh. It is run with bash and fails by
# exiting non-zero.
run_tests () {
  local tests test failed=0
  mapfile -d '' tests < <(find . -path ./.git -prune -o \
                               -name '*_test.sh' -type f -print0 | sort -z)
  for test in "${tests[@]}"
  do
    if ! bash "$test"
    then
      echo "FAIL: $test" >&2
      failed=1
    fi
  done
  return "$failed"
}

# The systemd units in systemd/ are valid. systemd-analyze only warns
# about an unknown or misspelt setting, so any output fails the check.
# The units run %h/.local/bin/backup, which a scratch home provides,
# so the check does not depend on initialize.sh having run here.
check_units () {
  local home output status
  home="$(mktemp -d)"
  mkdir -p "$home/.local/bin"
  cp bin/backup "$home/.local/bin/backup"
  output="$(HOME="$home" systemd-analyze --user verify systemd/* 2>&1)"
  status=$?
  rm -rf "$home"
  if [[ $status -ne 0 || -n $output ]]
  then
    echo "systemd-analyze finds fault with the units in systemd/:" >&2
    echo "$output" >&2
    return 1
  fi
}

# Each structural rule of the project is a check called from here,
# and returns non-zero when the code breaks the rule.
check_structure () {
  local failed=0
  check_reads_clock_only_in bin/backup now \
    '[0-9]+:now \(\) \{ date \+%s; \}' || failed=1
  check_reads_clock_only_in bash.sh seconds_since_epoch \
    '[0-9]+: *function seconds_since_epoch \(\) \{ date \+%s; \}' || failed=1
  return "$failed"
}

# A script reads the clock in one named function, defined on the one
# line the pattern matches, and nowhere else.
check_reads_clock_only_in () {
  local file="$1" function="$2" definition="$3" reads
  reads="$(grep -nE '\bdate\b|EPOCHSECONDS|EPOCHREALTIME|\bSECONDS\b|%\([^)]*\)T' \
                "$file" \
             | grep -vxE "$definition")"
  if [[ -n $reads ]]
  then
    echo "$file reads the clock outside $function():" >&2
    echo "$reads" >&2
    return 1
  fi
}

main
