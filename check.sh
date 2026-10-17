#!/usr/bin/env bash

# The project's check command: lint the shell scripts, run the tests
# and check the structural rules. Every step runs, and the command
# exits non-zero if any of them failed.

set -uo pipefail

cd "$(dirname "$0")" || exit 1

main () {
  local failed=0
  lint_shell_scripts || failed=1
  run_tests || failed=1
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

# Each structural rule of the project is a check called from here,
# and returns non-zero when the code breaks the rule. There are no
# rules yet.
check_structure () {
  return 0
}

main
