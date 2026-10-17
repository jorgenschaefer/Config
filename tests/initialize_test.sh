#!/usr/bin/env bash

# Tests of initialize.sh installing and enabling the backup timer.
#
# Each test runs initialize.sh against a scratch $HOME whose
# ~/Projects/Config is this repository. A fake `systemctl` stands in for
# the user's service manager: like the real one, it enables a unit only
# if its file is in ~/.config/systemd/user, by linking it into the
# target's .wants directory, and it records which units were started.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
failures=0

start_test () {
  current_test="$1"
  scratch="$(mktemp -d)"
  export HOME="$scratch/home"
  export FAKE_STARTED="$scratch/started"
  mkdir -p "$HOME/Projects" "$scratch/fakes"
  ln -s "$repo" "$HOME/Projects/Config"
  fake_systemctl > "$scratch/fakes/systemctl"
  chmod +x "$scratch/fakes/systemctl"
  touch "$FAKE_STARTED"
  export PATH="$scratch/fakes:$PATH"
}

fake_systemctl () {
  cat <<'EOF'
#!/usr/bin/env bash
units="$HOME/.config/systemd/user"
if [[ $1 != --user ]]
then
  echo "fake systemctl: only --user is supported, got: $*" >&2
  exit 1
fi
shift
case "$1 ${2:-}" in
  "daemon-reload ")
    ;;
  "enable --now")
    unit="$3"
    if ! [[ -f $units/$unit ]]
    then
      echo "Failed to enable unit: Unit file $unit does not exist." >&2
      exit 1
    fi
    target="$(sed -n 's/^WantedBy=//p' "$units/$unit")"
    mkdir -p "$units/$target.wants"
    ln -sfn "$units/$unit" "$units/$target.wants/$unit"
    grep -qx "$unit" "$FAKE_STARTED" || echo "$unit" >> "$FAKE_STARTED"
    ;;
  *)
    echo "fake systemctl: unexpected command: $*" >&2
    exit 1
    ;;
esac
EOF
}

end_test () {
  rm -rf "$scratch"
}

fail () {
  echo "FAIL: $current_test: $*" >&2
  failures=$((failures + 1))
}

run_initialize () {
  if ! output="$(sh "$repo/initialize.sh" 2>&1)"
  then
    fail "initialize.sh failed: $output"
  fi
}

assert_timer_installed_and_enabled () {
  local unit
  for unit in backup.service backup.timer
  do
    if ! [[ "$HOME/.config/systemd/user/$unit" -ef "$repo/systemd/$unit" ]]
    then
      fail "$HOME/.config/systemd/user/$unit is not a link to systemd/$unit"
    fi
  done
  if ! [[ "$HOME/.config/systemd/user/timers.target.wants/backup.timer" \
          -ef "$repo/systemd/backup.timer" ]]
  then
    fail "backup.timer is not enabled"
  fi
  if [[ "$(cat "$FAKE_STARTED")" != backup.timer ]]
  then
    fail "expected backup.timer started, started: $(cat "$FAKE_STARTED")"
  fi
}

start_test "initialize.sh installs and enables the backup timer"
run_initialize
assert_timer_installed_and_enabled
end_test

start_test "running initialize.sh again is harmless"
run_initialize
run_initialize
if [[ -n $output ]]
then
  fail "the second run printed: $output"
fi
assert_timer_installed_and_enabled
end_test

if [[ $failures -gt 0 ]]
then
  exit 1
fi
