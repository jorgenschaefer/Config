#!/usr/bin/env bash

# Tests of the systemd units' own logic. Whether systemd honours their
# settings needs a user manager and is checked by hand; check.sh only
# verifies their syntax.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
failures=0

start_test () {
  current_test="$1"
  scratch="$(mktemp -d)"
  mkdir -p "$scratch/home/.local/state/backup"
}

end_test () {
  rm -rf "$scratch"
}

fail () {
  echo "FAIL: $current_test: $*" >&2
  failures=$((failures + 1))
}

# Run backup.service's ExecCondition with %h as the scratch home, and
# keep its exit status in $status: 0 lets the run start.
run_condition () {
  local condition
  condition="$(sed -n 's/^ExecCondition=//p' "$repo/systemd/backup.service")"
  if [[ -z $condition ]]
  then
    fail "backup.service has no ExecCondition"
    status=-1
    return
  fi
  condition="${condition//%h/$scratch/home}"
  eval "$condition" > /dev/null 2>&1
  status=$?
}

start_test "the service runs when no backup has ever succeeded"
run_condition
if [[ $status != 0 ]]
then
  fail "condition exited $status"
fi
end_test

start_test "the service runs when the last backup succeeded before today"
touch -d '2 days ago' "$scratch/home/.local/state/backup/last-backup"
run_condition
if [[ $status != 0 ]]
then
  fail "condition exited $status"
fi
end_test

start_test "the service skips the run when a backup already succeeded today"
touch "$scratch/home/.local/state/backup/last-backup"
run_condition
if [[ $status == 0 || $status -ge 255 ]]
then
  fail "condition exited $status, not a skip (1 to 254)"
fi
end_test

start_test "the timer also starts the service at every login"
if ! grep -qE '^OnStartupSec=' "$repo/systemd/backup.timer"
then
  fail "backup.timer has no OnStartupSec"
fi
end_test

if [[ $failures -gt 0 ]]
then
  exit 1
fi
