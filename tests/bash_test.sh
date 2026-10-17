#!/usr/bin/env bash

# Tests of the warnings bash.sh prints in a new interactive shell when
# the last backup or the last restore is too old.
#
# Each test sources bash.sh in a fresh bash with a scratch $HOME, and a
# fake `date` in ~/.local/bin, which bash.sh puts first on PATH, that
# answers `date +%s` with $FAKE_NOW.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
failures=0

day=86400
now=1800000000

start_test () {
  current_test="$1"
  scratch="$(mktemp -d)"
  home="$scratch/home"
  mkdir -p "$home/.local/bin" "$home/.local/state/backup"
  cat > "$home/.local/bin/date" <<'EOF'
#!/bin/sh
if [ "$*" = "+%s" ]
then
  echo "$FAKE_NOW"
else
  exec /usr/bin/date "$@"
fi
EOF
  chmod +x "$home/.local/bin/date"
}

end_test () {
  rm -rf "$scratch"
}

fail () {
  echo "FAIL: $current_test: $*" >&2
  failures=$((failures + 1))
}

# Record a backup or restore the given number of seconds before now.
recorded_ago () {
  local name="$1" seconds="$2"
  echo "$((now - seconds))" > "$home/.local/state/backup/$name"
}

# Everything a new shell prints while it sources bash.sh. PS1 is what
# makes bash.sh treat the shell as interactive.
new_shell_output () {
  local ps1="$1"
  env -i HOME="$home" PATH=/usr/bin:/bin TERM=dumb USER=jorgen \
      FAKE_NOW="$now" \
      bash --norc --noprofile -c "PS1='$ps1'; . '$repo/bash.sh'" 2>&1
}

new_interactive_shell_output () {
  new_shell_output '\$ '
}

assert_output () {
  local actual="$1" expected="$2"
  if [[ $actual != "$expected" ]]
  then
    fail "expected output:"$'\n'"$expected"$'\n'"got:"$'\n'"$actual"
  fi
}

backup_never="Warning: no backup has succeeded on this machine yet; see journalctl --user -t backup for why"
restore_never="Warning: no restore of the backup has been done on this machine; check that it restores with: backup restore <empty dir>"

start_test "a shell with a recent backup and restore prints nothing extra"
recorded_ago last-backup $((3 * day))
recorded_ago last-restore $((90 * day))
assert_output "$(new_interactive_shell_output)" ""
end_test

start_test "a shell warns once when there has never been a backup"
recorded_ago last-restore 0
assert_output "$(new_interactive_shell_output)" "$backup_never"
end_test

start_test "a shell warns when the last backup is more than 3 days old"
recorded_ago last-backup $((3 * day + 1))
recorded_ago last-restore 0
assert_output "$(new_interactive_shell_output)" \
  "Warning: the last successful backup was 3 days ago; see journalctl --user -t backup for why"
end_test

start_test "the backup warning counts whole days"
recorded_ago last-backup $((10 * day - 1))
recorded_ago last-restore 0
assert_output "$(new_interactive_shell_output)" \
  "Warning: the last successful backup was 9 days ago; see journalctl --user -t backup for why"
end_test

start_test "a shell warns once when there has never been a restore"
recorded_ago last-backup 0
assert_output "$(new_interactive_shell_output)" "$restore_never"
end_test

start_test "a shell warns when the last restore is more than 90 days old"
recorded_ago last-backup 0
recorded_ago last-restore $((90 * day + 1))
assert_output "$(new_interactive_shell_output)" \
  "Warning: the last restore of the backup was 90 days ago; check that it restores with: backup restore <empty dir>"
end_test

start_test "a shell with neither a backup nor a restore warns about both"
assert_output "$(new_interactive_shell_output)" \
  "$backup_never"$'\n'"$restore_never"
end_test

start_test "a backup time in the future is not a warning"
recorded_ago last-backup $((-day))
recorded_ago last-restore 0
assert_output "$(new_interactive_shell_output)" ""
end_test

start_test "a file that holds no time is named in the warning"
# shellcheck disable=SC2016 # the file must hold the literal text
echo 'x[$(touch "$HOME/evaluated")]' > "$home/.local/state/backup/last-backup"
: > "$home/.local/state/backup/last-restore"
assert_output "$(new_interactive_shell_output)" \
  "Warning: $home/.local/state/backup/last-backup does not hold a time; see journalctl --user -t backup for why"$'\n'"Warning: $home/.local/state/backup/last-restore does not hold a time; check that it restores with: backup restore <empty dir>"
if [[ -e $home/evaluated ]]
then
  fail "the shell ran code from last-backup"
fi
end_test

start_test "a shell that is not interactive prints no warning"
assert_output "$(new_shell_output '')" ""
end_test

if [[ $failures -gt 0 ]]
then
  exit 1
fi
