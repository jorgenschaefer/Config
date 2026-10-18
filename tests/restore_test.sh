#!/usr/bin/env bash

# Tests of `bin/backup restore`.

# shellcheck source=tests/backup_helpers.sh
source "$(dirname "$0")/backup_helpers.sh"

# The private key as the user types it in: the identity file's one line
# that is not a comment.
private_key () {
  grep -v '^#' "$scratch/keys/identity"
}

# Run a restore with the private key typed in, and keep its exit
# status in $status and its output in $output.
restore () {
  output="$(private_key | "$backup" restore "$@" 2>&1)"
  status=$?
}

# Run a restore on a terminal that types nothing, and keep what the
# terminal shows in $terminal. The prompt for the key shows only on a
# terminal.
restore_on_terminal () {
  local command="'$backup' restore"
  local argument
  for argument in "$@"
  do
    command+=" '$argument'"
  done
  terminal="$(SHELL=/bin/bash timeout 10 script -qec "$command" /dev/null \
                < /dev/null 2>&1)"
}

start_test "a restore unpacks the current version into the directory and records the time"
make_common_home
"$backup" > /dev/null 2>&1
before="$(date +%s)"
restore "$scratch/restored"
after="$(date +%s)"
if [[ $status != 0 ]]
then
  fail "restore failed: $output"
fi
if [[ $(cat "$scratch/restored/.ssh/id_ed25519" 2>/dev/null) != ssh-key ]]
then
  fail "restored directory lacks .ssh/id_ed25519"
fi
if [[ $output != *"$scratch/restored"* ]]
then
  fail "restore does not say where the files are: $output"
fi
recorded="$(cat "$HOME/.local/state/backup/last-restore" 2>/dev/null)"
if ! [[ $recorded =~ ^[0-9]+$ ]] || (( recorded < before || recorded > after ))
then
  fail "last-restore holds '$recorded', not a time between $before and $after"
fi
end_test

start_test "a restore says how to revoke the key of the machine it replaces"
make_common_home
"$backup" > /dev/null 2>&1
restore "$scratch/restored"
if [[ $output != *"delete the backup profile from ~/.aws/credentials and run backup setup"* ]]
then
  fail "restore does not say to delete the backup profile and run backup setup: $output"
fi
end_test

start_test "a restore of another host's backup says how to revoke that host's key"
make_common_home
"$backup" > /dev/null 2>&1
export FAKE_HOSTNAME=newhost
restore "$scratch/restored" --host otherhost
if [[ $output != *"aws iam delete-access-key --user-name backup-otherhost"* ]]
then
  fail "restore does not say how to revoke backup-otherhost's key: $output"
fi
if [[ $output == *"run backup setup"* ]]
then
  fail "restore says backup setup revokes another host's key: $output"
fi
end_test

start_test "a restore fails and says so without XDG_RUNTIME_DIR"
make_common_home
"$backup" > /dev/null 2>&1
unset XDG_RUNTIME_DIR
restore "$scratch/restored"
if [[ $status == 0 ]]
then
  fail "restore succeeded"
fi
assert_journal_mentions "restore_failed reason=runtime_dir_missing"
restore_on_terminal "$scratch/restored-on-terminal"
if [[ $terminal == *"private key"* ]]
then
  fail "restore asked for the key before checking XDG_RUNTIME_DIR: $terminal"
fi
end_test

start_test "a restore refuses a directory that isn't empty"
make_common_home
"$backup" > /dev/null 2>&1
mkdir -p "$scratch/restored"
echo mine > "$scratch/restored/.hidden"
restore "$scratch/restored"
if [[ $status == 0 ]]
then
  fail "restore into a non-empty directory succeeded"
fi
if [[ -e $scratch/restored/.ssh ]]
then
  fail "restore unpacked into a non-empty directory"
fi
if [[ $output != *"$scratch/restored is not empty"* ]]
then
  fail "restore does not say why it refused: $output"
fi
if [[ -e $HOME/.local/state/backup/last-restore ]]
then
  fail "a refused restore was recorded"
fi
end_test

start_test "a restore with --host unpacks that host's object"
make_common_home
export FAKE_HOSTNAME=laptop
echo laptop > "$HOME/Documents/letter.txt"
"$backup" > /dev/null 2>&1
export FAKE_HOSTNAME=otherhost
echo otherhost > "$HOME/Documents/letter.txt"
"$backup" > /dev/null 2>&1
restore "$scratch/restored" --host laptop
if [[ $(cat "$scratch/restored/Documents/letter.txt" 2>/dev/null) != laptop ]]
then
  fail "restore did not unpack laptop's object: $output"
fi
end_test

start_test "a restore unpacks the given version, and the newest without one"
make_common_home
echo first > "$HOME/Documents/letter.txt"
"$backup" > /dev/null 2>&1
echo second > "$HOME/Documents/letter.txt"
"$backup" > /dev/null 2>&1
restore "$scratch/first" v1
if [[ $(cat "$scratch/first/Documents/letter.txt" 2>/dev/null) != first ]]
then
  fail "restore of version v1 did not unpack it: $output"
fi
restore "$scratch/current"
if [[ $(cat "$scratch/current/Documents/letter.txt" 2>/dev/null) != second ]]
then
  fail "restore without a version did not unpack the newest: $output"
fi
end_test

start_test "a restore reads with the default profile, or the one --profile names"
make_common_home
"$backup" > /dev/null 2>&1
export FAKE_AWS_PROFILES="admin"
restore "$scratch/default"
if [[ $status == 0 ]]
then
  fail "restore succeeded without a default profile"
fi
restore "$scratch/admin" --profile admin
if [[ ! -f $scratch/admin/.ssh/id_ed25519 ]]
then
  fail "restore with --profile admin did not unpack: $output"
fi
end_test

start_test "a restore with the wrong key fails and is not recorded"
make_common_home
"$backup" > /dev/null 2>&1
rm "$scratch/keys/identity"
age-keygen -o "$scratch/keys/identity" 2>/dev/null
restore "$scratch/restored"
if [[ $status == 0 ]]
then
  fail "restore with the wrong key succeeded: $output"
fi
if [[ -e $HOME/.local/state/backup/last-restore ]]
then
  fail "a failed restore was recorded"
fi
end_test

start_test "a restore that can't download fails and is not recorded"
make_common_home
"$backup" > /dev/null 2>&1
export FAKE_AWS_FAIL=1
restore "$scratch/restored"
if [[ $status == 0 ]]
then
  fail "restore without a download succeeded: $output"
fi
if [[ -e $HOME/.local/state/backup/last-restore ]]
then
  fail "a failed restore was recorded"
fi
end_test

start_test "a restore that finds no object for this machine suggests --host"
make_common_home
export FAKE_HOSTNAME=laptop
"$backup" > /dev/null 2>&1
export FAKE_HOSTNAME=debian
restore "$scratch/restored"
if [[ $status == 0 || $output != *--host* ]]
then
  fail "restore did not fail with a pointer to --host: $output"
fi
end_test

start_test "a restore leaves no copy of the private key on disk"
make_common_home
"$backup" > /dev/null 2>&1
restore "$scratch/restored"
if grep -rlF --exclude-dir=keys "$(private_key)" "$scratch"
then
  fail "the files above hold the private key"
fi
end_test

start_test "a restore asks for the private key on the terminal without echoing it"
make_common_home
"$backup" > /dev/null 2>&1
mkfifo "$scratch/typed"
SHELL=/bin/bash script -qec "'$backup' restore '$scratch/restored'" /dev/null \
  < "$scratch/typed" > "$scratch/terminal" 2>&1 &
exec 3> "$scratch/typed"
for _ in {1..50}
do
  grep -q 'private key' "$scratch/terminal" && break
  sleep 0.1
done
private_key >&3
exec 3>&-
wait
if ! grep -q 'private key' "$scratch/terminal"
then
  fail "restore did not ask for the private key: $(cat "$scratch/terminal")"
fi
if grep -qF "$(private_key)" "$scratch/terminal"
then
  fail "the terminal shows the private key: $(cat "$scratch/terminal")"
fi
if [[ ! -f $scratch/restored/.ssh/id_ed25519 ]]
then
  fail "restore from the terminal did not unpack: $(cat "$scratch/terminal")"
fi
end_test

start_test "a restore that can't create its directory says so"
make_common_home
"$backup" > /dev/null 2>&1
mkdir "$scratch/read-only"
chmod 555 "$scratch/read-only"
restore "$scratch/read-only/restored"
if [[ $status == 0 || $output != *"could not create $scratch/read-only/restored"* ]]
then
  fail "restore did not say it could not create the directory: $output"
fi
chmod 755 "$scratch/read-only"
end_test

start_test "a restore into a directory it can't write says so before asking for the key"
make_common_home
"$backup" > /dev/null 2>&1
mkdir "$scratch/read-only"
chmod 555 "$scratch/read-only"
restore "$scratch/read-only"
if [[ $status == 0 || $output != *"can't write into $scratch/read-only"* ]]
then
  fail "restore did not say it can't write into the directory: $output"
fi
restore_on_terminal "$scratch/read-only"
if [[ $terminal == *"private key"* ]]
then
  fail "restore asked for the key before checking the directory: $terminal"
fi
chmod 755 "$scratch/read-only"
end_test

start_test "a restore without a directory, or with too many arguments, shows the usage"
for arguments in "" "$scratch/a v1 v2" "$scratch/a --hots x"
do
  # shellcheck disable=SC2086 # split into arguments on purpose
  output="$("$backup" restore $arguments 2>&1 < /dev/null)"
  if [[ $? == 0 || $output != Usage:* ]]
  then
    fail "restore $arguments did not show the usage: $output"
  fi
done
end_test

finish
