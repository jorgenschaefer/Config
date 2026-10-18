#!/usr/bin/env bash

# Tests of a backup run: `bin/backup` with no subcommand.

# shellcheck source=tests/backup_helpers.sh
source "$(dirname "$0")/backup_helpers.sh"

start_test "a run uploads the common paths to the machine's own object"
make_common_home
if ! "$backup" > /dev/null 2>&1
then
  fail "run failed: $(cat "$FAKE_JOURNAL")"
fi
members="$(archive_members otherhost/backup.tar.age)"
for member in Documents/letter.txt .ssh/id_ed25519 .aws/config \
              .local/share/keyrings/login.keyring \
              .claude/projects/one/memory/MEMORY.md \
              .claude/projects/two/memory/MEMORY.md
do
  assert_member "$members" "$member"
done
assert_no_member "$members" without-memory
end_test

start_test "a run uploads only to its own host's object and records the time"
make_common_home
before="$(date +%s)"
"$backup" > /dev/null 2>&1
after="$(date +%s)"
keys="$(cd "$FAKE_S3/jorgenschaefer-backup" && find . -type f | sort)"
if [[ $keys != "./otherhost/backup.tar.age/v1" ]]
then
  fail "expected one version of otherhost/backup.tar.age, bucket holds: $keys"
fi
recorded="$(cat "$HOME/.local/state/backup/last-backup" 2>/dev/null)"
if ! [[ $recorded =~ ^[0-9]+$ ]] || (( recorded < before || recorded > after ))
then
  fail "last-backup holds '$recorded', not a time between $before and $after"
fi
end_test

start_test "a run on njord adds its paths and everything under Documents"
make_njord_home
export FAKE_HOSTNAME=njord
if ! "$backup" > /dev/null 2>&1
then
  fail "run failed: $(cat "$FAKE_JOURNAL")"
fi
members="$(archive_members njord/backup.tar.age)"
for member in Documents/letter.txt Documents/Certificates/ca.pem \
              Documents/elster.pfx .ssh/id_ed25519 \
              .claude/projects/one/memory/MEMORY.md \
              .myinvoice/rechnungen/2026-001.pdf .myinvoice/myinvoice.db \
              Projects/haushaltsbuch/haushaltsbuch.db
do
  assert_member "$members" "$member"
done
assert_no_member "$members" 'haushaltsbuch.db.bak'
assert_no_member "$members" 'package.json'
end_test

start_test "a run on njord holds consistent database snapshots without -wal or -shm"
make_njord_home
export FAKE_HOSTNAME=njord
"$backup" > /dev/null 2>&1
assert_no_member "$(archive_members njord/backup.tar.age)" '\.db-\(wal\|shm\)$'
unpack_archive njord/backup.tar.age "$scratch/unpacked"
for db in .myinvoice/myinvoice.db Projects/haushaltsbuch/haushaltsbuch.db
do
  check="$(sqlite3 "$scratch/unpacked/$db" 'PRAGMA integrity_check')"
  if [[ $check != ok ]]
  then
    fail "integrity check of $db says: $check"
  fi
  if [[ $(sqlite3 "$scratch/unpacked/$db" "SELECT count(*) FROM t
                                           WHERE x = 'in-wal'") != 1 ]]
  then
    fail "$db lacks the change that was only in its -wal file"
  fi
done
end_test

start_test "a run fails and says so when a listed path is missing"
make_njord_home
export FAKE_HOSTNAME=njord
rm -r "$HOME/.myinvoice"
assert_backup_fails
assert_journal_mentions "path=.myinvoice"
end_test

start_test "a run fails and says so when a file can't be packed"
make_common_home
chmod 000 "$HOME/.ssh/id_ed25519"
assert_backup_fails
assert_journal_mentions "reason=pack_failed"
chmod 600 "$HOME/.ssh/id_ed25519"
end_test

start_test "a run fails and says so when a database snapshot fails its integrity check"
make_njord_home
export FAKE_HOSTNAME=njord
db="$HOME/Projects/haushaltsbuch/haushaltsbuch.db"
rm "$db" "$db-wal" "$db-shm"
sqlite3 "$db" "CREATE TABLE t (x);
               WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i + 1
                                       FROM n WHERE i < 2000)
               INSERT INTO t SELECT printf('%0100d', i) FROM n;"
printf 'garbage%.0s' {1..200} \
  | dd of="$db" bs=4096 seek=3 conv=notrunc 2>/dev/null
assert_backup_fails
assert_journal_mentions "reason=database_damaged"
assert_journal_mentions "database=Projects/haushaltsbuch/haushaltsbuch.db"
end_test

start_test "a run fails and says so when a database within a listed path is missing"
make_njord_home
export FAKE_HOSTNAME=njord
rm "$HOME"/.myinvoice/myinvoice.db*
assert_backup_fails
assert_journal_mentions "database=.myinvoice/myinvoice.db"
end_test

start_test "a run fails and says so when S3 doesn't confirm the upload"
make_common_home
export FAKE_AWS_FAIL=1
assert_backup_fails
assert_journal_mentions "reason=upload_failed"
assert_journal_mentions "key=otherhost/backup.tar.age"
end_test

start_test "a run without the backup profile fails and says to run backup setup"
make_common_home
export FAKE_AWS_PROFILES="default"
assert_backup_fails
assert_journal_mentions "reason=profile_missing"
if [[ $("$backup" 2>&1) != *"backup setup"* ]]
then
  fail "the run's error does not say to run backup setup"
fi
end_test

start_test "a run without the backup profile still reports a missing path"
make_common_home
export FAKE_AWS_PROFILES="default"
rm -r "$HOME/.ssh"
assert_backup_fails
assert_journal_mentions "path=.ssh"
end_test

start_test "a run fails and says so while the age public key is missing"
make_common_home
backup_with_public_key ""
assert_backup_fails
assert_journal_mentions "reason=public_key_missing"
end_test

start_test "a run fails and says so when it can't encrypt to the public key"
make_common_home
backup_with_public_key age1notakey
assert_backup_fails
assert_journal_mentions "reason=encrypt_failed"
end_test

start_test "the uploaded object can't be read without the private key"
make_common_home
"$backup" > /dev/null 2>&1
object="$FAKE_S3/jorgenschaefer-backup/otherhost/backup.tar.age/v1"
age-keygen -o "$scratch/keys/other-identity" 2>/dev/null
if age -d -i "$scratch/keys/other-identity" "$object" > /dev/null 2>&1
then
  fail "another age key decrypts the object"
fi
if tar -tzf "$object" > /dev/null 2>&1 || grep -q ssh-key "$object"
then
  fail "the object is readable without decrypting it"
fi
end_test

start_test "nothing in the repository holds an age private key"
if grep -rlIE --exclude-dir=.git 'AGE-SECRET-KEY-1[0-9A-Z]{20,}' "$repo"
then
  fail "the files above hold an age private key"
fi
end_test

start_test "a run succeeds when no Claude project has a memory directory"
make_common_home
rm -r "$HOME"/.claude/projects/*/memory
if ! "$backup" > /dev/null 2>&1
then
  fail "run failed: $(cat "$FAKE_JOURNAL")"
fi
assert_member "$(archive_members otherhost/backup.tar.age)" .ssh/id_ed25519
end_test

start_test "a run on njord succeeds when no Claude project has a memory directory"
make_njord_home
export FAKE_HOSTNAME=njord
rm -r "$HOME"/.claude/projects/*/memory
if ! "$backup" > /dev/null 2>&1
then
  fail "run failed: $(cat "$FAKE_JOURNAL")"
fi
assert_member "$(archive_members njord/backup.tar.age)" .myinvoice/myinvoice.db
end_test

finish
