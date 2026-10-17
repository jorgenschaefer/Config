# shellcheck shell=bash

# Shared setup for the tests of bin/backup, sourced by
# backup_test.sh, restore_test.sh and setup_test.sh.
#
# Each test runs bin/backup against a scratch $HOME, with fakes for the
# commands that reach outside the machine: `aws` keeps a versioned
# bucket in a directory, `logger` writes the user journal to a file and
# `hostname` answers with $FAKE_HOSTNAME. age, sqlite3 and tar are the
# real ones.

set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
failures=0

# Start a test: a fresh scratch directory with an empty $HOME, a
# bucket, a journal, a test age key pair and a copy of bin/backup that
# encrypts to the test key.
start_test () {
  current_test="$1"
  scratch="$(mktemp -d)"
  export HOME="$scratch/home"
  export XDG_RUNTIME_DIR="$scratch/run"
  export FAKE_S3="$scratch/s3"
  export FAKE_JOURNAL="$scratch/journal"
  export FAKE_HOSTNAME="otherhost"
  export FAKE_AWS_PROFILES="backup default"
  export FAKE_AWS_FAIL=""
  mkdir -p "$HOME" "$XDG_RUNTIME_DIR" "$FAKE_S3/jorgenschaefer-backup" \
           "$scratch/keys"
  touch "$FAKE_JOURNAL"
  age-keygen -o "$scratch/keys/identity" 2>/dev/null
  backup="$scratch/backup"
  backup_with_public_key "$(age-keygen -y "$scratch/keys/identity")"
}

# Write $backup as a copy of bin/backup that encrypts to the given
# public key, as if it were the constant at the top of the script.
backup_with_public_key () {
  local key="$1"
  if ! grep -q "^AGE_PUBLIC_KEY=''$" "$repo/bin/backup"
  then
    fail "bin/backup has no line AGE_PUBLIC_KEY='' to put the test key in"
  fi
  sed "s/^AGE_PUBLIC_KEY=''$/AGE_PUBLIC_KEY='$key'/" "$repo/bin/backup" \
      > "$backup"
  chmod +x "$backup"
}

end_test () {
  rm -rf "$scratch"
}

fail () {
  echo "FAIL: $current_test: $*" >&2
  failures=$((failures + 1))
}

finish () {
  if [[ $failures -gt 0 ]]
  then
    exit 1
  fi
}

# The paths every machine backs up, each with one file in it.
make_common_home () {
  mkdir -p "$HOME/Documents" "$HOME/.ssh" "$HOME/.aws" \
           "$HOME/.local/share/keyrings" \
           "$HOME/.claude/projects/one/memory" \
           "$HOME/.claude/projects/two/memory" \
           "$HOME/.claude/projects/without-memory"
  echo document > "$HOME/Documents/letter.txt"
  echo ssh-key > "$HOME/.ssh/id_ed25519"
  echo aws-config > "$HOME/.aws/config"
  echo keyring > "$HOME/.local/share/keyrings/login.keyring"
  echo memory-one > "$HOME/.claude/projects/one/memory/MEMORY.md"
  echo memory-two > "$HOME/.claude/projects/two/memory/MEMORY.md"
  echo transcript > "$HOME/.claude/projects/without-memory/session.jsonl"
}

# What njord backs up beyond the common paths, plus files beside them
# that are not backed up. Both databases are in WAL mode and hold a row
# 'in-wal' that is only in their -wal file.
make_njord_home () {
  make_common_home
  mkdir -p "$HOME/Documents/Certificates" "$HOME/.myinvoice/rechnungen" \
           "$HOME/Projects/haushaltsbuch"
  echo certificate > "$HOME/Documents/Certificates/ca.pem"
  echo pfx > "$HOME/Documents/elster.pfx"
  echo invoice > "$HOME/.myinvoice/rechnungen/2026-001.pdf"
  echo backup > "$HOME/Projects/haushaltsbuch/haushaltsbuch.db.bak"
  echo source > "$HOME/Projects/haushaltsbuch/package.json"
  make_database_with_wal_changes "$HOME/.myinvoice/myinvoice.db"
  make_database_with_wal_changes \
    "$HOME/Projects/haushaltsbuch/haushaltsbuch.db"
}

make_database_with_wal_changes () {
  local db="$1"
  sqlite3 "$db" "PRAGMA journal_mode=WAL; CREATE TABLE t (x);
                 INSERT INTO t VALUES ('checkpointed');" > /dev/null
  sqlite3 "$db" '.dbconfig no_ckpt_on_close on' \
                "INSERT INTO t VALUES ('in-wal');" > /dev/null
  if [[ ! -s $db-wal ]]
  then
    fail "could not leave changes in $db-wal"
  fi
}

# Run the backup, expecting it to fail: a non-zero exit, nothing
# uploaded and the last successful backup left as it was.
assert_backup_fails () {
  mkdir -p "$HOME/.local/state/backup"
  echo 1000 > "$HOME/.local/state/backup/last-backup"
  if "$backup" > /dev/null 2>&1
  then
    fail "run succeeded"
  fi
  if [[ -n $(find "$FAKE_S3/jorgenschaefer-backup" -type f) ]]
  then
    fail "run uploaded $(find "$FAKE_S3/jorgenschaefer-backup" -type f)"
  fi
  if [[ $(cat "$HOME/.local/state/backup/last-backup") != 1000 ]]
  then
    fail "failed run counted as the last successful backup"
  fi
}

# The versions stored for a key in the fake bucket, oldest first.
stored_versions () {
  local key="$1"
  local dir="$FAKE_S3/jorgenschaefer-backup/$key"
  [[ -d $dir ]] || return 0
  find "$dir" -type f -printf '%f\n' | sort -V
}

# Decrypt the current version of a key with the test identity and list
# the archive's members.
archive_members () {
  local key="$1" version
  version="$(stored_versions "$key" | tail -n 1)"
  age -d -i "$scratch/keys/identity" \
      "$FAKE_S3/jorgenschaefer-backup/$key/$version" | tar -tzf -
}

# Decrypt the current version of a key into a directory.
unpack_archive () {
  local key="$1" dir="$2" version
  version="$(stored_versions "$key" | tail -n 1)"
  mkdir -p "$dir"
  age -d -i "$scratch/keys/identity" \
      "$FAKE_S3/jorgenschaefer-backup/$key/$version" | tar -xzf - -C "$dir"
}

assert_member () {
  local members="$1" member="$2"
  if ! grep -qxF -- "$member" <<< "$members"
  then
    fail "archive has no $member; it has: $(tr '\n' ' ' <<< "$members")"
  fi
}

assert_no_member () {
  local members="$1" pattern="$2"
  if grep -q -- "$pattern" <<< "$members"
  then
    fail "archive has $(grep -- "$pattern" <<< "$members" | tr '\n' ' ')"
  fi
}

assert_journal_mentions () {
  local text="$1"
  if ! grep -qF -- "$text" "$FAKE_JOURNAL"
  then
    fail "journal does not mention '$text'; it holds: $(cat "$FAKE_JOURNAL")"
  fi
}

hostname () {
  if [[ $* != "-s" ]]
  then
    echo "fake hostname called with '$*', not -s" >&2
    return 1
  fi
  echo "$FAKE_HOSTNAME"
}

logger () {
  echo "$*" >> "$FAKE_JOURNAL"
}

# A versioned S3 bucket in $FAKE_S3/<bucket>/<key>/<version-id>, as
# reached by a machine with the profiles in $FAKE_AWS_PROFILES and no
# default region. Like S3, put-object rejects a body whose MD5 differs
# from --content-md5; unlike S3 it also rejects a put without one, so a
# test notices if the check is dropped.
aws () {
  local command="$1 $2" bucket="" key="" body="" md5="" version=""
  local profile="" region="" query="" outfile=""
  shift 2
  while [[ $# -gt 0 ]]
  do
    case "$1" in
      --bucket) bucket="$2"; shift 2 ;;
      --key) key="$2"; shift 2 ;;
      --body) body="$2"; shift 2 ;;
      --content-md5) md5="$2"; shift 2 ;;
      --version-id) version="$2"; shift 2 ;;
      --profile) profile="$2"; shift 2 ;;
      --region) region="$2"; shift 2 ;;
      --query) query="$2"; shift 2 ;;
      --output) shift 2 ;;
      -*) echo "fake aws: unknown option $1" >&2; return 252 ;;
      *) outfile="$1"; shift ;;
    esac
  done
  if [[ -n $FAKE_AWS_FAIL ]]
  then
    echo "Could not connect to the endpoint URL" >&2
    return 255
  fi
  if [[ " $FAKE_AWS_PROFILES " != *" ${profile:-default} "* ]]
  then
    echo "The config profile (${profile:-default}) could not be found" >&2
    return 255
  fi
  if [[ $region != "eu-central-1" ]]
  then
    echo "You must specify a region." >&2
    return 255
  fi
  if [[ ! -d $FAKE_S3/$bucket ]]
  then
    echo "NoSuchBucket: $bucket" >&2
    return 254
  fi
  local dir="$FAKE_S3/$bucket/$key"
  case "$command" in
    "s3api put-object")
      if [[ ! -f $body ]]
      then
        echo "Error parsing parameter '--body': Blob values must be a path to a file." >&2
        return 252
      fi
      if [[ $md5 != "$(openssl md5 -binary "$body" | base64)" ]]
      then
        echo "BadDigest: the Content-MD5 you specified did not match" >&2
        return 254
      fi
      mkdir -p "$dir"
      version="v$(( $(find "$dir" -type f | wc -l) + 1 ))"
      cp "$body" "$dir/$version"
      ;;
    "s3api get-object")
      if [[ -z $version ]]
      then
        version="$(find "$dir" -type f -printf '%f\n' 2>/dev/null \
                     | sort -V | tail -n 1)"
      fi
      if [[ -z $version || ! -f $dir/$version ]]
      then
        echo "NoSuchKey: $key ${version}" >&2
        return 254
      fi
      cp "$dir/$version" "$outfile"
      ;;
    *)
      echo "fake aws: unknown command $command" >&2
      return 252
      ;;
  esac
  if [[ $query == "VersionId" ]]
  then
    echo "$version"
  else
    echo "{\"VersionId\": \"$version\"}"
  fi
}

export -f hostname logger aws
