#!/usr/bin/env bash

# Tests of `bin/backup setup`.

# shellcheck source=tests/backup_helpers.sh
source "$(dirname "$0")/backup_helpers.sh"

# The aws command as setup reaches it, in place of the one from
# backup_helpers.sh. `aws configure` is the real one writing into the
# scratch $HOME, and the rest goes to fake_aws_admin.
# $FAKE_AWS_FAIL_ON names one command that fails, as "<service>
# <command>", or with its first argument as well.
aws () {
  if [[ "$1 ${2:-}" == "$FAKE_AWS_FAIL_ON" || "$1 ${2:-} ${3:-}" == "$FAKE_AWS_FAIL_ON" ]]
  then
    echo "An error occurred (InternalError) when calling $1 $2" >&2
    return 254
  fi
  case "$1" in
    configure) command aws "$@" ;;
    *) fake_aws_admin "$@" ;;
  esac
}

# The S3 and IAM calls setup makes, reached with the profiles in
# $FAKE_AWS_PROFILES and no default region. A bucket is a directory in
# $FAKE_S3, and the last value put for each of its settings is a file
# in $FAKE_S3_SETTINGS/<bucket>/<command>. An IAM user is a directory
# in $FAKE_IAM with keys/<access-key-id> holding the secret and
# policies/<name> holding the document. A query is answered only in the
# form setup asks it, so a changed query fails rather than goes unseen.
fake_aws_admin () {
  local command="$1 $2"
  local -A option=()
  shift 2
  while [[ $# -gt 0 ]]
  do
    if [[ $1 != --* || $# -lt 2 ]]
    then
      echo "fake aws: unexpected argument $1" >&2
      return 252
    fi
    option[${1#--}]="$2"
    shift 2
  done
  if [[ " $FAKE_AWS_PROFILES " != *" ${option[profile]:-default} "* ]]
  then
    echo "The config profile (${option[profile]:-default}) could not be found" >&2
    return 255
  fi
  if [[ ${option[region]:-} != "eu-central-1" ]]
  then
    echo "You must specify a region." >&2
    return 255
  fi
  local bucket="${option[bucket]:-}" user="${option[user-name]:-}"
  local query="${option[query]:-}"
  case "$command" in
    "s3api list-buckets")
      [[ $query =~ ^Buckets\[\?Name==\'([^\']+)\'\]\.Name$ ]] \
        || { echo "fake aws: unknown query $query" >&2; return 252; }
      if [[ -d $FAKE_S3/${BASH_REMATCH[1]} ]]
      then
        echo "${BASH_REMATCH[1]}"
      fi
      ;;
    "s3api create-bucket")
      if [[ ${option[create-bucket-configuration]:-} != "LocationConstraint=eu-central-1" ]]
      then
        echo "IllegalLocationConstraintException" >&2
        return 254
      fi
      if [[ -d $FAKE_S3/$bucket ]]
      then
        echo "BucketAlreadyOwnedByYou: $bucket" >&2
        return 254
      fi
      mkdir -p "$FAKE_S3/$bucket" "$FAKE_S3_SETTINGS/$bucket"
      echo "eu-central-1" > "$FAKE_S3_SETTINGS/$bucket/region"
      ;;
    "s3api put-bucket-versioning")
      put_bucket_setting "$bucket" versioning "${option[versioning-configuration]:-}" ;;
    "s3api put-bucket-lifecycle-configuration")
      put_bucket_setting "$bucket" lifecycle "${option[lifecycle-configuration]:-}" ;;
    "s3api put-public-access-block")
      put_bucket_setting "$bucket" public-access-block \
                         "${option[public-access-block-configuration]:-}" ;;
    "s3api put-bucket-encryption")
      put_bucket_setting "$bucket" encryption \
                         "${option[server-side-encryption-configuration]:-}" ;;
    "iam list-users")
      [[ $query =~ ^Users\[\?UserName==\'([^\']+)\'\]\.UserName$ ]] \
        || { echo "fake aws: unknown query $query" >&2; return 252; }
      if [[ -d $FAKE_IAM/${BASH_REMATCH[1]} ]]
      then
        echo "${BASH_REMATCH[1]}"
      fi
      ;;
    "iam create-user")
      if [[ -d $FAKE_IAM/$user ]]
      then
        echo "EntityAlreadyExists: User with name $user already exists." >&2
        return 254
      fi
      mkdir -p "$FAKE_IAM/$user/keys" "$FAKE_IAM/$user/policies"
      ;;
    "iam put-user-policy")
      iam_user_exists "$user" || return 254
      echo "${option[policy-document]}" \
        > "$FAKE_IAM/$user/policies/${option[policy-name]}"
      ;;
    "iam list-access-keys")
      iam_user_exists "$user" || return 254
      [[ $query == "AccessKeyMetadata[].AccessKeyId" ]] \
        || { echo "fake aws: unknown query $query" >&2; return 252; }
      find "$FAKE_IAM/$user/keys" -type f -printf '%f\n' | sort | paste -sd '\t'
      ;;
    "iam delete-access-key")
      iam_user_exists "$user" || return 254
      rm "$FAKE_IAM/$user/keys/${option[access-key-id]}" || return 254
      ;;
    "iam create-access-key")
      iam_user_exists "$user" || return 254
      [[ $query == "AccessKey.[AccessKeyId,SecretAccessKey]" ]] \
        || { echo "fake aws: unknown query $query" >&2; return 252; }
      if [[ $(find "$FAKE_IAM/$user/keys" -type f | wc -l) -ge 2 ]]
      then
        echo "LimitExceeded: Cannot exceed quota for AccessKeysPerUser: 2" >&2
        return 254
      fi
      local id secret
      id="AKIAFAKE$(find "$FAKE_IAM" -path '*/keys/*' | wc -l)$RANDOM"
      secret="secret-$RANDOM$RANDOM"
      echo "$secret" > "$FAKE_IAM/$user/keys/$id"
      printf '%s\t%s\n' "$id" "$secret"
      ;;
    *)
      echo "fake aws: unknown command $command" >&2
      return 252
      ;;
  esac
}

put_bucket_setting () {
  local bucket="$1" setting="$2" value="$3"
  if [[ ! -d $FAKE_S3/$bucket ]]
  then
    echo "NoSuchBucket: $bucket" >&2
    return 254
  fi
  echo "$value" > "$FAKE_S3_SETTINGS/$bucket/$setting"
}

iam_user_exists () {
  local user="$1"
  if [[ ! -d $FAKE_IAM/$user ]]
  then
    echo "NoSuchEntity: The user with name $user cannot be found." >&2
    return 1
  fi
}

export -f aws fake_aws_admin put_bucket_setting iam_user_exists

# Start a setup test on an AWS account with neither the bucket nor any
# IAM user, reached with the admin profile `default`.
start_setup_test () {
  start_test "$1"
  unset AWS_PROFILE AWS_CONFIG_FILE AWS_SHARED_CREDENTIALS_FILE
  export FAKE_AWS_PROFILES="default"
  export FAKE_S3_SETTINGS="$scratch/s3-settings"
  export FAKE_IAM="$scratch/iam"
  export FAKE_AWS_FAIL_ON=""
  rm -rf "$FAKE_S3/jorgenschaefer-backup"
  mkdir -p "$FAKE_S3_SETTINGS" "$FAKE_IAM"
}

run_setup () {
  if ! output="$("$backup" setup "$@" 2>&1)"
  then
    fail "setup failed: $output"
  fi
}

assert_output_mentions () {
  local text="$1"
  if ! grep -qF -- "$text" <<< "$output"
  then
    fail "output does not mention '$text'; it is: $output"
  fi
}

start_setup_test "setup creates the bucket in eu-central-1"
run_setup
if [[ $(cat "$FAKE_S3_SETTINGS/jorgenschaefer-backup/region" 2>/dev/null) != eu-central-1 ]]
then
  fail "no bucket jorgenschaefer-backup in eu-central-1"
fi
assert_output_mentions "Created bucket jorgenschaefer-backup in eu-central-1"
end_test

start_setup_test "setup turns on versioning and the retention rule"
run_setup
settings="$FAKE_S3_SETTINGS/jorgenschaefer-backup"
if [[ $(cat "$settings/versioning" 2>/dev/null) != "Status=Enabled" ]]
then
  fail "versioning is not enabled: $(cat "$settings/versioning" 2>/dev/null)"
fi
# The one rule covers every object, and expires only noncurrent
# versions: superseded for 30 days, with 90 newer noncurrent ones.
rule="$(jq -c '.Rules | if length == 1 then .[0] else error("not one rule") end
               | del(.ID)' "$settings/lifecycle" 2>&1)"
expected='{"Status":"Enabled","Filter":{"Prefix":""},"NoncurrentVersionExpiration":{"NoncurrentDays":30,"NewerNoncurrentVersions":90}}'
if [[ $(jq -cS . <<< "$rule" 2>&1) != "$(jq -cS . <<< "$expected")" ]]
then
  fail "the lifecycle rule is $rule, expected $expected"
fi
assert_output_mentions "versioning"
assert_output_mentions "NewerNoncurrentVersions=90"
end_test

start_setup_test "setup blocks public access and encrypts with SSE-S3"
run_setup
settings="$FAKE_S3_SETTINGS/jorgenschaefer-backup"
blocked="$(tr ',' '\n' < "$settings/public-access-block" 2>/dev/null | sort | paste -sd ,)"
if [[ $blocked != "BlockPublicAcls=true,BlockPublicPolicy=true,IgnorePublicAcls=true,RestrictPublicBuckets=true" ]]
then
  fail "public access is not fully blocked: $blocked"
fi
algorithms="$(jq -c '[.Rules[].ApplyServerSideEncryptionByDefault.SSEAlgorithm]' \
                 "$settings/encryption" 2>&1)"
if [[ $algorithms != '["AES256"]' ]]
then
  fail "the bucket is not encrypted with SSE-S3 alone: $algorithms"
fi
assert_output_mentions "public access blocked"
assert_output_mentions "SSE-S3"
end_test

# The policies of an IAM user, merged into one sorted list of
# statements.
user_statements () {
  local user="$1"
  find "$FAKE_IAM/$user/policies" -type f -exec cat {} + 2>/dev/null \
    | jq -cS -s '[.[].Statement | if type == "array" then .[] else . end]
                 | sort'
}

start_setup_test "the machine's IAM user may only put the machine's object"
export FAKE_HOSTNAME=laptop
run_setup
statements="$(user_statements backup-laptop)"
expected='[{"Action":"s3:PutObject","Effect":"Allow","Resource":"arn:aws:s3:::jorgenschaefer-backup/laptop/backup.tar.age"}]'
if [[ $statements != "$expected" ]]
then
  fail "backup-laptop may do $statements, expected $expected"
fi
assert_output_mentions "Created IAM user backup-laptop"
end_test

# Fail unless the backup profile in the scratch $HOME holds one of the
# user's access keys, with its secret.
assert_profile_holds_key_of () {
  local user="$1" id secret
  id="$(command aws configure get aws_access_key_id --profile backup 2>&1)"
  secret="$(command aws configure get aws_secret_access_key --profile backup 2>&1)"
  if [[ ! -f $FAKE_IAM/$user/keys/$id ]]
  then
    fail "the backup profile holds key '$id', which is not one of $user's"
  elif [[ $secret != "$(cat "$FAKE_IAM/$user/keys/$id")" ]]
  then
    fail "the backup profile holds the wrong secret for $id"
  fi
}

start_setup_test "setup writes the user's key into the backup profile"
export FAKE_HOSTNAME=laptop
run_setup
assert_profile_holds_key_of backup-laptop
assert_output_mentions "Created profile backup"
end_test

# Every file of the fake AWS account and of ~/.aws, with its checksum.
account_state () {
  (cd "$scratch" && find s3 s3-settings iam home/.aws -type f -print0 \
                      | sort -z | xargs -0 md5sum)
}

start_setup_test "a second setup changes nothing and says what was there"
export FAKE_HOSTNAME=laptop
run_setup
before="$(account_state)"
run_setup
if [[ $(account_state) != "$before" ]]
then
  fail "second setup changed: $(diff <(echo "$before") <(account_state))"
fi
assert_output_mentions "Bucket jorgenschaefer-backup was already there"
assert_output_mentions "IAM user backup-laptop was already there"
assert_output_mentions "Profile backup already holds access key"
end_test

start_setup_test "setup without a backup profile revokes the user's old keys"
export FAKE_HOSTNAME=laptop
mkdir -p "$FAKE_IAM/backup-laptop/keys" "$FAKE_IAM/backup-laptop/policies"
echo old-secret-1 > "$FAKE_IAM/backup-laptop/keys/AKIAOLD1"
echo old-secret-2 > "$FAKE_IAM/backup-laptop/keys/AKIAOLD2"
run_setup
assert_profile_holds_key_of backup-laptop
keys="$(ls "$FAKE_IAM/backup-laptop/keys")"
if [[ $keys == *AKIAOLD* || $(wc -l <<< "$keys") != 1 ]]
then
  fail "backup-laptop has the keys $keys, expected only the new one"
fi
assert_output_mentions "Revoked the old access keys of IAM user backup-laptop: AKIAOLD1 AKIAOLD2"
end_test

start_setup_test "setup replaces a backup profile holding another machine's key"
export FAKE_HOSTNAME=oldhost
run_setup
export FAKE_HOSTNAME=laptop
run_setup
assert_profile_holds_key_of backup-laptop
if [[ -z $(ls "$FAKE_IAM/backup-oldhost/keys") ]]
then
  fail "setup on laptop revoked the key of backup-oldhost"
fi
assert_output_mentions "Created profile backup"
end_test

start_setup_test "machines sharing the bucket may each put only their own object"
export FAKE_HOSTNAME=laptop
run_setup
export HOME="$scratch/njord-home" FAKE_HOSTNAME=njord
mkdir -p "$HOME"
run_setup
assert_output_mentions "Bucket jorgenschaefer-backup was already there"
for host in laptop njord
do
  statements="$(user_statements "backup-$host")"
  expected='[{"Action":"s3:PutObject","Effect":"Allow","Resource":"arn:aws:s3:::jorgenschaefer-backup/'"$host"'/backup.tar.age"}]'
  if [[ $statements != "$expected" ]]
  then
    fail "backup-$host may do $statements, expected $expected"
  fi
done
assert_profile_holds_key_of backup-njord
HOME="$scratch/home" assert_profile_holds_key_of backup-laptop
end_test

start_setup_test "setup uses the admin credentials of the profile given"
export FAKE_HOSTNAME=laptop FAKE_AWS_PROFILES=admin
run_setup --profile admin
assert_profile_holds_key_of backup-laptop
end_test

# Each step AWS can refuse, the reason setup logs for it, and the state
# it is reached from: a fresh account, or one where the user is there
# with a key but the backup profile is not.
while read -r service command reason account
do
  step="$service $command"
  start_setup_test "setup fails loudly when AWS refuses $step"
  export FAKE_HOSTNAME=laptop
  if [[ $account == user-with-key ]]
  then
    mkdir -p "$FAKE_IAM/backup-laptop/keys" "$FAKE_IAM/backup-laptop/policies"
    echo old-secret > "$FAKE_IAM/backup-laptop/keys/AKIAOLD1"
  fi
  export FAKE_AWS_FAIL_ON="$step"
  if output="$("$backup" setup 2>&1 < /dev/null)"
  then
    fail "setup succeeded: $output"
  fi
  assert_journal_mentions "setup_failed reason=$reason"
  if command aws configure get aws_access_key_id --profile backup > /dev/null 2>&1
  then
    fail "a failed setup wrote the backup profile"
  fi
  end_test
done <<'STEPS'
s3api list-buckets bucket_unreadable fresh
s3api create-bucket bucket_create_failed fresh
s3api put-bucket-versioning bucket_settings_failed fresh
s3api put-bucket-lifecycle-configuration bucket_settings_failed fresh
s3api put-public-access-block bucket_settings_failed fresh
s3api put-bucket-encryption bucket_settings_failed fresh
iam list-users user_unreadable fresh
iam create-user user_create_failed fresh
iam put-user-policy policy_failed fresh
iam list-access-keys key_list_failed fresh
iam delete-access-key key_revoke_failed user-with-key
iam create-access-key key_create_failed fresh
configure set profile_failed fresh
STEPS

for setting in aws_access_key_id aws_secret_access_key
do
  start_setup_test "setup run again after failing to write $setting completes the profile"
  export FAKE_HOSTNAME=laptop FAKE_AWS_FAIL_ON="configure set $setting"
  "$backup" setup > /dev/null 2>&1
  export FAKE_AWS_FAIL_ON=""
  run_setup
  assert_profile_holds_key_of backup-laptop
  end_test
done

start_setup_test "a failed step says what failed"
export FAKE_HOSTNAME=laptop FAKE_AWS_FAIL_ON="iam create-user"
"$backup" setup > /dev/null 2> "$scratch/stderr"
output="$(cat "$scratch/stderr")"
assert_output_mentions "could not create IAM user backup-laptop with AWS profile default"
end_test

start_setup_test "setup says how to make the age key while bin/backup has none"
backup_with_public_key ""
run_setup
assert_output_mentions "age-keygen"
assert_output_mentions "AGE_PUBLIC_KEY"
backup_with_public_key "$(age-keygen -y "$scratch/keys/identity")"
run_setup
if grep -qF age-keygen <<< "$output"
then
  fail "setup asks for an age key that bin/backup already has: $output"
fi
end_test

start_setup_test "setup rejects an argument it doesn't know"
"$backup" setup --host laptop > /dev/null 2>&1
if [[ $? != 2 ]]
then
  fail "setup --host did not end with the usage error"
fi
end_test

finish
