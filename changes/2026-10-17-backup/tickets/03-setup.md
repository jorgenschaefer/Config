---
criteria:  CRITERIA.md
closes:    AC-8, AC-9, AC-10, AC-14
advances:
after:     01-check-command, 02-backup-and-restore
status:    done
attempts:  1
---

## Build
`backup setup` creates, or finds already in place, the bucket with versioning, retention rule, public access block and encryption, the machine's IAM user with its put-only policy, and the `backup` profile holding that user's key.

## Done when
> **AC-8** For each machine's object, an older version is deleted only once it has been superseded for at least 30 days and at least 90 newer versions exist. The current version is never deleted. With no uploads, nothing is ever deleted, and uploads made in a burst don't delete anything for 30 days.

> **AC-9** The credentials the timer uses on a machine can upload that machine's object and nothing else. They can't read, list or delete any version of any machine's object.

> **AC-10** `backup setup`, run with the admin profile, leaves behind: the bucket in eu-central-1 with versioning, the retention rule and public access blocked; the IAM user `backup-<hostname>` for the machine it runs on; and a `backup` profile in `~/.aws` holding that user's key. It prints what it created and what was already there. A second run changes nothing and doesn't fail. If `backup-<hostname>` already exists but there is no `backup` profile, setup deletes that user's existing access keys, creates a new one, and prints that the old ones were revoked.

> **AC-14** Two machines backing up to the same bucket never overwrite or expire each other's versions.

## Technical decisions
> **Stored formats** In S3: one object per machine, `<hostname>/backup.tar.age`, with the hostname from `hostname -s`. It is a gzipped tar with paths relative to `$HOME` (`haushaltsbuch.db` and `myinvoice.db` as consistent snapshots under their own names, without their `-wal` and `-shm` files), encrypted to one age recipient. The bucket is versioned, with a lifecycle rule `NoncurrentVersionExpiration` with `NewerNoncurrentVersions=90` and `NoncurrentDays=30`. On the laptop: `~/.local/state/backup/last-backup` and `~/.local/state/backup/last-restore` (timestamps), a `backup` profile in `~/.aws/credentials`, and links to the units in `~/.config/systemd/user/`. Nothing is migrated.

> **Wire formats** The commands `backup`, `backup setup [--profile P]` and `backup restore <dir> [--host H] [version-id] [--profile P]`, where `--profile` defaults to `default`; a non-zero exit on failure; the AWS profile name `backup`.

> **Trust boundaries** A daily outgoing upload of encrypted secrets to S3. Each machine's IAM user `backup-<hostname>` gets only `s3:PutObject` on that machine's key. The private key lives only in the password manager and is typed in at restore. Restore and setup use admin credentials that the user supplies. On a new machine, these come from an access key created through an AWS console sign-in. That sign-in and its MFA must work without this laptop; this is the user's to ensure, not the code's. The bucket has public access blocked and SSE-S3 encryption.

> **Dependencies** None new. `age`, `aws`, `sqlite3`, `tar` and systemd are already installed.

## Nudges
> One script, `bin/backup`, in bash like `bin/push-projects`, with the subcommands (no subcommand) / `setup` / `restore`. The path lists (the common one and one per hostname), bucket name, object key pattern and age public key are constants at its top, the one home for each.

> The age key pair is generated once with `age-keygen`, the secret goes straight into the password manager, and only the public key is written into `bin/backup`. `setup` prints these steps if the public key is missing.

> A comment in the setup code says why `backup-<hostname>` must not get read or delete rights, so nobody "fixes" it later to make automatic verification possible.

## Context
The design: each machine has its own IAM user, which can only put that machine's object. A lifecycle rule deletes an older version only once 90 newer ones exist and it has been superseded for 30 days, so neither a stopped timer nor a burst of junk uploads empties the bucket. Delete rights for the per-machine users were ruled out because anyone holding the laptop could wipe every version; retention by version count alone because 91 junk uploads from a stolen laptop would push every good version out within a day; one IAM user shared by all machines because IAM allows only two access keys per user and revoking a stolen laptop's key would cut off the others.

Ticket 02 builds `bin/backup` with the run and `restore`, and the constants at its top: the bucket name `jorgenschaefer-backup` in eu-central-1 and the object key pattern `<hostname>/backup.tar.age`. Setup uses those constants and adds none of its own for them. The run uploads with the AWS profile `backup` that setup writes; setup itself uses the profile given by `--profile`.

## Not here
- The run, `restore` and the constants are ticket 02.
- Enabling the timer belongs to `initialize.sh` in ticket 04, not to setup.

## Left standing
- **Real AWS credentials exposed during review.** In review round two, the reviewer's probe script had a wrong path, so `$HOME` stayed the real one. It printed the real `~/.aws/credentials` into the reviewing agent's tool output, including the access key and secret of the `[default]` profile. No AWS call was made, but the key is now in a local session transcript. Whether to rotate it is the user's decision.
- **Review findings not fixed:**
  - Nit, round one and round two: `aws configure set aws_secret_access_key "$secret"` puts the new secret in the process arguments for the moment the command runs, where another local account could read it from `/proc`. `aws configure` has no other way to take it. Writing `~/.aws/credentials` by hand would avoid this, at the cost of an INI editor in bash. On a single-user laptop that did not seem worth it.
  - Nit, round two: not every setup failure message names the AWS profile. The list and create steps name it, but the bucket-setting, policy and key steps do not. I renamed the test that claimed they all did, rather than changing the messages.
  - Nit, round two: `setup --profile` without a value stops through bash's `${2:?…}`, not through `fail`, so it does not reach the journal. `restore` handles its options the same way, and I kept setup consistent with it.
- **Checks not run: real AWS.** Setup has never run against AWS. Every S3 and IAM call in the tests goes to a fake (`fake_aws_admin` in `tests/setup_test.sh`). The fake answers each `--query` only in the exact form setup sends it. `aws configure` is the real CLI, writing into a scratch `$HOME`. The reviewer read every call and query against the real CLI's behaviour and found nothing that would differ. Two edge cases are known and not handled. With more than 100 IAM users, the user lookup can miss, because the CLI applies the query page by page. Right after a create call, AWS can lag briefly before the next call sees the new bucket or user; running setup again fixes that. The first real `backup setup` with the admin profile is the first time any of this meets AWS.
- **AC-8 and AC-14 (retention):** the test pins the exact lifecycle rule that setup sends. It is a single rule, covers every object, has only `NoncurrentVersionExpiration` with `NoncurrentDays=30` and `NewerNoncurrentVersions=90`, and has no `Expiration`. That S3 applies it per object, and never to the current version, is S3's behaviour, and no test here can prove it. S3 keeps 90 noncurrent versions plus the current one, so a version expires only once 91 newer versions exist, which still meets "at least 90".
- **AC-9 and AC-14 (permissions):** the tests prove that each machine's user ends up with exactly one policy statement: `s3:PutObject` on its own `<hostname>/backup.tar.age`. That IAM enforces this is AWS's behaviour. Setup does not look for policies or groups someone attached to `backup-<hostname>` by hand. If any exist, AC-9 does not hold for that user.
- **Decisions that would be costly to reverse:**
  - **Names in AWS:** the IAM user is `backup-<hostname>`, its inline policy is `put-own-backup`, and the lifecycle rule ID is `keep-90-versions-for-30-days`.
  - **Bucket settings are put on every run.** Putting the same settings again changes nothing, so a second run still leaves everything as it was. It also means setup does not report each setting as created or already there: it prints one line saying the settings are in place. `put-bucket-lifecycle-configuration` replaces every lifecycle rule on the bucket, so setup removes any rule someone adds by hand.
  - **A `backup` profile holding a key that is not one of the user's keys counts as missing**, for example a `~/.aws` restored from another machine. This goes beyond AC-10's "no profile" case. Setup then revokes the user's keys, creates a new one and writes it into the profile. A profile that holds the user's key id with a wrong secret is not detected. Setup writes the secret before the id, so a profile left half-written by a failure is replaced on the next run.
  - **Journal reasons**, under the event `setup_failed`: `bucket_unreadable`, `bucket_create_failed`, `bucket_settings_failed` (with `setting=`), `user_unreadable`, `user_create_failed`, `policy_failed`, `key_list_failed`, `key_revoke_failed`, `key_create_failed`, `profile_failed`.
  - **Test tooling:** the setup tests use `jq` to read the JSON that setup sends. Like `openssl` for the run's tests, `jq` is needed only by the tests; `bin/backup` does not use it.
- **Ticket 01's open item** (a structural rule against tests in `bin/`) is outside this ticket's Done when and is still open.

