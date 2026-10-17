---
criteria:  CRITERIA.md
closes:    AC-8, AC-9, AC-10, AC-14
advances:
after:     01-check-command, 02-backup-and-restore
status:    ready
attempts:  0
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
