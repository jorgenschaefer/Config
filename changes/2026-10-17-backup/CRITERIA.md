# Criteria: Back up specified paths off each laptop

## Problem
Some data exists only on this laptop: the Android release keystore for dom, the invoice database, the household database, the SSH and AWS keys and keyrings, `~/Documents` and the Claude memories. If the laptop is lost or stolen, or its disk dies, that data is gone for good. Losing the keystore means the dom app can never again be updated under its signature. What should be true instead: a recent copy of this data exists away from the laptop, only its owner can read it, and it can be restored onto a new machine from that copy.

Nothing has been lost yet. The survey of 2026-10-15 (`changes/backlog/backup.md`) found that all of it exists only here. A related instance of silent failure: `maintenance.py` was deleted in commit `5515f73` on 2026-04-19, and the crontab has been calling it every hour since, failing without anyone noticing.

## Acceptance criteria
- **AC-1** One successful `backup` run puts the paths listed for its machine into that machine's S3 object. On every machine that is `~/Documents`, `~/.ssh`, `~/.aws`, `~/.local/share/keyrings` and every `~/.claude/projects/*/memory/`. On `njord` it is also `~/.myinvoice` and `~/Projects/haushaltsbuch/haushaltsbuch.db`, and its `~/Documents` includes `Certificates/` and the `.pfx` files.
- **AC-2** The restored `haushaltsbuch.db` and `myinvoice.db` pass `PRAGMA integrity_check` and contain changes that were still only in the `-wal` file when the backup ran. The backup holds no `-wal` or `-shm` file for either database.
- **AC-3** The uploaded object can't be read without the age private key. No file on any backed-up machine and nothing in the repo contains that private key.
- **AC-4** The backup runs once a day without the user doing anything. A day missed because the laptop was off is caught up after the next login.
- **AC-5** A run that fails, e.g. because the laptop is offline, is retried at least every two hours until one succeeds, without the user doing anything.
- **AC-6** A run fails with a non-zero exit, and logs the reason to the user journal, when any of these happen: a path listed for its machine is missing, a database snapshot fails its integrity check, or S3 doesn't confirm the upload matches what was sent. A failed run doesn't count as the last successful backup.
- **AC-7** Every new interactive shell prints a one-line warning when the last successful backup is more than 3 days old, or there has never been one. If it's recent, nothing extra is printed.
- **AC-8** For each machine's object, an older version is deleted only once it has been superseded for at least 30 days and at least 90 newer versions exist. The current version is never deleted. With no uploads, nothing is ever deleted, and uploads made in a burst don't delete anything for 30 days.
- **AC-9** The credentials the timer uses on a machine can upload that machine's object and nothing else. They can't read, list or delete any version of any machine's object.
- **AC-10** `backup setup`, run with the admin profile, leaves behind: the bucket in eu-central-1 with versioning, the retention rule and public access blocked; the IAM user `backup-<hostname>` for the machine it runs on; and a `backup` profile in `~/.aws` holding that user's key. It prints what it created and what was already there. A second run changes nothing and doesn't fail.
- **AC-11** `backup restore <dir> [--host H] [version-id]` asks for the private key without echoing it, then downloads the current or the given version of host `H`'s object (by default the current hostname's), decrypts it, unpacks it into `<dir>`, and prints where the files are. It refuses if `<dir>` isn't empty. It leaves no copy of the key on disk. It works on a fresh machine that has only the repo, `age`, `aws`, and an admin access key created there through a console sign-in.
- **AC-12** A successful restore is recorded. Every new interactive shell prints a one-line warning when the last restore is more than 90 days old, or there has never been one.
- **AC-13** `initialize.sh` installs and enables the timer, and running it again is harmless.
- **AC-14** Two machines backing up to the same bucket never overwrite or expire each other's versions.

## Agreed design
A daily systemd user timer runs `bin/backup`. It snapshots the SQLite databases, packs the listed paths into a gzipped tar, encrypts it with age to a public key whose private key lives only in the password manager, and uploads it to that machine's own key in a versioned S3 bucket. Every machine backs up a common list of paths plus the paths listed for its hostname. A lifecycle rule deletes an older version only once 90 newer ones exist and it has been superseded for 30 days, so neither a stopped timer nor a burst of junk uploads empties the bucket. Each machine has its own IAM user, which can only put that machine's object. Each new shell warns when the last backup or the last restore drill is too old. `backup restore` unpacks a version into an empty directory, and running it also counts as the restore drill.

No specimen; nothing in this change is visual.

## Technical decisions
- **Stored formats** In S3: one object per machine, `<hostname>/backup.tar.age`, with the hostname from `hostname -s`. It is a gzipped tar with paths relative to `$HOME` (`haushaltsbuch.db` and `myinvoice.db` as consistent snapshots under their own names, without their `-wal` and `-shm` files), encrypted to one age recipient. The bucket is versioned, with a lifecycle rule `NoncurrentVersionExpiration` with `NewerNoncurrentVersions=90` and `NoncurrentDays=30`. On the laptop: `~/.local/state/backup/last-backup` and `~/.local/state/backup/last-restore` (timestamps), a `backup` profile in `~/.aws/credentials`, and links to the units in `~/.config/systemd/user/`. Nothing is migrated.
- **Wire formats** The commands `backup`, `backup setup [--profile P]` and `backup restore <dir> [--host H] [version-id] [--profile P]`, where `--profile` defaults to `default`; a non-zero exit on failure; the AWS profile name `backup`.
- **Trust boundaries** A daily outgoing upload of encrypted secrets to S3. Each machine's IAM user `backup-<hostname>` gets only `s3:PutObject` on that machine's key. The private key lives only in the password manager and is typed in at restore. Restore and setup use admin credentials that the user supplies. On a new machine, these come from an access key created through an AWS console sign-in. That sign-in and its MFA must work without this laptop; this is the user's to ensure, not the code's. The bucket has public access blocked and SSE-S3 encryption.
- **Dependencies** None new. `age`, `aws`, `sqlite3`, `tar` and systemd are already installed.

## Nudges
- One script, `bin/backup`, in bash like `bin/push-projects`, with the subcommands (no subcommand) / `setup` / `restore`. The path lists (the common one and one per hostname), bucket name, object key pattern and age public key are constants at its top, the one home for each.
- The unit files live in a new `systemd/` directory: `backup.service` with `Restart=on-failure`, `RestartSec=1h` and `StartLimitIntervalSec=0`, so it keeps retrying hourly, and `backup.timer` with `OnCalendar=daily` and `Persistent=true`.
- Database snapshots are taken with `sqlite3 .backup`. Intermediate files go in `$XDG_RUNTIME_DIR` (tmpfs), not on disk.
- Upload with `aws s3api put-object --content-md5`, so S3 itself rejects a corrupted upload.
- The age key pair is generated once with `age-keygen`, the secret goes straight into the password manager, and only the public key is written into `bin/backup`. `setup` prints these steps if the public key is missing.
- The freshness checks go into `bash.sh` and only read the two timestamp files: no network, no AWS call.
- A comment in the setup code says why `backup-<hostname>` must not get read or delete rights, so nobody "fixes" it later to make automatic verification possible.

## Out of scope
- The dead `maintenance.py` line in the crontab. The crontab isn't in this repo; removing it is a one-time `crontab -e`.
- Installing `age` and `aws` on a new machine.
- Backing up anything beyond the lists in AC-1.

## Ruled out
- **restic** - deduplication buys nothing at about 10 MB per run, and restores would depend on restic's repository format.
- **rclone, as transport or as crypt** - a new dependency. rclone crypt keeps its password in `rclone.conf` on the laptop and has no S3-style lifecycle rules.
- **A private GitHub repo as the destination** - grows by about 10 MB per run for good, and puts key material on GitHub.
- **Age-based expiry** - it deletes every backup once uploads stop.
- **Delete rights for the `backup-<hostname>` users** - anyone holding the laptop could wipe every version.
- **Retention by version count alone** - 91 junk uploads from a stolen laptop would push every good version out within a day.
- **S3 Object Lock** - the bucket would have to be created with it, and the 30-day age condition protects just as well for this purpose.
- **A fully automatic restore test with the key on the laptop** - a stolen laptop would expose all 90 old versions.
- **A restore test in the cloud** - Lambda, IAM roles, Secrets Manager and alerting are far more machinery than the backup itself.
- **A read-only `backup-restore` IAM user with its key in the password manager** - an extra user and policy; a console sign-in on the new machine does the job.
- **The admin access key in the password manager** - a leak of the password manager would expose full admin access to the AWS account.
- **Restoring straight into `$HOME`** - it can overwrite newer files on a machine that isn't empty.
- **One IAM user shared by all machines** - IAM allows only two access keys per user, and revoking a stolen laptop's key would cut off the others.
- **One full path list, missing paths skipped** - a moved path would silently stop being backed up, the way `maintenance.py` silently stopped running.
- **A per-machine path list in a local file** - the list would live outside the repo and be lost with its laptop.
- **Enabling the timer only in `backup setup`, or only where a `backup` profile exists** - every machine should be backed up, and on one not yet set up the warning is the reminder.
- **Hourly uploads** - with versions counted, 90 versions would cover less than four days.
