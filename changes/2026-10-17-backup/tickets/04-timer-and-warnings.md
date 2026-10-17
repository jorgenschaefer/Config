---
criteria:  CRITERIA.md
closes:    AC-4, AC-5, AC-7, AC-12, AC-13
advances:
after:     01-check-command, 02-backup-and-restore
status:    ready
attempts:  0
---

## Build
A systemd user timer and service that run `backup` daily and retry it after failure, installed and enabled by `initialize.sh`, and warnings in every new interactive shell when the last backup or the last restore is too old.

## Done when
> **AC-4** The backup runs once a day without the user doing anything. A day missed because the laptop was off is caught up after the next login.

> **AC-5** A run that fails, e.g. because the laptop is offline, is retried at least every two hours until one succeeds, without the user doing anything.

> **AC-7** Every new interactive shell prints a one-line warning when the last successful backup is more than 3 days old, or there has never been one. If it's recent, nothing extra is printed.

> **AC-12** A successful restore is recorded. Every new interactive shell prints a one-line warning when the last restore is more than 90 days old, or there has never been one.

> **AC-13** `initialize.sh` installs and enables the timer, and running it again is harmless.

## Technical decisions
> **Stored formats** In S3: one object per machine, `<hostname>/backup.tar.age`, with the hostname from `hostname -s`. It is a gzipped tar with paths relative to `$HOME` (`haushaltsbuch.db` and `myinvoice.db` as consistent snapshots under their own names, without their `-wal` and `-shm` files), encrypted to one age recipient. The bucket is versioned, with a lifecycle rule `NoncurrentVersionExpiration` with `NewerNoncurrentVersions=90` and `NoncurrentDays=30`. On the laptop: `~/.local/state/backup/last-backup` and `~/.local/state/backup/last-restore` (timestamps), a `backup` profile in `~/.aws/credentials`, and links to the units in `~/.config/systemd/user/`. Nothing is migrated.

> **Wire formats** The commands `backup`, `backup setup [--profile P]` and `backup restore <dir> [--host H] [version-id] [--profile P]`, where `--profile` defaults to `default`; a non-zero exit on failure; the AWS profile name `backup`.

> **Trust boundaries** A daily outgoing upload of encrypted secrets to S3. Each machine's IAM user `backup-<hostname>` gets only `s3:PutObject` on that machine's key. The private key lives only in the password manager and is typed in at restore. Restore and setup use admin credentials that the user supplies. On a new machine, these come from an access key created through an AWS console sign-in. That sign-in and its MFA must work without this laptop; this is the user's to ensure, not the code's. The bucket has public access blocked and SSE-S3 encryption.

> **Dependencies** None new. `age`, `aws`, `sqlite3`, `tar` and systemd are already installed.

## Nudges
> The unit files live in a new `systemd/` directory: `backup.service` with `Restart=on-failure`, `RestartSec=1h` and `StartLimitIntervalSec=0`, so it keeps retrying hourly, and `backup.timer` with `OnCalendar=daily` and `Persistent=true`.

> The freshness checks go into `bash.sh` and only read the two timestamp files: no network, no AWS call.

## Context
Ticket 02 builds `bin/backup`: a run exits non-zero on failure, writes `~/.local/state/backup/last-backup` on success, and a successful `backup restore` writes `~/.local/state/backup/last-restore`. Each file holds one line, seconds since the epoch as `date +%s` prints them. `initialize.sh` already links `bin/backup` into `~/.local/bin`.

`bash.sh` is sourced from both `~/.bash_profile` and `~/.bashrc` and has a block that runs only in interactive shells. `initialize.sh` is POSIX `sh` and already makes each of its steps safe to repeat. The machine runs systemd 262.

The timer is enabled on every machine, whether or not `backup setup` has run there: on a machine not yet set up, the runs fail and the backup warning is the reminder.

The new `systemd/` directory is a new kind of file in the repository; this ticket adds a check of the unit files to the check command.

`bash.sh` reads the clock in one named function only; this ticket extends ticket 02's structural rule on clock reads to cover it.

## Not here
- What a run does and when it fails is ticket 02.
- Out of scope: The dead `maintenance.py` line in the crontab. The crontab isn't in this repo; removing it is a one-time `crontab -e`.
- Out of scope: Installing `age` and `aws` on a new machine.

## Left standing
