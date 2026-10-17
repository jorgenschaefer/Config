---
criteria:  CRITERIA.md
closes:
advances:
after:
status:    ready
attempts:  0
---

## Build
A single check command for this repository, declared in a new project `CLAUDE.md`, that lints the shell scripts and runs the tests, with a structural check that has no rules in it yet.

## Done when
- A `CLAUDE.md` at the repository root declares the command on a line of its own in exactly the form ``Check: `<command>` ``.
- The command lints the repository's shell scripts, runs every test the repository has (none yet), runs a structural check with no rules in it yet, and exits non-zero on any failure.
- It is green on the repository as it stands, with no behaviour of any existing script changed.

## Technical decisions
> **Stored formats** In S3: one object per machine, `<hostname>/backup.tar.age`, with the hostname from `hostname -s`. It is a gzipped tar with paths relative to `$HOME` (`haushaltsbuch.db` and `myinvoice.db` as consistent snapshots under their own names, without their `-wal` and `-shm` files), encrypted to one age recipient. The bucket is versioned, with a lifecycle rule `NoncurrentVersionExpiration` with `NewerNoncurrentVersions=90` and `NoncurrentDays=30`. On the laptop: `~/.local/state/backup/last-backup` and `~/.local/state/backup/last-restore` (timestamps), a `backup` profile in `~/.aws/credentials`, and links to the units in `~/.config/systemd/user/`. Nothing is migrated.

> **Wire formats** The commands `backup`, `backup setup [--profile P]` and `backup restore <dir> [--host H] [version-id] [--profile P]`, where `--profile` defaults to `default`; a non-zero exit on failure; the AWS profile name `backup`.

> **Trust boundaries** A daily outgoing upload of encrypted secrets to S3. Each machine's IAM user `backup-<hostname>` gets only `s3:PutObject` on that machine's key. The private key lives only in the password manager and is typed in at restore. Restore and setup use admin credentials that the user supplies. On a new machine, these come from an access key created through an AWS console sign-in. That sign-in and its MFA must work without this laptop; this is the user's to ensure, not the code's. The bucket has public access blocked and SSE-S3 encryption.

> **Dependencies** None new. `age`, `aws`, `sqlite3`, `tar` and systemd are already installed.

## Nudges

## Context
The repository is a set of bash and sh scripts for a home directory: `bash.sh`, `bash_logout.sh`, `initialize.sh`, `gitconfig.sh`, `claude/statusline-command.sh` and the scripts in `bin/`. It has no tests, no project `CLAUDE.md` and no check command. `claude/CLAUDE.md` is not the project's: `initialize.sh` links it to `~/.claude/CLAUDE.md` as the user's global instructions, so the project's goes at the root.

`shellcheck` is installed; no bash test framework is. The existing scripts have shellcheck findings today.

The later tickets of this change add `bin/backup` and its tests, tests for `bash.sh`, and systemd unit files, and add to this command whatever they need checked. Tests must not go into `bin/`: `initialize.sh` links every file in `bin/` into `~/.local/bin`, which is on `PATH`.

## Not here
`bin/backup`, the unit files and the shell warnings are tickets 02 to 04.

## Left standing
