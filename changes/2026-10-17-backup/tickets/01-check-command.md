---
criteria:  CRITERIA.md
closes:
advances:
after:
status:    done
attempts:  1
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

`shellcheck` is installed; no bash test framework is. The existing scripts pass shellcheck.

The later tickets of this change add `bin/backup` and its tests, tests for `bash.sh`, and systemd unit files, and add to this command whatever they need checked. Tests must not go into `bin/`: `initialize.sh` links every file in `bin/` into `~/.local/bin`, which is on `PATH`.

## Not here
`bin/backup`, the unit files and the shell warnings are tickets 02 to 04.

## Left standing
- **No automated test proves the command's failure paths** (lint, test or structural failure gives a non-zero exit). A test of `check.sh` would be a test the command itself runs, and the ticket asks for none yet. Checked by hand in a scratch copy: a shellcheck finding in `bin/` and in a `*.sh` file, a failing `*_test.sh`, and a structural check returning 1 each made `./check.sh` exit 1. A passing test ran and exited 0. Running from another directory works.
- **Formats later code is written against:** the command is `./check.sh` at the root (it is not in `bin/`, so `initialize.sh` does not put it on `PATH`). Tests are any file named `*_test.sh` anywhere in the repository, run with `bash` and failing by exiting non-zero; there is no framework. Lint covers every `*.sh` file and every file in `bin/`, found by `find`, so new scripts are picked up without editing the command. Structural rules are checks called from `check_structure` in `check.sh`.
- **Review nit, not fixed (raised as should-fix in round one, as a nit in round two):** nothing stops a test going into `bin/`, where `initialize.sh` would link it onto `PATH`. A passing `bin/backup_test.sh` would stay green. The rule is not in `CLAUDE.md` because a rule written there comes with its check, and this ticket's structural check has no rules. Ticket 02, which adds `bin/backup` and its tests, should add the rule to `CLAUDE.md` together with its check in `check_structure`.
