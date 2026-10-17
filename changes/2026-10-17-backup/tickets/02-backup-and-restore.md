---
criteria:  CRITERIA.md
closes:    AC-1, AC-2, AC-3, AC-6, AC-11
advances:  AC-12, AC-14
after:     01-check-command
status:    done
attempts:  1
---

## Build
`bin/backup` with no subcommand packs, encrypts and uploads the machine's paths to its own S3 object, and `backup restore` downloads a version, decrypts it and unpacks it into an empty directory.

## Done when
> **AC-1** One successful `backup` run puts the paths listed for its machine into that machine's S3 object. On every machine that is `~/Documents`, `~/.ssh`, `~/.aws`, `~/.local/share/keyrings` and every `~/.claude/projects/*/memory/`. On `njord` it is also `~/.myinvoice` and `~/Projects/haushaltsbuch/haushaltsbuch.db`, and its `~/Documents` includes `Certificates/` and the `.pfx` files.

> **AC-2** The restored `haushaltsbuch.db` and `myinvoice.db` pass `PRAGMA integrity_check` and contain changes that were still only in the `-wal` file when the backup ran. The backup holds no `-wal` or `-shm` file for either database.

> **AC-3** The uploaded object can't be read without the age private key. No file on any backed-up machine and nothing in the repo contains that private key.

> **AC-6** A run fails with a non-zero exit, and logs the reason to the user journal, when any of these happen: a path listed for its machine is missing, a database snapshot fails its integrity check, or S3 doesn't confirm the upload matches what was sent. A failed run doesn't count as the last successful backup. A `~/.claude/projects/*/memory/` pattern that matches no directory is not a missing path.

> **AC-11** `backup restore <dir> [--host H] [version-id]` asks for the private key without echoing it, then downloads the current or the given version of host `H`'s object (by default the current hostname's), decrypts it, unpacks it into `<dir>`, and prints where the files are. It refuses if `<dir>` isn't empty. It leaves no copy of the key on disk. It works on a fresh machine that has only the repo, `age`, `aws`, and an admin access key created there through a console sign-in.

Toward AC-12: a successful restore writes `~/.local/state/backup/last-restore`, and a failed one does not.

Toward AC-14: each machine's run uploads only to its own object, `<hostname>/backup.tar.age`.

## Toward
> **AC-12** A successful restore is recorded. Every new interactive shell prints a one-line warning when the last restore is more than 90 days old, or there has never been one.

> **AC-14** Two machines backing up to the same bucket never overwrite or expire each other's versions.

## Technical decisions
> **Stored formats** In S3: one object per machine, `<hostname>/backup.tar.age`, with the hostname from `hostname -s`. It is a gzipped tar with paths relative to `$HOME` (`haushaltsbuch.db` and `myinvoice.db` as consistent snapshots under their own names, without their `-wal` and `-shm` files), encrypted to one age recipient. The bucket is versioned, with a lifecycle rule `NoncurrentVersionExpiration` with `NewerNoncurrentVersions=90` and `NoncurrentDays=30`. On the laptop: `~/.local/state/backup/last-backup` and `~/.local/state/backup/last-restore` (timestamps), a `backup` profile in `~/.aws/credentials`, and links to the units in `~/.config/systemd/user/`. Nothing is migrated.

> **Wire formats** The commands `backup`, `backup setup [--profile P]` and `backup restore <dir> [--host H] [version-id] [--profile P]`, where `--profile` defaults to `default`; a non-zero exit on failure; the AWS profile name `backup`.

> **Trust boundaries** A daily outgoing upload of encrypted secrets to S3. Each machine's IAM user `backup-<hostname>` gets only `s3:PutObject` on that machine's key. The private key lives only in the password manager and is typed in at restore. Restore and setup use admin credentials that the user supplies. On a new machine, these come from an access key created through an AWS console sign-in. That sign-in and its MFA must work without this laptop; this is the user's to ensure, not the code's. The bucket has public access blocked and SSE-S3 encryption.

> **Dependencies** None new. `age`, `aws`, `sqlite3`, `tar` and systemd are already installed.

## Nudges
> One script, `bin/backup`, in bash like `bin/push-projects`, with the subcommands (no subcommand) / `setup` / `restore`. The path lists (the common one and one per hostname), bucket name, object key pattern and age public key are constants at its top, the one home for each.

> Database snapshots are taken with `sqlite3 .backup`. Intermediate files go in `$XDG_RUNTIME_DIR` (tmpfs), not on disk.

> Upload with `aws s3api put-object --content-md5`, so S3 itself rejects a corrupted upload.

> The age key pair is generated once with `age-keygen`, the secret goes straight into the password manager, and only the public key is written into `bin/backup`. `setup` prints these steps if the public key is missing.

## Context
The design: a daily systemd user timer runs `bin/backup`. It snapshots the SQLite databases, packs the listed paths into a gzipped tar, encrypts it with age to a public key whose private key lives only in the password manager, and uploads it to that machine's own key in a versioned S3 bucket. Every machine backs up a common list of paths plus the paths listed for its hostname. `backup restore` unpacks a version into an empty directory, and running it also counts as the restore drill.

`bin/` holds small bash scripts; `bin/push-projects` is the nearest in style. `initialize.sh` already links every file in `bin/` into `~/.local/bin`, so `bin/backup` needs nothing there. On `njord` today, `~/.myinvoice` holds `myinvoice.db` beside `backups/` and `rechnungen/`, and `~/Projects/haushaltsbuch/` holds `haushaltsbuch.db` with a live `-wal` and `-shm`.

Shared with the other tickets:
- Ticket 03 adds `backup setup` to this script, creating the bucket, the lifecycle rule and the per-machine IAM user that this run uploads with. The bucket name is `jorgenschaefer-backup`, in eu-central-1.
- The run uploads with the AWS profile `backup`; restore with the profile given by `--profile`.
- Both timestamp files hold one line: seconds since the epoch, as `date +%s` prints them. Ticket 04's shell warnings read them and nothing else. A successful run writes `last-backup`, a successful restore writes `last-restore`.
- `bin/backup` reads the clock in one named function only, and this ticket adds that rule to the structural check from ticket 01.
- The age public key does not exist yet; the user generates it. Until it is in the script, a run fails as under AC-6.

## Not here
- `backup setup` and everything it creates in AWS are ticket 03.
- The timer, the retries, `initialize.sh` and the shell warnings are ticket 04.
- Out of scope: Installing `age` and `aws` on a new machine.
- Out of scope: Backing up anything beyond the lists in AC-1.

## Left standing
- **Review findings not fixed.** Two nits from round one. (1) Move `object_key` below its callers: kept at the top, because the nudge puts the object key pattern among the constants there. (2) One source file, `bin/backup`, has two test files, `tests/backup_test.sh` and `tests/restore_test.sh`, which together with `tests/backup_helpers.sh` come to over 500 lines. They split along the two subcommands. Splitting `bin/backup` itself is awkward, because `initialize.sh` puts every file in `bin/` onto `PATH`, so a sourced library can't live there. Round two had no blockers or nits; its one should-fix (an existing read-only directory blamed the key) is fixed.
- **Checks not run: real AWS.** The bucket and the `backup` profile come with ticket 03, so no run has reached S3. Every upload and download in the tests goes to a fake `aws` (`tests/backup_helpers.sh`). It models what this code relies on: put-object rejects a wrong Content-MD5, objects are versioned, `--query VersionId --output text` prints the version id, a profile can be missing, and there is no default region. The first real run after ticket 03 is the first time any of that meets real S3.
- **AC-1 on njord itself:** the tests prove the lists with a scratch home. I also ran a copy of the script on njord's real home with a throwaway key and the fake S3. The archive held all 93 `~/.claude/projects/*/memory/` directories, both `.pfx` files, `.myinvoice` and `haushaltsbuch.db`, and no `-wal` or `-shm` file. Both snapshots passed `integrity_check`. I deleted the `last-backup` it wrote. `~/Documents/Certificates/` does not exist on njord today, so the real run could not show it. It is packed as part of `~/Documents`, not listed on its own, so it is not checked as a missing path; the test puts one there and finds it in the archive.
- **AC-6, the user journal:** the tests check the journal through a fake `logger`. On njord, I checked by hand that `logger -t … -p user.err` lands in `journalctl --user`.
- **AC-3, no private key on any backed-up machine:** the tests prove that nothing in the repository holds one, and that a restore leaves no copy in the scratch home, the runtime directory or the restore target. The key reaches `age` through a process substitution (a pipe). Where else the user keeps the key is up to them.
- **AC-11, works on a fresh machine:** no test can prove this; I checked it by reading the code. A restore uses only bash, coreutils, `tar`/`gzip`, `age` and `aws`. It passes `--region eu-central-1` itself, so the profile needs no region, and the profile defaults to `default`, which is the one `aws configure` writes. One precondition: it puts the downloaded, still-encrypted object in `$XDG_RUNTIME_DIR`, and stops with "unbound variable" if that isn't set. A graphical or systemd login session sets it.
- **Formats and names other code will be written against:**
  - Journal lines, under the syslog tag `backup` with the PID as the run id: `backup_failed reason=<r> [path=|database=|key=]` for a run, and `restore_failed reason=<r> [dir=|key=]` for a restore.
  - Run reasons: `public_key_missing`, `path_missing`, `snapshot_failed`, `database_damaged`, `pack_failed`, `encrypt_failed`, `upload_failed`.
  - Restore reasons: `directory_not_empty`, `directory_unusable`, `download_failed`, `unpack_failed`.
  - Exit codes: 1 on failure, 2 on a usage error.
  - Test layout: tests of `bin/backup` live in `tests/`, not next to the script, because of `bin/`. The fakes for `aws`, `logger` and `hostname` are bash functions exported by `tests/backup_helpers.sh`, and ticket 03's tests can extend them. The fake `aws` uses `openssl` to check Content-MD5 independently of the script; `openssl` is needed only by the tests.
- **Decisions beyond the nudges:**
  - **An extra constant, `SQLITE_DATABASES`.** The nudge names the path lists, the bucket, the key pattern and the public key as the constants. Snapshotting needs to know which listed files are databases, so this list is a fifth constant beside them. A database inside a listed path that is missing fails the run (`snapshot_failed`), just as a missing listed path does.
  - **`-journal` excluded too.** Besides `-wal` and `-shm`, any `-journal` file of a snapshotted database is left out, for the same reason.
  - **Content-MD5 without a new dependency.** The value is computed with `md5sum` and `base64` from coreutils.
  - **A file changing during a run fails it.** If a file changes while tar reads it (tar exit 1), the run fails with `pack_failed` instead of uploading. The retry from ticket 04 is what covers this.
  - **A missing restore directory is created.** `backup restore <dir>` creates `<dir>` if it doesn't exist. It refuses a non-empty directory, and one it can't write, before asking for the key.
- **Ticket 01's open item:** ticket 01 left open a structural rule against tests in `bin/`. It is outside this ticket's Done when and is still open.
