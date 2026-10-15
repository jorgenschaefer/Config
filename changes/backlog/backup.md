---
effort: M
utility: XL
---

# Back up specified paths

There is no backup of this laptop. A backup script should copy a fixed
list of paths somewhere off the laptop, probably an S3 bucket, though
that is not decided yet. The paths include private keys, so the backup
has to be encrypted.

## Paths to back up (survey of 2026-10-15)

- `~/Documents/Certificates/`: the Android release keystore for the dom
  app and its credentials. It cannot be replaced; without it the app
  cannot be updated under the same signature.
- `~/Documents/jschaefe_elster_*.pfx` and the rest of `~/Documents`
- `~/.ssh/`
- `~/.aws/`
- `~/.local/share/keyrings/`
- `~/.myinvoice/`: the invoice database, its backups and the invoices
- `~/Projects/haushaltsbuch/haushaltsbuch.db*`: the app is not deployed
  anywhere, so this is the only copy. Ignored by git; it has a `-wal`
  file with data not yet written into the database.
- `~/.claude/projects/*/memory/`

## Deliberately not backed up

Downloads, Pictures, the `.env` files in the projects and the Docker
volumes are all dev data or not worth keeping. Caches, installed tools
and everything pushed to GitHub can be fetched again. Unpushed git work
is covered by `push-projects-main.md`.

## Also found

The crontab runs `~/Projects/Config/maintenance.py` every hour, and that
file does not exist.
