---
effort: S
utility: S
---

# List a host's backup versions

`backup restore <dir> [--host H] [version-id]` can restore an older
version, but nothing lists which versions exist and when they were
made. Picking one takes a hand-written
`aws s3api list-object-versions --bucket jorgenschaefer-backup --prefix <host>/`
with an admin profile.

The current version can be the one you don't want: junk uploads from
a stolen laptop, or a fresh machine's first run backing up an almost
empty home.

Idea: `backup versions [--host H]`, listing a host's stored versions
with their dates, using the admin profile like `restore` and `setup`
do (`bin/backup`).
