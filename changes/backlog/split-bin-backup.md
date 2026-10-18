---
effort: M
utility: M
---

# Split bin/backup into parts

`bin/backup` is 484 lines, just under the 500-line limit of the coding
standard, and holds three parts: the run, `restore` and `setup`. Its
tests are three files beside a shared helper, about 1270 lines in all
(`tests/backup_test.sh`, `tests/restore_test.sh`,
`tests/setup_test.sh`, `tests/backup_helpers.sh`), where the standard
wants one test file per source part.

The parts can't be sourced files in `bin/`: `initialize.sh` links
every file there onto `PATH`. They need a home outside `bin/`, which
`check.sh`'s lint then has to cover.

The next change that adds to `bin/backup` has to do this split first.
