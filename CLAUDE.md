# Config

Check: `./check.sh`

Tests are files named `*_test.sh`, run with bash, failing by exiting
non-zero. They live in `tests/`. None may be in `bin/`, whose files
`initialize.sh` links onto `PATH`; `check.sh` checks that.

`bin/backup` reads the clock only in its function `now`, and `bash.sh`
only in its function `seconds_since_epoch`, which `check.sh` checks.
