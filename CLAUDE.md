# Config

Check: `./check.sh`

Tests are files named `*_test.sh`, run with bash, failing by exiting
non-zero.

`bin/backup` reads the clock only in its function `now`, which
`check.sh` checks.
