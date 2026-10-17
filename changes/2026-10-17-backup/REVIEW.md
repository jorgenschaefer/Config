## For you

1. **Departure, fixed (ticket 04's blocker):** a failed run cut off by shutdown is now retried at the next login. The timer gained `OnStartupSec=1min`, and the service an `ExecCondition` that skips the run once a backup has succeeded today. This adds to the nudge's unit settings, and the unit file now reads the clock (through `find -newermt`), outside the two clock functions `check.sh` pins.
2. **Decision (review round one):** if a lost machine's `~/.aws` is restored onto its replacement, `backup setup` finds the old `backup` key, keeps it, and never revokes the stolen laptop's upload rights. The fix is either to rotate the key on every setup, which goes against AC-10's "a second run changes nothing", or to leave the `backup` profile out of the backup, which goes against AC-1's `~/.aws`. Until you decide, revoke a lost machine's key by hand in IAM.
3. **Decision (ticket 03):** the `[default]` AWS access key and secret were printed into a review agent's transcript. Whether to rotate them is yours to decide.
4. **Decision (review round one):** nothing lists a host's versions and their dates, so restoring anything other than the current version takes a hand-written `aws s3api list-object-versions`. The design expects the current version to be bad at times (junk uploads, or a fresh machine's first run).
5. **Not checked (tickets 02, 03):** no run, restore or setup has ever met real AWS. Every call goes to a fake. The first real `backup setup` and run are the first test of them. 4 more below.

## Open

- **One source file, three test files (review round one, should-fix; ticket 02 nit).** `bin/backup` is tested by `tests/backup_test.sh`, `restore_test.sh` and `setup_test.sh`, which together with `backup_helpers.sh` come to over 1100 lines. The standard wants one test file per source part, so `bin/backup` holds more than one part. Not fixed: splitting it means sourced parts outside `bin/`, because `initialize.sh` links all of `bin/` onto `PATH`. That is a restructuring with no change in behaviour, and no review round was left to cover it.
- **Nothing is backed up yet (review round one).** `AGE_PUBLIC_KEY` is empty and `backup setup` has not run, so every run fails until both are done. The timer is not enabled on njord either: `initialize.sh` has not been re-run (ticket 04).
- **The login run is checked by hand only.** `tests/units_test.sh` runs the `ExecCondition` against a scratch home and pins its midnight boundary. On njord, a throwaway unit with the same condition ran without a stamp and was skipped with one stamped today. The second review saw a skip in the middle of `Restart=` end as success, not as a failure. Not checked: the timer's `OnStartupSec` firing at a real login. With lingering on, it would fire only at boot.
- **Nit (review round one): two fakes of the AWS account.** `tests/backup_helpers.sh` and `tests/setup_test.sh` each have their own `aws`, so no test runs setup and then a run. Today the two sides meet only through `UPLOAD_PROFILE` and `object_key`.

## Findings not fixed, nits left for acceptance

- Review round one, nits: three copies of the test harness (`start_test`, `fail`, `finish`) in `bash_test.sh`, `initialize_test.sh` and `backup_helpers.sh`. The path and format of the timestamp files are written out separately in `bin/backup` and `bash.sh`, and no test crosses that seam.
- Review round two, nits:
  - The `--host` hint is added to every failed download without `--host`, including a wrong version id or the network being down. The aws error above it names the real cause.
  - No test pins that the hint is left out when `--host` is given.
  - The "skips today" unit test can fail if `./check.sh` runs across midnight.
  - The helper fake answers `aws configure get` itself, while `setup_test.sh` uses the real one.
- Ticket 01: nothing stops a test going into `bin/`, where `initialize.sh` would put it on `PATH`. This is still open.
- Ticket 03, nits:
  - `aws configure set` puts the new secret in the process arguments for a moment.
  - Not every setup failure message names the profile.
  - `setup --profile` without a value does not reach the journal.
- Ticket 03: with more than 100 IAM users, the user lookup can miss. Right after a create call, AWS can lag, and re-running setup fixes it. Setup does not look for policies attached to `backup-<hostname>` by hand.
- Ticket 04: no test fails if `Restart=`, the retry delays or `Persistent=` are removed.

## Fixed in this review

- Ticket 04's blocker: a failed run is retried at the next login. See For you, item 1.
- Round one: a run without a `backup` profile now fails with `profile_missing` and says to run `backup setup`, instead of `upload_failed`. Round two: a missing path is still reported first.
- Round one: a restore that finds no object for this hostname says to name the machine with `--host`.
- Round two: tests pin the login run's midnight boundary. The comments say the run fires when the user manager starts, not at every login.

## Checks

`./check.sh` passes on the final commit. Not run: anything against real AWS, a real login with the timer enabled, and a restore on a fresh machine.
