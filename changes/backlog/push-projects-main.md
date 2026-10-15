---
effort: S
utility: M
---

# Push the main branch of every project

There is no backup of this laptop. Work that is committed but not pushed
exists only here. A script should go through `~/Projects/*` and run
`git push` for the main branch only, on every project that has a remote,
and warn about every repo that has no remote.

## Evidence (survey of 2026-10-15)

- Repos with a remote and unpushed commits on `main`: drk-barmbek (38),
  infrastructure (26), skills (17), einsatz (3), and 1–2 each in ausgaben,
  dom, drk-meldekopf, ekg-trainer, isabellkoester.com, jorgenschaefer.de,
  katzenpsychologin-von-knobloch.de and kh-finder.
- Repos without a remote, which should get the warning: `isa-rezept` and
  `patientenakte-isa`.
- `Archive`, `ZKN` and `digitalservicebund` are not repos themselves but
  hold repos one level down. ZKN/eingabeportal has 20 unpushed commits on
  `main`, ZKN/tauschportal 1.
- Not covered by pushing `main`: other branches (drk-barmbek
  `ereignisregistrierung`, patientenakte `testakte-evals`, haushaltsbuch
  `kontierung-zuruecknehmen`), stashes (drk-barmbek, haushaltsbuch) and
  uncommitted changes (isa-rezept).
- `bin/` scripts get linked into `~/.local/bin` by `initialize.sh`.
