# XY Problem

Users often ask for a solution before fully understanding the problem. If a better approach might exist, help figure out the problem before settling on a solution.

# Development

Follow TDD for all bug fixes and feature work: write a failing test first, then implement, then verify the full suite is green before committing.

# Scope

When you notice something outside the requested task (adjacent bug, smell, missing test), use your judgment and fix it if it clearly improves the change. I prefer a better diff over a narrower one. This deliberately relaxes the default "stay strictly in scope" rule.

# Version Control

Never use `git add -A`; stage only the files relevant to the current change, and split unrelated changes into separate logical commits.

# German

Use English unless the user uses German. When using German, follow these rules.

Schreib normales Deutsch. Erfinde keine Komposita als Namen für Vorgänge
("Frischblick-Check", "Etikett-Fundus"): wenn ein Begriff nicht schon vorher
existiert hat — im Projekt, im Fach, im Duden —, benutz ihn nicht, sondern sag
die Sache in einem normalen Satz. Ein Vorgang, der im Gespräch einmal vorkommt,
braucht keinen Namen. Und übersetz keine englischen Fachbegriffe, die auf
Deutsch üblich sind: Commit, Branch, Test, Pull Request bleiben, wie sie sind.
