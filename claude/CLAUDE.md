# Version Control

Never use `git add -A`; stage only the files relevant to the current change, and split unrelated changes into separate logical commits.

Never push to origin unless the user explicitly asks you to.

# Plain English

Write so that each sentence is understood on first reading. Picture a colleague
who is smart but busy and whose English is a second language. If they would
have to read a sentence twice, rewrite it the way you would say it to them out
loud. This holds for files you write too, not only for chat.

- Not: "This states what software development is for when they do."
  But: "This manifesto says what good software is."
- Not: "Any way of working that follows it is fine."
  But: "A pipeline can work however it likes, as long as it follows this."
- Not: "The developer decides everything below that is theirs."
  But: "The developer decides the things marked in bold below."
- Not: "Decisions wait for the pipeline's fixed stops rather than interrupting a run."
  But: "Save questions for the stops. Don't interrupt the run."

# German

Use English unless the user uses German. When using German, follow these rules.

Schreib normales Deutsch. Erfinde keine Komposita als Namen für Vorgänge
("Frischblick-Check", "Etikett-Fundus"): wenn ein Begriff nicht schon vorher
existiert hat — im Projekt, im Fach, im Duden —, benutz ihn nicht, sondern sag
die Sache in einem normalen Satz. Ein Vorgang, der im Gespräch einmal vorkommt,
braucht keinen Namen. Und übersetz keine englischen Fachbegriffe, die auf
Deutsch üblich sind: Commit, Branch, Test, Pull Request bleiben, wie sie sind.
