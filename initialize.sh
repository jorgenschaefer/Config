#!/bin/sh

# Initialize my home directory structure

GITREPO_URL="https://github.com/jorgenschaefer/Config.git"

mkdir -p "$HOME/.local/bin"
mkdir -p "$HOME/Projects"

if [ ! -d "$HOME/Projects/Config" ]
then
  git clone "$GITREPO_URL" "$HOME/Projects/Config"
fi

ensure_contains () {
  FILE="$1"
  LINE="$2"
  if ! [ -f "$FILE" ] || ! grep -Fq "$LINE" "$FILE"
  then
    echo "$LINE" >> "$FILE"
  fi
}

# bin/
cp -ns "$HOME"/Projects/Config/bin/* "$HOME"/.local/bin/

# bash.sh
ensure_contains "$HOME/.bash_profile" \
                '. ~/Projects/Config/bash.sh'
ensure_contains "$HOME/.bashrc" \
                '. ~/Projects/Config/bash.sh'

# bash_logout.sh
ensure_contains "$HOME/.bash_logout" \
                '. ~/Projects/Config/bash_logout.sh'

# inputrc
test -e "$HOME/.inputrc" || ln -s "Projects/Config/inputrc" "$HOME/.inputrc"

# claude/
mkdir -p "$HOME/.claude"
test -e "$HOME/.claude/CLAUDE.md" || \
  ln -s "../Projects/Config/claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"
test -e "$HOME/.claude/statusline-command.sh" || \
  ln -s "../Projects/Config/claude/statusline-command.sh" "$HOME/.claude/statusline-command.sh"
test -e "$HOME/.claude/settings.json" || \
  ln -s "../Projects/Config/claude/settings.json" "$HOME/.claude/settings.json"

# systemd/: the daily backup. The reload picks up unit files a pull
# has changed; enabling an enabled timer, or starting a started one,
# changes nothing.
mkdir -p "$HOME/.config/systemd/user"
for unit in backup.service backup.timer
do
  test -e "$HOME/.config/systemd/user/$unit" || \
    ln -s "../../../Projects/Config/systemd/$unit" "$HOME/.config/systemd/user/$unit"
done
systemctl --user daemon-reload
systemctl --user enable --now backup.timer

# gitconfig
"$HOME/Projects/Config/gitconfig.sh"
