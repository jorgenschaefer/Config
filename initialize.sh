#!/bin/sh

# Initialize my home directory structure

GITREPO_URL="https://github.com/jorgenschaefer/Config.git"

# Ignored for now: Music Pictures Videos
mkdir "$HOME/.local/bin"
mkdir "$HOME/Projects"

if [ ! -d "$HOME/Projects/Config" ]
then
  git clone "$GITREPO_URL" "$HOME/Projects/Config"
fi

ensure_contains () {
  local FILE="$1"
  local LINE="$2"
  if ! [ -f "$FILE" ] || ! fgrep -q "$LINE" "$FILE"
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

# gitconfig
"$HOME/Projects/Config/gitconfig.sh"
