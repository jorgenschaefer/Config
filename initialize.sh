#!/bin/sh

# Initialize my home directory structure

GITREPO_URL="https://github.com/jorgenschaefer/Config.git"

# Ignored for now: Music Pictures Videos
for dir in .local/bin .cache/tmp \
           Documents Downloads Files Programs Projects Public
do
    test -d "$HOME/$dir" || mkdir -p "$HOME/$dir"
done

if [ ! -f "$HOME/Projects/Config/initialize.sh" ]
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

# emacs.el
test -d "$HOME/.emacs.d" || mkdir "$HOME/.emacs.d"
ensure_contains "$HOME/.emacs.d/init.el" \
                '(load "~/Projects/Config/emacs.el" t t t)'

# inputrc
test -f "$HOME/.inputrc" || ln -s "Projects/Config/inputrc" "$HOME/.inputrc"

# gitconfig
"$HOME/Projects/Config/gitconfig.sh"

# virtualenvwrapper
if [ -z "$VIRTUALENVWRAPPER_HOOK_DIR" ] && type virtualenvwrapper.sh &> /dev/null
then
#    . virtualenvwrapper.sh
:
fi

TEMPFILE=$(mktemp)
trap "rm -f $TEMPFILE" EXIT

crontab -l > "$TEMPFILE"
ensure_contains "$TEMPFILE" '10 * * * * ~/Projects/Config/maintenance.py'
crontab < "$TEMPFILE"
