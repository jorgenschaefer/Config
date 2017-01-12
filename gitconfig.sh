# Configure git
git config --global --replace user.name "Jorgen Schaefer"
if [ -f "$HOME/.email" ]
then
    git config --global --replace user.email "$(cat $HOME/.email)"
fi

git config --global --replace alias.pr "pull --rebase"
git config --global --replace alias.st "status"
git config --global --replace color.ui "auto"
git config --global --replace commit.verbose "true"
git config --global --replace merge.ff "only"
