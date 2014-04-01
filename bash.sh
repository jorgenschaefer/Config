# Combined bash configuration file, sourced by both bashrc and
# bash_profile.

for bindir in "$HOME/.local/bin" "$HOME/bin"
do
    if [ -d "$bindir" ]
    then
        export PATH="$bindir:${PATH}"
    fi
done

export LANG=POSIX
export LC_CTYPE=de_DE.UTF-8
export LC_NUMERIC=POSIX
export LC_TIME=POSIX
export LC_COLLATE=de_DE.UTF-8
export LC_MONETARY=POSIX
export LC_MESSAGES=POSIX
export LC_PAPER=POSIX
export LC_NAME=POSIX
export LC_ADDRESS=POSIX
export LC_TELEPHONE=POSIX
export LC_MEASUREMENT=POSIX
export LC_IDENTIFICATION=POSIX
export LC_ALL=

if [ -f "$HOME/.email" ]
then
    export EMAIL=$(cat "$HOME/.email")
fi
export HISTCONTROL=ignoredups
export LESS="-MIRnX"
export LESSCHARSET=utf-8
export SCREENDIR="$HOME/tmp/screen"
export TMP="$HOME/tmp"
export TMPDIR="$HOME/tmp"

export PYTHONSTARTUP="$HOME/Projects/Config/pythonrc.py"
unset PYTHONDONTWRITEBYTECODE

export PROJECT_HOME=~/Projects

alias rot13='tr a-zA-Z n-za-mN-ZA-M'
alias mv='mv -i'
alias %='fg'

if type virtualenvwrapper.sh &>/dev/null
then
    . virtualenvwrapper.sh
fi

if [ -f "/etc/bash_completion" ]
then
    . /etc/bash_completion
fi

# Only in interactive shells
if [ -n "$PS1" ]
then
    shopt -s checkwinsize
    set nocaseglob

    # cd with history
    cd () {
        if [ "$1" = "-" ]
        then
            popd &>/dev/null
        elif [ "$1" = "" ]
        then
            pushd ~ &>/dev/null
        elif [ ! -d "$1" ]
        then
            echo "cd: $1: Not a directory" >&2
            return 1
        else
            pushd "$@" &>/dev/null
        fi
    }

    # Colors in non-dumb terminals or in Emacs
    if [ "$EMACS" = "t" ] || [ "$TERM" != "dumb" ]
    then
        eval `dircolors -b`
        alias ls='ls -F --color=auto'
        alias grep='grep --color --exclude-dir=.svn --exclude-dir=.git --exclude="*.pyc"'
    else
        alias ls='ls -F'
        alias grep='grep --exclude-dir=.svn --exclude-dir=.git'
    fi

    # And now, the prompt
    if type tput &>/dev/null && tput setaf 1 >&/dev/null
    then
        c_reset="\\[`tput sgr0`\\]"
        c_bold="\\[`tput bold`\\]"
        c_red="\\[`tput setaf 1`\\]"
        c_green="\\[`tput setaf 2`\\]"
        c_blue="\\[`tput setaf 4`\\]"
    else
        c_reset=""
        c_bold=""
        c_red=""
        c_green=""
        c_blue=""
    fi

    export PROMPT_COMMAND=set_prompt
    function set_prompt () {
        if [ -n "$VIRTUAL_ENV" ]
        then
            local venv="$(basename "$VIRTUAL_ENV")"
        else
            local venv=""
        fi
        if git rev-parse --git-dir > /dev/null 2>&1
        then
            local git_branch=$(git branch 2>/dev/null | sed -ne 's/^\* //p')
            if [ $(git status --porcelain 2>/dev/null | wc -l) -gt 0 ]
            then
                local git="${c_red}$git_branch${c_reset}"
            else
                local git="$git_branch"
            fi
        else
            local git=""
        fi

        if [ -n "$venv" -a -n "$git" ]
        then
            local info=" [$venv:$git]"
        elif [ -n "$venv" ]
        then
            local info=" ($venv)"
        elif [ -n "$git" ]
        then
            local info=" [$git]"
        else
            local info=""
        fi

        retval='<$?>'
        hostpath='\h:\w'
        prompt='\$'
        PS1="$c_blue$retval$hostpath$info$c_blue$prompt$c_reset "
    }
fi
