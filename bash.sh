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
        local c_reset="\\[`tput sgr0`\\]"
        local c_bold="\\[`tput bold`\\]"
        local c_underline="\\[`tput smul`\\]"
        local c_nounderline="\\[`tput rmul`\\]"
        local c_reverse="\\[`tput rev`\\]"
        local c_standout="\\[`tput smso`\\]"
        local c_nostandout="\\[`tput rmso`\\]"
        local c_black="\\[`tput setaf 0`\\]"
        local c_red="\\[`tput setaf 1`\\]"
        local c_green="\\[`tput setaf 2`\\]"
        local c_yellow="\\[`tput setaf 3`\\]"
        local c_blue="\\[`tput setaf 4`\\]"
        local c_magenta="\\[`tput setaf 5`\\]"
        local c_cyan="\\[`tput setaf 6`\\]"
        local c_white="\\[`tput setaf 7`\\]"
        local c_bgblack="\\[`tput setab 0`\\]"
        local c_bgred="\\[`tput setab 1`\\]"
        local c_bggreen="\\[`tput setab 2`\\]"
        local c_bgyellow="\\[`tput setab 3`\\]"
        local c_bgblue="\\[`tput setab 4`\\]"
        local c_bgmagenta="\\[`tput setab 5`\\]"
        local c_bgcyan="\\[`tput setab 6`\\]"
        local c_bgwhite="\\[`tput setab 7`\\]"
    else
        local c_reset=""
        local c_bold=""
        local c_underline=""
        local c_nounderline=""
        local c_reverse=""
        local c_standout=""
        local c_nostandout=""
        local c_black=""
        local c_red=""
        local c_green=""
        local c_yellow=""
        local c_blue=""
        local c_magenta=""
        local c_cyan=""
        local c_white=""
        local c_bgblack=""
        local c_bgred=""
        local c_bggreen=""
        local c_bgyellow=""
        local c_bgblue=""
        local c_bgmagenta=""
        local c_bgcyan=""
        local c_bgwhite=""
    fi

    export PROMPT_COMMAND=set_prompt
    function set_prompt () {
        local last_exit="$?"

        local dir_line=''
        if [[ "$USER" != 'forcer' && "$USER" != 'schaefer' ]]
        then
            # If we are not our usual users, add the username
            dir_line='\u@\h:\w'
        elif [[ -n "$SSH_CONNECTION" ]]
        then
            # If not, but we are logged in remotely, use the host
            # name only
            dir_line='\h:\w'
        else
            dir_line='\w'
        fi

        local info_line=""
        if [[ "$last_exit" != 0 ]]
        then
            info_line="$info_line|ret:$last_exit"
        fi
        if [[ -n "$VIRTUAL_ENV" ]]
        then
            info_line="$info_line|venv:$(basename "$VIRTUAL_ENV")"
        fi
        if git rev-parse --git-dir > /dev/null 2>&1
        then
            local git_branch=$(git branch 2>/dev/null | sed -ne 's/^\* //p')
            if [ $(git status --porcelain 2>/dev/null | wc -l) -gt 0 ]
            then
                local git="${c_red}$git_branch${c_default}"
            else
                local git="$git_branch"
            fi
            info_line="$info_line|git:$git"
        fi
        if [[ -n "$info_line" ]]
        then
            info_line="[${info_line#?}]"
        fi
        PS1="$c_bold$c_default$dir_line$c_reset\n$c_default$info_line\$${c_reset} "
    }
fi
