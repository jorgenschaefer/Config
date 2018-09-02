# Combined bash configuration file, sourced by both bashrc and
# bash_profile.

export PATH="$HOME/.local/bin:${PATH}"

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

export EDITOR=vi

if [ -f "$HOME/.email" ]
then
    export EMAIL=$(cat "$HOME/.email")
fi
export HISTCONTROL=ignoredups
export LESS="-MIRnX"
export LESSCHARSET=utf-8
export SCREENDIR="$HOME/.cache/screen"

export PYTHONSTARTUP="$HOME/Projects/Config/pythonrc.py"
unset PYTHONDONTWRITEBYTECODE

# Utterly ridiculous Debian whackery:
# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=852398
export CHROMIUM_FLAGS="--enable-remote-extensions"

export PROJECT_HOME="$HOME/Projects"

alias rot13='tr a-zA-Z n-za-mN-ZA-M'
alias mv='mv -i'
alias %='fg'
alias sc="screen -rd"
alias be="bundle exec"
alias fr="bundle exec foreman run"

if type virtualenvwrapper.sh &>/dev/null
then
    if [ -z "$VIRTUALENVWRAPPER_PYTHON" ]
    then
        export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
    fi
    . virtualenvwrapper.sh
fi

if [ -f "/etc/bash_completion" ]
then
    . /etc/bash_completion
fi

if [ -f "$HOME/.virtualenvs/aws/bin/aws_bash_completer" ]
then
    . "$HOME/.virtualenvs/aws/bin/aws_bash_completer"
fi

if [ -d "$HOME/Programs/pyenv" ]
then
    export PYENV_ROOT="$HOME/Programs/pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    # eval "$(pyenv init -)"
fi

if [ -d "$HOME/Programs/go" ]
then
    export GOROOT="$HOME/Programs/go"
    export PATH="$GOROOT/bin:$PATH"
fi

if [ -d "$HOME/.local/gopath/" ]
then
    export GOPATH="$HOME/.local/gopath/"
    export PATH="$GOPATH/bin:$PATH"
fi

if [ -d "$HOME/.nvm" ]
then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
fi

if [ -d "$HOME/.rbenv" ]
then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

if [ -d "/usr/lib/chromium" ]
then
    export PATH="/usr/lib/chromium:$PATH"
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

    # A workon command for generic projects
    function wo () {
        local name="$1"
        if [ -z "$name" ]
        then
            echo "usage: wo <project>" >&2
            echo >&2
            echo "Possible projects:" >&2
            echo >&2
            /bin/ls "$HOME/Projects" >&2
        fi
        if [ ! -d "$HOME/Projects/$name" ]
        then
            echo "No such project: $name" >&2
            return 1
        fi
        cd "$HOME/Projects/$name"
        if [ -f ".env" ]
        then
            set -a
            . .env
            set +a
        fi
        if [ -d "$HOME/.virtualenvs/$name" ]
        then
            workon "$name"
        fi
    }
    complete -o default -o nospace -F _projects wo
    function _projects () {
        COMPREPLY=($(compgen -W "`/bin/ls "$HOME/Projects"`" -- "$2"))
    }

    # And now, the prompt
    if type tput &>/dev/null && tput setaf 1 >&/dev/null
    then
        c_reset="\\[`tput sgr0`\\]"
        c_bold="\\[`tput bold`\\]"
        c_underline="\\[`tput smul`\\]"
        c_nounderline="\\[`tput rmul`\\]"
        c_reverse="\\[`tput rev`\\]"
        c_standout="\\[`tput smso`\\]"
        c_nostandout="\\[`tput rmso`\\]"
        c_black="\\[`tput setaf 0`\\]"
        c_red="\\[`tput setaf 1`\\]"
        c_green="\\[`tput setaf 2`\\]"
        c_yellow="\\[`tput setaf 3`\\]"
        c_blue="\\[`tput setaf 4`\\]"
        c_magenta="\\[`tput setaf 5`\\]"
        c_cyan="\\[`tput setaf 6`\\]"
        c_white="\\[`tput setaf 7`\\]"
        c_bgblack="\\[`tput setab 0`\\]"
        c_bgred="\\[`tput setab 1`\\]"
        c_bggreen="\\[`tput setab 2`\\]"
        c_bgyellow="\\[`tput setab 3`\\]"
        c_bgblue="\\[`tput setab 4`\\]"
        c_bgmagenta="\\[`tput setab 5`\\]"
        c_bgcyan="\\[`tput setab 6`\\]"
        c_bgwhite="\\[`tput setab 7`\\]"
    else
        c_reset=""
        c_bold=""
        c_underline=""
        c_nounderline=""
        c_reverse=""
        c_standout=""
        c_nostandout=""
        c_black=""
        c_red=""
        c_green=""
        c_yellow=""
        c_blue=""
        c_magenta=""
        c_cyan=""
        c_white=""
        c_bgblack=""
        c_bgred=""
        c_bggreen=""
        c_bgyellow=""
        c_bgblue=""
        c_bgmagenta=""
        c_bgcyan=""
        c_bgwhite=""
    fi

    export PROMPT_COMMAND=set_prompt
    function set_prompt () {
        # Store this for later
        local last_exit="$?"

        local cwd_info=""
        if [[ "$USER" != 'forcer' && "$USER" != 'schaefer' && "$USER" != 'jorgen' ]]
        then
            # If we are not our usual users, add the username
            cwd_info='\u@\h:\w'
        elif [[ -n "$SSH_CONNECTION" ]]
        then
            # If not, but we are logged in remotely, use the host name
            cwd_info='\h:\w'
        else
            # Otherwise, working directory only
            cwd_info='\w'
        fi

        local ret_info=""
        if [[ "$last_exit" != 0 ]]
        then
            ret_info="$last_exit"
        fi

        local venv_info=""
        if [[ -n "$VIRTUAL_ENV" ]]
        then
            venv_info="$(basename "$VIRTUAL_ENV")"
        fi

        local git_dirty_info=""
        local git_clean_info=""
        if git rev-parse --git-dir > /dev/null 2>&1
        then
            local git_branch=$(git branch 2>/dev/null | sed -ne 's/^\* //p')
            if [ $(git status --porcelain 2>/dev/null | wc -l) -gt 0 ]
            then
                git_dirty_info="$git_branch"
                git_clean_info=""
            else
                git_dirty_info=""
                git_clean_info="$git_branch"
            fi
        fi

        local na_docker_info=""
        if [ -n "$DOCKER_HOST" ]
        then
            local na_docker_host
            local na_docker_env="$(echo "$DOCKER_HOST" | awk -F. '{print $3}')"
            if [[ "$DOCKER_HOST" == *'swarm'* ]]
            then
                na_docker_host=swarm
            else
                na_docker_host="$(echo "$DOCKER_HOST" | sed -e 's,tcp://\([^.]*\)\..*,\1,')"
            fi
            na_docker_info="${na_docker_env}@${na_docker_host}"
        fi


        local info_line=""
        function add_info () {
            local col="$1"
            local name="$2"
            local val="$3"
            if [[ -n "$val" ]]
            then
                local this_line="$col$c_bold$name$c_reset$col:$val$c_reset"
                info_line="$info_line $this_line"
            fi
        }
        add_info "$c_red" ret "$ret_info"
        add_info "$c_blue" venv "$venv_info"
        add_info "$c_blue" git "$git_clean_info"
        add_info "$c_magenta" git "$git_dirty_info"
        add_info "$c_blue" docker "$na_docker_info"

        if [[ -n "$info_line" ]]
        then
            local line1="$c_reset[${info_line# }]$c_reset"
            local line2="$c_blue$cwd_info$c_reset"
            local line3="$c_blue\\\$$c_reset "
            PS1="$c_reset\n$line1\n$line2\n$line3"
        else
            local line1="$c_blue$cwd_info$c_reset"
            local line2="$c_blue\\\$$c_reset "
            PS1="$c_reset\n$line1\n$line2"
        fi
    }
fi
