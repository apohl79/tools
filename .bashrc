export PATH=$HOME/bin:$HOME/bin/tools/sshtools:$HOME/bin/tools:$HOME/.sshsessions:/usr/local/bin:/usr/local/sbin:/opt/433ctrl/scripts:$PATH

IS_TTY=1
if [ -n "$(which tty)" ]; then
    tty -s
    IS_TTY=$?
fi

#echo "IS_TTY = $IS_TTY"
#echo "BASH_VERSION = $BASH_VERSION"

# Check if running bash
if [ $IS_TTY == 0 ] && [ -n "$BASH_VERSION" ]; then
    # don't put duplicate lines in the history. See bash(1) for more options
    # don't overwrite GNU Midnight Commander's setting of `ignorespace'.
    export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
    # ... or force ignoredups and ignorespace
    export HISTCONTROL=ignoreboth

    # append to the history file, don't overwrite it
    shopt -s histappend

    # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    export LANG=C
    export EDITOR=emacs

    # set a fancy prompt (non-color, unless we know we "want" color)
    export TERM=xterm-color

    # git support
    . $HOME/tools/git-completion.bash
    . $HOME/tools/git-prompt.sh

    # Use colors anywhere else
    if [ -n "$INSIDE_EMACS" ]; then
        export INSIDE_EMACS
        if [ $(id -u) == 0 ]; then
            PS1_BASE='\[\033[02;31m\]\u\[\033[02;35m\]@\[\033[00m\]\[\033[01;34m\]\h\[\033[00m\]:\W'
        else
            PS1_BASE='\[\033[02;33m\]\u\[\033[02;35m\]@\[\033[00m\]\[\033[01;34m\]\h\[\033[00m\]:\W'
        fi
    else
        if [ $(id -u) == 0 ]; then
            PS1_BASE='\001\033[02;31m\002\u\001\033[02;35m\002@\001\033[00m\002\001\033[01;34m\002\h\001\033[00m\002:\001\033[01;30m\002\w\001\033[00m\002'
        else
            PS1_BASE='\001\033[02;33m\002\u\001\033[02;35m\002@\001\033[00m\002\001\033[01;34m\002\h\001\033[00m\002:\001\033[01;30m\002\w\001\033[00m\002'
        fi
    fi
    PS1="$PS1_OATH$PS1_BASE$ "

    # OS X home brew found, my dev box
    if [ -n "$(which brew)" ]; then
        if [ -f $(brew --prefix)/etc/bash_completion ]; then
            . $(brew --prefix)/etc/bash_completion
        fi
        export HOMEBREW_NO_EMOJI=1
        export HOMEBREW_NO_AUTO_UPDATE=1
        export PATH=$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH
        export MANPATH=$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH
        export XML_CATALOG_FILES=/usr/local/etc/xml/catalog

        # Colorfull prompt with git support
        GIT_PS1_SHOWDIRTYSTATE=1
        GIT_PS1_SHOWCOLORHINTS=1
        #PROMPT_COMMAND='__git_ps1 "\[\e]0;\h: \w\a\]$PS1_BASE" "\\\$ "'

        # SSH agent, required from 10.12 on
        # PS1_OATH defined via vzm-env in .bashrc_local
        if [ -z "$PS1_OATH" ]; then
            #export SSH_AUTH_SOCK=$HOME/.yubiagent/sock
            if [ -e $HOME/.ssh/id_rsa ]; then
                ssh-add -K
            fi
        fi
    else
        # Bash completion
        if [ -f /etc/bash_completion ]; then
            . /etc/bash_completion
        fi

        tty > /tmp/tty.x

        # SSH agent
        if [ -e $HOME/.ssh/id_rsa ]; then
            if [ ! -e /tmp/ssh-agent.env ]; then
                ssh-agent > /tmp/ssh-agent.env
                . /tmp/ssh-agent.env
                ssh-add
            else
                . /tmp/ssh-agent.env
            fi
        fi
    fi

    # make less more friendly for non-text input files, see lesspipe(1)
    [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

    # Try to make ls output colored
    ls --color >/dev/null 2>&1
    if [ $? == 0 ]; then
        alias ls='ls --color'
    else
        ls -G >/dev/null 2>&1
        if [ $? == 0 ]; then
            alias ls='ls -G'
        fi
    fi
    alias ll='ls -la'

    # Some android dev helpers
    if [ -n "$(which adb)" ]; then
        alias adbs='adb shell'
        alias adbk='adb kill-server'
        alias adbl='adb logcat'
        alias adbkl='adb shell cat /proc/kmsg'
        alias adbd='adb devices'
    fi

    # Some debian shortcuts
    if [ -e /etc/debian_version ]; then
        alias aptinstall='sudo apt-get install'
        alias aptsearch='aptitude search'
        alias aptupdate='sudo apt-get update'
        alias aptupgrade='sudo apt-get upgrade'
        alias aptremove='sudo apt-get remove; sudo apt-get autoremove'
    fi

    # misc
    alias mv_files_by_date='for f in *; do d=$(stat -t "%F" "$f"|awk -F\" "{print \$2}"); if [ ! -d $d ]; then mkdir $d; fi; mv "$f" $d/; done'
    alias ssh_tor="ssh -o ProxyCommand='nc -x 127.0.0.1:9150 %h %p'"
    export JAVA_HOME=/opt/java

    # local stuff
    if [ -e $HOME/.bashrc_local ]; then
        . $HOME/.bashrc_local
    fi
fi
