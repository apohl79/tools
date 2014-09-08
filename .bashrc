# If not running interactively, don't do anything
#[ -z "$PS1" ] && return

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

export PATH=$HOME/bin:$HOME/bin/tools/sshtools:$HOME/bin/tools:$HOME/.sshsessions:$HOME/android/tools:$HOME/android/adt/sdk/tools:$HOME/android/adt/sdk/platform-tools:/usr/local/bin:/usr/local/sbin:/opt/433ctrl/scripts:$PATH

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
export TERM=xterm-color

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

# Use colors anywhere else
if [ $(id -u) == 0 ]; then
    PS1_BASE='\[\033[02;31m\]\u\[\033[02;35m\]@\[\033[00m\]\[\033[02;34m\]\h\[\033[00m\]:\[\033[01;30m\]\w\[\033[00m\]'
else
    PS1_BASE='\[\033[02;33m\]\u\[\033[02;35m\]@\[\033[00m\]\[\033[02;34m\]\h\[\033[00m\]:\[\033[01;30m\]\w\[\033[00m\]'
fi
PS1=$PS1_BASE'$ '

# OS X home brew found, my dev box
if [ -n "$(which brew)" ]; then
    if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
        . $(brew --prefix)/share/bash-completion/bash_completion
    fi
    export HOMEBREW_NO_EMOJI=1
    export PATH=$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH
    export MANPATH=$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH
    export XML_CATALOG_FILES=/usr/local/etc/xml/catalog

    # Colorfull prompt with git support
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWCOLORHINTS=1
    PROMPT_COMMAND='__git_ps1 "\[\e]0;\h: \w\a\]$PS1_BASE" "\\\$ "'
else
    # Bash completion
    if [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi

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
