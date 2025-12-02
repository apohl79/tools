# SSH agent, required from 10.12 on
# PS1_OATH defined via vzm-env in .bashrc_local
if [ -z "$WORK_SESSION" ]; then
    #export SSH_AUTH_SOCK=$HOME/.yubiagent/sock
    if [ -e $HOME/.ssh/id_rsa ]; then
        ssh-add --apple-use-keychain
    fi
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="robbyrussell"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration
unsetopt share_history

export LANG=C
export EDITOR=emacs

export HOMEBREW_NO_ENV_HINTS=1

if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    export HOMEBREW_NO_EMOJI=1
    export HOMEBREW_NO_AUTO_UPDATE=1
    export PATH=$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH
    export MANPATH=$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH
    export XML_CATALOG_FILES=/usr/local/etc/xml/catalog
fi

export PATH=$HOME/bin:$HOME/bin/tools/ccscript:$HOME/bin/tools:$HOME/.local/bin:$HOME/.config/emacs/bin:/usr/local/bin:/usr/local/sbin:$HOME/.claude/local:$PATH

alias ll='ls -la --color=auto'

zstyle ':completion:*' rehash true

if [ -e $HOME/.zshrc_local ]; then
    . $HOME/.zshrc_local
fi

if [ "$(uname)" = "Darwin" ]; then
    bindkey "[D" backward-word
    bindkey "[C" forward-word
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PATH="/Users/andreas/.codeium/windsurf/bin:$PATH"

alias git-branch-local-cleanup='git fetch --prune; git branch -vv | grep ": gone]" | awk "{print \$1}" | xargs git branch -D'
alias cc='claude update && claude "read ./CLAUDE.md carefully. make sure you follow all of the defined rules and settings for code style, comments, testing, the development process and diagnostics like using an lsp via mcp when available. use you your builtin read tool to read file instead of using an mcp."'

export VCPKG_ROOT="$HOME/vcpkg"
export GPG_TTY=$(tty)

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

source ~/.safe-chain/scripts/init-posix.sh # Safe-chain Zsh initialization script
