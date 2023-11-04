#!/bin/sh
scriptname=$0
dir=$(readlink $0|sed "s,/$scriptname,,")

link() {
    if [ ! -e $3 ]; then
        echo "linking $1 ($2 -> $3)"
        ln -sf $2 $3
    else
        echo "skipping $1 (exists)"
    fi
}

link_force() {
    if [ -e $3 ]; then
        rm $3
    fi
    link $1 $2 $3
}

if [ -z $dir ]; then
    dir=$(pwd)
fi
cd $HOME
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin
fi

if [ ! -e $HOME/.oh-my-zsh ]; then
    echo "installing oh-my-zsh..."
    export RUNZSH=no
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    echo "installing powerlevel10k..."
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
fi

link "tools" $dir $HOME/bin/tools
link_force "zsh" $dir/.zshrc $HOME/.zshrc
link_force "powerlevel10k" $dir/.p10k.zsh $HOME/.p10k.zsh
link_force "profile" $HOME/.zshrc $HOME/.profile
link "clang-format" $dir/.clang-format $HOME/.clang-format

echo "choose emacs setup:"
echo " 1) doom"
echo " 2) light"
read DOOM
if [ "$DOOM" = "1" ]; then
    link "doom" $dir/.doom.d $HOME/.config/doom
else
    link "emacs" $dir/.emacs $HOME/
fi

if [ "$(uname)" == "Darwin" ]; then
    if ! /usr/bin/which -s brew; then
        echo "installing homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    if [ "$DOOM" = "1" ]; then
        if ! /usr/bin/which -s emacs; then
            echo "installing emacs..."
            brew tap railwaycat/emacsmacport
            brew install emacs-mac --with-starter
        fi
        if [ ! -e $HOME/.config/emacs ]; then
            echo "installing doom emacs..."
            brew install git ripgrep coreutils fd
            git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
            export PATH=/opt/homebrew/bin:$PATH
            $HOME/.config/emacs/bin/doom install
            $HOME/.config/emacs/bin/doom sync
        fi
    fi

    echo "installing environment..."
    brew install cmake python3 conan@1 clang-format jenv libtool oracle-jdk alfred contexts deepl hammerspoon keepassxc menumeters \
        owncloud signal brave-browser cryptomator fuse-t iterm2 mailspring rectangle spotify htop packages tor-browser dos2unix \
        ilok-license-manager mosquitto arduino-ide utm cliclick
    pip3 install python-lsp-server paho-mqtt
fi
