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

if [ ! -e $HOME/.oh-my-zsh/oh-my-zsh.sh ]; then
    rm -rf $HOME/.oh-my-zsh
    rm -rf /etc/oh-my-zsh
    echo "installing oh-my-zsh..."
    export RUNZSH=no
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    echo "installing powerlevel10k..."
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
else
    echo "skipping oh-my-zsh"
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
    if [ ! -d $HOME/.config ]; then
        mkdir $HOME/.config
    fi
    link "doom" $dir/.doom.d $HOME/.config/doom
else
    link "emacs" $dir/.emacs $HOME/.emacs
fi

if [ "$(uname)" = "Darwin" ]; then
    if ! which -s brew; then
        echo "installing homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    if [ "$DOOM" = "1" ]; then
        if ! which -s emacs; then
            echo "installing emacs..."
            brew tap railwaycat/emacsmacport
            brew install emacs-mac --with-starter --with-native-compilation
        fi
        if [ ! -e $HOME/.config/emacs ]; then
            echo "installing doom emacs..."
            brew install git ripgrep coreutils fd fontconfig shellcheck isort pipenv markdown jq
            git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
            export PATH=/opt/homebrew/bin:$PATH
            $HOME/.config/emacs/bin/doom install
            $HOME/.config/emacs/bin/doom sync
        fi
    fi

    echo "installing environment..."
    brew install cmake || true
    brew install python3 || true
    brew install conan@1 || true
    brew install clang-format || true
    brew install llvm || true
    brew install jenv || true
    brew install libtool || true
    brew install oracle-jdk || true
    brew install alfred || true
    brew install contexts || true
    brew install deepl || true
    brew install hammerspoon || true
    brew install keepassxc || true
    brew install menumeters || true
    brew install owncloud || true
    brew install signal || true
    brew install whatsapp || true
    brew install brave-browser || true
    brew install cryptomator || true
    brew install iterm2 || true
    brew install mailspring || true
    brew install rectangle || true
    brew install spotify || true
    brew install htop || true
    brew install packages || true
    brew install tor-browser || true
    brew install dos2unix || true
    brew install ilok-license-manager || true
    brew install mosquitto || true
    brew install arduino-ide || true
    brew install utm || true
    brew install cliclick || true
    brew install balenaetcher || true
    brew install pyright || true
    brew install gh || true

    pip3 install paho-mqtt || true
fi
