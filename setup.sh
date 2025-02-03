#!/bin/bash
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
cd "$HOME" || exit
if [ ! -d "$HOME/bin" ]; then
    mkdir "$HOME/bin"
fi

if [ ! -e "$HOME/.oh-my-zsh/oh-my-zsh.sh" ]; then
    rm -rf "$HOME/.oh-my-zsh"
    rm -rf /etc/oh-my-zsh
    echo "installing oh-my-zsh..."
    export RUNZSH=no
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    echo "installing powerlevel10k..."
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
else
    echo "skipping oh-my-zsh"
fi

link "tools" "$dir" "$HOME/bin/tools"
link_force "zsh" "$dir/.zshrc" "$HOME/.zshrc"
link_force "powerlevel10k" "$dir/.p10k.zsh" "$HOME/.p10k.zsh"
link_force "profile" "$HOME/.zshrc" "$HOME/.profile"
link "clang-format" "$dir/.clang-format" "$HOME/.clang-format"

echo "choose emacs setup:"
echo " 1) doom"
echo " 2) light"
echo " 3) skip"
read DOOM
if [ "$DOOM" = "1" ]; then
    if [ ! -d "$HOME/.config" ]; then
        mkdir "$HOME/.config"
    fi
    link "doom" "$dir/.doom.d" "$HOME/.config/doom"
elif [ "$DOOM" = "2" ]; then
    link "emacs" "$dir/.emacs" "$HOME/.emacs"
else
    echo "skipping emacs"
fi

if [ "$(uname)" = "Darwin" ]; then
    if ! which -s brew; then
        echo "installing homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    if [ "$DOOM" = "1" ]; then
        brew install nvm || true
        nvm install 20
        nvm use 20

        if ! which -s emacs; then
            echo "installing emacs..."
            brew install jansson libxml2 tree-sitter imagemagick librsvg python3
            # lsp-bridge deps
            pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
            # language servers
            brew install basedpyright ruff || true
            npm install -g yaml-language-server
            # emacs
            brew tap railwaycat/emacsmacport
            brew install emacs-mac --with-starter --with-native-compilation --with-natural-title-bar --with-imagemagick --with-librsvg --with-xwidgets --with-glib
            defaults write org.gnu.Emacs TransparentTitleBar DARK
        fi
        if [ ! -e "$HOME/.config/emacs" ]; then
            echo "installing doom emacs..."
            brew install git ripgrep coreutils fd fontconfig font-iosevka-comfy font-roboto shellcheck isort pipenv markdown jq hunspell enchant
            git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
            export PATH=/opt/homebrew/bin:"$PATH"
            "$HOME/.config/emacs/bin/doom" install
            "$HOME/.config/emacs/bin/doom" sync
            echo "installing dictionaries..."
            wget -nc https://cgit.freedesktop.org/libreoffice/dictionaries/plain/de/de_DE_frami.aff -O ~/Library/Spelling/de_DE_frami.aff
            wget -nc https://cgit.freedesktop.org/libreoffice/dictionaries/plain/de/de_DE_frami.dic -O ~/Library/Spelling/de_DE_frami.dic
            wget -nc wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff -O ~/Library/Spelling/en_US.aff
            wget -nc wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic -O ~/Library/Spelling/en_US.dic
        fi
    fi

    echo -n "install dev ennvironment and tools? [Y/n]: "
    read INST

    if [ -z "$INST" ] || [ "$INST" = "y" ] || [ "$INST" = "Y" ]; then
        echo "installing environment..."
        brew install owncloud keepassxc cryptomator || true
        brew install wezterm llvm clang-format cmake python3 libtool gh nvm htop packages dos2unix mosquitto || true
        brew install oracle-jdk jenv || true
        brew install rectangle raycast contexts hammerspoon menumeters deepl || true
        brew install signal whatsapp || true
        brew install arc brave-browser tor-browser || true
        npm install -g @georgesg/arc-cli
        brew install mailspring || true
        brew install spotify || true
        brew install ilok-license-manager || true
        brew install balenaetcher || true
        brew install arduino-ide || true
        brew install utm || true
        brew install cliclick || true
        pip3 install paho-mqtt || true
    fi
fi
