#!/bin/sh
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin
fi
if [ ! -e $HOME/bin/tools ]; then
    ln -s $(pwd) $HOME/bin/tools
fi
ln -sf $(pwd)/.emacs $HOME/
ln -sf $(pwd)/.bashrc $HOME/
ln -sf $HOME/.bashrc $HOME/.profile
ln -sf $(pwd)/.clang-format $HOME/
