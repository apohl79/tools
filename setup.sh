#!/bin/sh
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin
fi
if [ ! -e $HOME/bin/tools ]; then
    ln -s $(pwd) $HOME/bin/tools
fi
ln -s $(pwd)/.emacs $HOME/
ln -s $(pwd)/.bashrc $HOME/
ln -sf $HOME/.bashrc $HOME/.profile
