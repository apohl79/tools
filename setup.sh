#!/bin/sh
scriptname=$0
dir=$(readlink $0|sed "s,/$scriptname,,")
if [ -z $dir ]; then
    dir=$(pwd)
fi
cd $HOME
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin
fi
if [ ! -e $HOME/bin/tools ]; then
    ln -s $dir $HOME/bin/tools
fi
ln -sf $dir/.zshrc $HOME/
#ln -sf $dir/.bashrc $HOME/
ln -sf $HOME/.zshrc $HOME/.profile
ln -sf $dir/.clang-format $HOME/

echo "choose emacs setup:"
echo " 1) doom"
echo " 2) light"
read X
if [ "$X" = "1" ]; then
    echo "linking doom config (note: this is not installing doom emacs)"
    ln -sf $dir/.doom.d $HOME/
else
    echo "linking light weight config"
    ln -sf $dir/.emacs $HOME/
fi
