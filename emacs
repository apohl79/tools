#!/bin/sh
bin=/opt/homebrew/bin
if [ ! -x $bin/emacs ]; then
  bin=/usr/local/bin
fi
if [ ! -x $bin/emacs ]; then
  bin=/bin
fi
no_client=0
gui_mode=0
while [ $# -gt 0 ]; do
    case "$1" in
        -nc)
            no_client=1
            shift
            ;;
        -gui)
            gui_mode=1
            shift
            ;;
        *)
            break
            ;;
    esac
done
if [ $gui_mode -eq 1 ]; then
    nw_flag=""
else
    nw_flag="-nw"
fi
if [ "$(uname)" = "Darwin" ]; then
    if [ $no_client -eq 0 ]; then
        $bin/emacsclient -c $nw_flag "$@" || ($bin/emacs $nw_flag "$@")
    else
        $bin/emacs $nw_flag "$@"
    fi
else
    $bin/emacs $nw_flag "$@"
fi
