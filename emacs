#!/bin/sh
bin=/opt/homebrew/bin
if [ ! -x $bin/emacs ]; then
  bin=/usr/local/bin
fi
if [ ! -x $bin/emacs ]; then
  bin=/bin
fi
no_client=0
if [ "$1" = "-nc" ]; then
    no_client=1
    shift
fi
if [ "$(uname)" = "Darwin" ]; then
    if [ $no_client -eq 0 ]; then
        $bin/emacsclient -c "$@" || ($bin/emacs "$@")
    else
        $bin/emacs "$@"
    fi
else
    $bin/emacs "$@"
fi
