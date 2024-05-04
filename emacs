#!/bin/sh
no_client=0
if [ "$1" = "-nc" ]; then
    no_client=1
    shift
fi
if [ "$(uname)" = "Darwin" ]; then
    if [ $no_client -eq 0 ]; then
        emacsclient -c "$@" || (/opt/homebrew/bin/emacs "$@")
    else
        /opt/homebrew/bin/emacs "$@"
    fi
else
    /bin/emacs "$@"
fi
