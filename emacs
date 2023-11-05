#!/bin/sh
if [ "$(uname)" = "Darwin" ]; then
    emacsclient -c "$@" || (/opt/homebrew/bin/emacs "$@")
else
    /bin/emacs "$@"
fi
