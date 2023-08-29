#!/bin/sh
emacsclient -c "$@" || (/opt/homebrew/bin/emacs "$@")
