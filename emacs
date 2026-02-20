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
    -kill)
      $bin/emacsclient -e '(kill-emacs)' 2>/dev/null && echo "Emacs daemon killed." || echo "No daemon running."
      exit 0
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
if [ $no_client -eq 1 ]; then
  $bin/emacs $nw_flag "$@"
else
  # Try connecting to existing daemon, start one if needed
  $bin/emacsclient -c $nw_flag "$@" 2>/dev/null || {
    $bin/emacs --daemon
    $bin/emacsclient -c $nw_flag "$@"
  }
fi
