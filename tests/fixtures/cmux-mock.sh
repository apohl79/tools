#!/bin/sh
# Fake cmux. Records argv to $CAPTURE_FILE and prints a stub line to stdout.
: "${CAPTURE_FILE:?CAPTURE_FILE must be set}"
{
  printf 'cmux'
  for arg in "$@"; do
    printf '\t%s' "$arg"
  done
  printf '\n'
} >> "$CAPTURE_FILE"
case "$1" in
  ping) exit 0 ;;
  list-workspaces) printf 'workspace:1\tname=foo\n' ;;
  current-workspace) printf 'workspace:1\tname=foo\n' ;;
esac
exit 0
