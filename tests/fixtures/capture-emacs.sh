#!/bin/sh
# Fake emacs binary. Records argv as a single tab-separated line in $CAPTURE_FILE.
: "${CAPTURE_FILE:?CAPTURE_FILE must be set}"
{
  printf 'emacs'
  for arg in "$@"; do
    printf '\t%s' "$arg"
  done
  printf '\n'
} >> "$CAPTURE_FILE"
exit 0
