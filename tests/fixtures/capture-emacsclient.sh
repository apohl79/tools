#!/bin/sh
# Fake emacsclient. Records argv and exits with the value of $EMACSCLIENT_EXIT
# (default 0). Tests set EMACSCLIENT_EXIT=1 to force the wrapper into the
# fallback "start daemon then connect" branch.
: "${CAPTURE_FILE:?CAPTURE_FILE must be set}"
printf 'emacsclient'
for arg in "$@"; do
  printf '\t%s' "$arg"
done
printf '\n' >> "$CAPTURE_FILE"
exit "${EMACSCLIENT_EXIT:-0}"
