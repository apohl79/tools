#!/bin/sh

resolve_bin() {
  case "$1" in
    /*) printf '%s\n' "$1"; return 0 ;;
  esac
  for prefix in /opt/homebrew/bin /usr/local/bin /bin; do
    if [ -x "$prefix/$1" ]; then
      printf '%s/%s\n' "$prefix" "$1"
      return 0
    fi
  done
  printf '%s\n' "$1"
}

emacs_bin="${EMACS_BIN:-$(resolve_bin emacs)}"
emacsclient_bin="${EMACSCLIENT_BIN:-$(resolve_bin emacsclient)}"

detect_mode() {
  if [ -n "${MY_PROJECTS_MODE:-}" ] && [ -n "${EMACS_DAEMON_NAME:-}" ]; then
    return 0
  fi
  if [ -n "${CMUX_SOCKET_PATH:-}" ]; then
    : "${MY_PROJECTS_MODE:=cmux}"
    : "${EMACS_DAEMON_NAME:=cmux}"
  elif [ -n "${WEZTERM_PANE:-}" ]; then
    : "${MY_PROJECTS_MODE:=wezterm}"
    : "${EMACS_DAEMON_NAME:=wezterm}"
  else
    : "${MY_PROJECTS_MODE:=wezterm}"
    : "${EMACS_DAEMON_NAME:=default}"
  fi
  export MY_PROJECTS_MODE EMACS_DAEMON_NAME
}

detect_mode

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
      "$emacsclient_bin" -e '(kill-emacs)' 2>/dev/null && echo "Emacs daemon killed." || echo "No daemon running."
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
if [ "$no_client" -eq 1 ]; then
  "$emacs_bin" $nw_flag "$@"
else
  "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag "$@" 2>/dev/null || {
    "$emacs_bin" --daemon="$EMACS_DAEMON_NAME"
    "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag "$@"
  }
fi
