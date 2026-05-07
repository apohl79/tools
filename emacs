#!/bin/sh

resolve_bin() {
  case "$1" in
    /*) printf '%s\n' "$1"; return 0 ;;
  esac
  for prefix in /opt/homebrew/bin /usr/local/bin /usr/bin /bin; do
    if [ -x "$prefix/$1" ]; then
      printf '%s/%s\n' "$prefix" "$1"
      return 0
    fi
  done
  printf 'emacs wrapper: cannot find %s in /opt/homebrew/bin /usr/local/bin /usr/bin /bin (set EMACS_BIN to override)\n' "$1" >&2
  exit 127
}

if [ -n "${EMACS_BIN:-}" ]; then
  emacs_bin="$EMACS_BIN"
else
  emacs_bin="$(resolve_bin emacs)" || exit $?
fi
if [ -n "${EMACSCLIENT_BIN:-}" ]; then
  emacsclient_bin="$EMACSCLIENT_BIN"
else
  emacsclient_bin="$(resolve_bin emacsclient)" || exit $?
fi

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

_user_daemon_set="${EMACS_DAEMON_NAME+set}"
_user_daemon_value="${EMACS_DAEMON_NAME-}"

detect_mode

if [ "$_user_daemon_set" = "set" ]; then
  _validate_target="$_user_daemon_value"
else
  _validate_target="$EMACS_DAEMON_NAME"
fi

case "$_validate_target" in
  *[!A-Za-z0-9._-]*|'')
    printf 'emacs wrapper: invalid EMACS_DAEMON_NAME=%s (must match [A-Za-z0-9._-]+)\n' "$_validate_target" >&2
    exit 64
    ;;
esac
unset _user_daemon_set _user_daemon_value _validate_target

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
      "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -e '(kill-emacs)' 2>/dev/null \
        && echo "Emacs daemon '$EMACS_DAEMON_NAME' killed." \
        || echo "No daemon '$EMACS_DAEMON_NAME' running."
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
