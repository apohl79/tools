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

trace() {
  [ -n "${EMACS_WRAPPER_DEBUG:-}" ] || return 0
  {
    printf '[emacs wrapper] +'
    for arg in "$@"; do
      printf ' %s' "$arg"
    done
    printf '\n'
  } >&2
}

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
      trace "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -e '(kill-emacs)'
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
  # Pass the caller's cwd as a frame parameter so the daemon's
  # `projects-cmux--frame-init` hook can look up the matching project
  # and open its info buffer. Quoting backslashes survive emacsclient's
  # `-F ALIST' parser; the value is a Lisp string literal.
_cwd="$(pwd -P)"
_cwd_escaped=$(printf '%s' "$_cwd" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
_frame_params="((cmux-cwd . \"$_cwd_escaped\"))"

ensure_cmux_project_for_cwd() {
  # If the cmux daemon socket is reachable, ensure a project exists for
  # the cwd so the daemon's `projects-cmux--frame-init` hook can switch
  # the new frame to it. Probing the socket directly is more robust
  # than gating on MY_PROJECTS_MODE (which can be unset in nested
  # shells) — the call is a no-op when the helper isn't bound.
  "$emacsclient_bin" -s cmux -e t >/dev/null 2>&1 || return 0
  _ensure_form="(when (fboundp 'projects-cmux-ensure-project-for-cwd) (projects-cmux-ensure-project-for-cwd \"$_cwd_escaped\"))"
  trace "$emacsclient_bin" -s cmux -e "$_ensure_form"
  "$emacsclient_bin" -s cmux -e "$_ensure_form" >/dev/null 2>&1 || true
}

if [ "$no_client" -eq 1 ]; then
  trace "$emacs_bin" $nw_flag "$@"
  "$emacs_bin" $nw_flag "$@"
else
  ensure_cmux_project_for_cwd
  trace "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag -F "$_frame_params" "$@"
  "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag -F "$_frame_params" "$@" 2>/dev/null || {
    trace "$emacs_bin" --daemon="$EMACS_DAEMON_NAME"
    "$emacs_bin" --daemon="$EMACS_DAEMON_NAME"
    ensure_cmux_project_for_cwd
    trace "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag -F "$_frame_params" "$@"
    "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag -F "$_frame_params" "$@"
  }
fi
