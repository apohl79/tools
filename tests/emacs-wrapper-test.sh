#!/usr/bin/env bash
# tests/emacs-wrapper-test.sh — drives tools/emacs with mock binaries.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WRAPPER="$REPO_ROOT/emacs"
FIXTURES="$REPO_ROOT/tests/fixtures"

fail=0
pass=0

run_case() {
  local name="$1"; shift
  local expected_pattern="$1"; shift
  local capture; capture="$(mktemp)"
  (
    export EMACS_BIN="$FIXTURES/capture-emacs.sh"
    export EMACSCLIENT_BIN="$FIXTURES/capture-emacsclient.sh"
    export CAPTURE_FILE="$capture"
    "$@"
  ) >/dev/null 2>&1 || true
  if grep -qE "$expected_pattern" "$capture"; then
    pass=$((pass+1)); printf 'PASS %s\n' "$name"
  else
    fail=$((fail+1))
    printf 'FAIL %s\n  expected match: %s\n  got:\n' "$name" "$expected_pattern"
    sed 's/^/    /' "$capture"
  fi
  rm -f "$capture"
}

# Cases will be added in later tasks.

# Baseline: with EMACS_BIN set, daemon-start path uses our fake emacs.
run_case 'baseline emacs --daemon used' \
  '^emacs(\t--daemon(=[a-z]+)?)?$' \
  env EMACSCLIENT_EXIT=1 "$WRAPPER" --version

# Baseline: emacsclient is invoked when not -nc.
run_case 'baseline emacsclient invoked' \
  '^emacsclient' \
  env EMACSCLIENT_EXIT=0 "$WRAPPER" --version

run_case 'cmux env selects cmux daemon' \
  '^emacs(\t--daemon=cmux)' \
  env CMUX_SOCKET_PATH=/tmp/cmux.sock EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case 'cmux env exports MY_PROJECTS_MODE=cmux to client' \
  $'^emacsclient(\t.+)?\t-s\tcmux(\t|$)' \
  env CMUX_SOCKET_PATH=/tmp/cmux.sock EMACSCLIENT_EXIT=0 "$WRAPPER" --version

run_case 'wezterm env selects wezterm daemon' \
  '^emacs\t--daemon=wezterm' \
  env -u CMUX_SOCKET_PATH WEZTERM_PANE=2 EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case 'no env selects default daemon' \
  '^emacs\t--daemon=default' \
  env -u CMUX_SOCKET_PATH -u WEZTERM_PANE EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case 'env override wins' \
  '^emacs\t--daemon=cmux' \
  env -u CMUX_SOCKET_PATH -u WEZTERM_PANE EMACS_DAEMON_NAME=cmux MY_PROJECTS_MODE=cmux EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case '-kill targets named daemon' \
  $'^emacsclient\t-s\tcmux\t-e\t\\(kill-emacs\\)' \
  env CMUX_SOCKET_PATH=/tmp/cmux.sock EMACSCLIENT_EXIT=0 "$WRAPPER" -kill

# F8: invalid EMACS_DAEMON_NAME with slash is rejected.
(
  capture="$(mktemp)"
  rc=0
  env -u CMUX_SOCKET_PATH -u WEZTERM_PANE EMACSCLIENT_EXIT=0 \
    EMACS_DAEMON_NAME=/tmp/attacker.sock MY_PROJECTS_MODE=cmux \
    EMACS_BIN="$FIXTURES/capture-emacs.sh" \
    EMACSCLIENT_BIN="$FIXTURES/capture-emacsclient.sh" \
    CAPTURE_FILE="$capture" "$WRAPPER" --version >/dev/null 2>&1 || rc=$?
  if [ "$rc" = "64" ]; then
    pass=$((pass+1)); printf 'PASS F8 invalid daemon name rejected (exit 64)\n'
  else
    fail=$((fail+1)); printf 'FAIL F8 invalid daemon name rejected (got exit %d)\n' "$rc"
  fi
  rm -f "$capture"
)

# F8: empty EMACS_DAEMON_NAME is rejected.
(
  capture="$(mktemp)"
  rc=0
  env -u CMUX_SOCKET_PATH -u WEZTERM_PANE EMACSCLIENT_EXIT=0 \
    EMACS_DAEMON_NAME= MY_PROJECTS_MODE=cmux \
    EMACS_BIN="$FIXTURES/capture-emacs.sh" \
    EMACSCLIENT_BIN="$FIXTURES/capture-emacsclient.sh" \
    CAPTURE_FILE="$capture" "$WRAPPER" --version >/dev/null 2>&1 || rc=$?
  if [ "$rc" = "64" ]; then
    pass=$((pass+1)); printf 'PASS F8 empty daemon name rejected (exit 64)\n'
  else
    fail=$((fail+1)); printf 'FAIL F8 empty daemon name rejected (got exit %d)\n' "$rc"
  fi
  rm -f "$capture"
)

if [ "$fail" -gt 0 ]; then
  printf '%d failed, %d passed\n' "$fail" "$pass"
  exit 1
fi
printf 'all %d passed\n' "$pass"
