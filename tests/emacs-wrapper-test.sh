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

if [ "$fail" -gt 0 ]; then
  printf '%d failed, %d passed\n' "$fail" "$pass"
  exit 1
fi
printf 'all %d passed\n' "$pass"
