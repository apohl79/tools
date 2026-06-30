#!/usr/bin/env bash
# tests/claude-cmux-tab-title-test.sh - verifies the Claude cmux tab-title hook.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HOOK="$REPO_ROOT/.claude/hooks/cmux-tab-title.sh"
FIXTURES="$REPO_ROOT/tests/fixtures"

fail=0
pass=0

with_env() {
  local capture="$1"; shift
  local tmp="$1"; shift

  mkdir -p "$tmp/bin"
  ln -sf "$FIXTURES/cmux-mock.sh" "$tmp/bin/cmux"

  env \
    PATH="$tmp/bin:$PATH" \
    CAPTURE_FILE="$capture" \
    HOOK="$HOOK" \
    "$@"
}

assert_capture() {
  local name="$1"; shift
  local expected_pattern="$1"; shift
  local capture; capture="$(mktemp)"
  local tmp; tmp="$(mktemp -d)"

  if with_env "$capture" "$tmp" "$@"; then
    if grep -qE "$expected_pattern" "$capture"; then
      pass=$((pass+1)); printf 'PASS %s\n' "$name"
    else
      fail=$((fail+1))
      printf 'FAIL %s\n  expected match: %s\n  got:\n' "$name" "$expected_pattern"
      sed 's/^/    /' "$capture"
    fi
  else
    fail=$((fail+1)); printf 'FAIL %s\n  hook command failed\n' "$name"
  fi

  rm -rf "$tmp"
  rm -f "$capture"
}

assert_empty_capture() {
  local name="$1"; shift
  local capture; capture="$(mktemp)"
  local tmp; tmp="$(mktemp -d)"

  if with_env "$capture" "$tmp" "$@"; then
    if [ ! -s "$capture" ]; then
      pass=$((pass+1)); printf 'PASS %s\n' "$name"
    else
      fail=$((fail+1))
      printf 'FAIL %s\n  expected no cmux call, got:\n' "$name"
      sed 's/^/    /' "$capture"
    fi
  else
    fail=$((fail+1)); printf 'FAIL %s\n  hook command failed\n' "$name"
  fi

  rm -rf "$tmp"
  rm -f "$capture"
}

run_custom_title_case() {
  local transcript; transcript="$(mktemp)"
  cat >"$transcript" <<'JSONL'
{"type":"custom-title","customTitle":"Old title"}
{"type":"custom-title","customTitle":"Claude rename"}
JSONL
  printf '{"session_id":"claude-session-1","transcript_path":"%s","cwd":"/Users/andreas/tools"}' "$transcript" |
    CMUX_SURFACE_ID=surface-1 "$HOOK"
  rm -f "$transcript"
}

run_session_id_fallback_case() {
  printf '%s' '{"session_id":"claude-session-2","transcript_path":null,"cwd":"/Users/andreas/tools"}' |
    CMUX_SURFACE_ID=surface-2 "$HOOK"
}

run_transcript_path_fallback_case() {
  local transcript; transcript="$(mktemp)"
  printf '%s' '{"type":"message","text":"hello"}' >"$transcript"
  printf '{"transcript_path":"%s","cwd":"/Users/andreas/tools"}' "$transcript" |
    CMUX_SURFACE_ID=surface-3 "$HOOK"
  rm -f "$transcript"
}

run_no_session_data_case() {
  printf '%s' '{"cwd":"/Users/andreas/tools"}' |
    CMUX_SURFACE_ID=surface-4 "$HOOK"
}

run_no_surface_case() {
  printf '%s' '{"session_id":"claude-session-5","cwd":"/Users/andreas/tools"}' |
    env -u CMUX_SURFACE_ID "$HOOK"
}

assert_capture 'custom session title has no project prefix' \
  $'^cmux\trename-tab\t--surface\tsurface-1\t--title\tClaude rename$' \
  bash -c "$(declare -f run_custom_title_case); run_custom_title_case"

assert_capture 'missing custom title with session id falls back to project name' \
  $'^cmux\trename-tab\t--surface\tsurface-2\t--title\ttools$' \
  bash -c "$(declare -f run_session_id_fallback_case); run_session_id_fallback_case"

assert_capture 'missing custom title with transcript falls back to project name' \
  $'^cmux\trename-tab\t--surface\tsurface-3\t--title\ttools$' \
  bash -c "$(declare -f run_transcript_path_fallback_case); run_transcript_path_fallback_case"

assert_capture 'missing session data falls back to project name' \
  $'^cmux\trename-tab\t--surface\tsurface-4\t--title\ttools$' \
  bash -c "$(declare -f run_no_session_data_case); run_no_session_data_case"

assert_empty_capture 'no cmux surface is a no-op' \
  bash -c "$(declare -f run_no_surface_case); run_no_surface_case"

if [ "$fail" -gt 0 ]; then
  printf '%d failed, %d passed\n' "$fail" "$pass"
  exit 1
fi
printf 'all %d passed\n' "$pass"
