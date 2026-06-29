#!/usr/bin/env bash
# tests/codex-cmux-tab-title-test.sh - verifies the Codex cmux tab-title hook.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HOOK="$REPO_ROOT/.codex/hooks/cmux-tab-title.sh"
HOOKS_JSON="$REPO_ROOT/.codex/hooks/hooks.json"
FIXTURES="$REPO_ROOT/tests/fixtures"

fail=0
pass=0

with_env() {
  local capture="$1"; shift
  local tmp="$1"; shift

  mkdir -p "$tmp/bin" "$tmp/codex"
  ln -sf "$FIXTURES/cmux-mock.sh" "$tmp/bin/cmux"

  env \
    PATH="$tmp/bin:$PATH" \
    CAPTURE_FILE="$capture" \
    CODEX_HOME="$tmp/codex" \
    HOOK="$HOOK" \
    HOOKS_JSON="$HOOKS_JSON" \
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

run_named_session_case() {
  cat >"$CODEX_HOME/session_index.jsonl" <<'JSONL'
{"id":"session-1","thread_name":"Implement cmux tabs","updated_at":"2026-05-31T10:00:00Z"}
JSONL
  printf '%s' '{"hook_event_name":"SessionStart","session_id":"session-1","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","source":"startup"}' |
    CMUX_SURFACE_ID=surface-1 "$HOOK"
}

run_duplicate_session_case() {
  cat >"$CODEX_HOME/session_index.jsonl" <<'JSONL'
{"id":"session-2","thread_name":"Old name","updated_at":"2026-05-31T10:00:00Z"}
{"id":"session-2","thread_name":"New name","updated_at":"2026-05-31T11:00:00Z"}
JSONL
  printf '%s' '{"hook_event_name":"UserPromptSubmit","session_id":"session-2","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","turn_id":"turn-1","prompt":"hi"}' |
    CMUX_SURFACE_ID=surface-2 "$HOOK"
}

run_sqlite_session_case() {
  cat >"$CODEX_HOME/session_index.jsonl" <<'JSONL'
{"id":"session-3","thread_name":"Stale JSONL name","updated_at":"2026-05-31T10:00:00Z"}
JSONL
  python3 <<'PY'
import os
import sqlite3

db_path = os.path.join(os.environ["CODEX_HOME"], "state_5.sqlite")
with sqlite3.connect(db_path) as conn:
    conn.execute("create table threads (id text primary key, title text not null)")
    conn.execute(
        "insert into threads (id, title) values (?, ?)",
        ("session-3", "SQLite session name"),
    )
PY
  printf '%s' '{"hook_event_name":"SessionStart","session_id":"session-3","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","source":"startup"}' |
    CMUX_SURFACE_ID=surface-3 "$HOOK"
}

run_sqlite_prompt_title_fallback_case() {
  cat >"$CODEX_HOME/session_index.jsonl" <<'JSONL'
{"id":"session-4","thread_name":"Actual session name","updated_at":"2026-05-31T10:00:00Z"}
JSONL
  python3 <<'PY'
import os
import sqlite3

db_path = os.path.join(os.environ["CODEX_HOME"], "state_5.sqlite")
with sqlite3.connect(db_path) as conn:
    conn.execute("create table threads (id text primary key, title text not null, first_user_message text)")
    conn.execute(
        "insert into threads (id, title, first_user_message) values (?, ?, ?)",
        ("session-4", "Investigate cmux hook", "Investigate cmux hook"),
    )
PY
  printf '%s' '{"hook_event_name":"UserPromptSubmit","session_id":"session-4","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","turn_id":"turn-1","prompt":"hi"}' |
    CMUX_SURFACE_ID=surface-4 "$HOOK"
}

run_missing_name_case() {
  printf '%s' '{"hook_event_name":"UserPromptSubmit","session_id":"missing","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","turn_id":"turn-1","prompt":"abcdefghijklmnopqrstuvwxyz1234567890"}' |
    CMUX_SURFACE_ID=surface-5 "$HOOK"
}

run_transcript_prompt_fallback_case() {
  local transcript; transcript="$(mktemp)"
  cat >"$transcript" <<'JSONL'
{"type":"event_msg","payload":{"type":"user_message","message":"first prompt should not win"}}
{"type":"event_msg","payload":{"type":"user_message","message":"## My request for Codex:\nlast transcript prompt is selected here"}}
JSONL
  printf '{"hook_event_name":"SessionStart","session_id":"missing-transcript","cwd":"/Users/andreas/tools","transcript_path":"%s","model":"gpt-5.5","permission_mode":"default","source":"resume"}' "$transcript" |
    CMUX_SURFACE_ID=surface-6 "$HOOK"
  rm -f "$transcript"
}

run_missing_name_without_prompt_case() {
  printf '%s' '{"hook_event_name":"SessionStart","session_id":"missing-without-prompt","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","source":"startup"}' |
    CMUX_SURFACE_ID=surface-7 "$HOOK"
}

run_no_surface_case() {
  printf '%s' '{"hook_event_name":"SessionStart","session_id":"session-1","cwd":"/Users/andreas/tools","transcript_path":null,"model":"gpt-5.5","permission_mode":"default","source":"startup"}' |
    env -u CMUX_SURFACE_ID "$HOOK"
}

assert_capture 'named session title' \
  $'^cmux\trename-tab\t--surface\tsurface-1\tImplement cmux tabs$' \
  bash -c "$(declare -f run_named_session_case); run_named_session_case"

assert_capture 'latest duplicate session name wins' \
  $'^cmux\trename-tab\t--surface\tsurface-2\tNew name$' \
  bash -c "$(declare -f run_duplicate_session_case); run_duplicate_session_case"

assert_capture 'sqlite session title wins over stale jsonl' \
  $'^cmux\trename-tab\t--surface\tsurface-3\tSQLite session name$' \
  bash -c "$(declare -f run_sqlite_session_case); run_sqlite_session_case"

assert_capture 'sqlite prompt title falls back to session index name' \
  $'^cmux\trename-tab\t--surface\tsurface-4\tActual session name$' \
  bash -c "$(declare -f run_sqlite_prompt_title_fallback_case); run_sqlite_prompt_title_fallback_case"

assert_capture 'missing session name falls back to prompt excerpt' \
  $'^cmux\trename-tab\t--surface\tsurface-5\tabcdefghijklmnopqrstuvwxyz123456$' \
  bash -c "$(declare -f run_missing_name_case); run_missing_name_case"

assert_capture 'session start falls back to last transcript prompt excerpt' \
  $'^cmux\trename-tab\t--surface\tsurface-6\tlast transcript prompt is select$' \
  bash -c "$(declare -f run_transcript_prompt_fallback_case); run_transcript_prompt_fallback_case"

assert_capture 'missing session name without prompt falls back to project name' \
  $'^cmux\trename-tab\t--surface\tsurface-7\ttools$' \
  bash -c "$(declare -f run_missing_name_without_prompt_case); run_missing_name_without_prompt_case"

assert_empty_capture 'no cmux surface is a no-op' \
  bash -c "$(declare -f run_no_surface_case); run_no_surface_case"

if python3 - "$HOOKS_JSON" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as f:
    data = json.load(f)

hooks = data["hooks"]
for event in ("SessionStart", "UserPromptSubmit"):
    groups = hooks[event]
    assert len(groups) == 1
    handlers = groups[0]["hooks"]
    assert len(handlers) == 1
    handler = handlers[0]
    assert handler["type"] == "command"
    assert handler["command"] == "$HOME/.codex/hooks/cmux-tab-title.sh"
PY
then
  pass=$((pass+1)); printf 'PASS hooks.json registers Codex hook events\n'
else
  fail=$((fail+1)); printf 'FAIL hooks.json registers Codex hook events\n'
fi

if [ "$fail" -gt 0 ]; then
  printf '%d failed, %d passed\n' "$fail" "$pass"
  exit 1
fi
printf 'all %d passed\n' "$pass"
