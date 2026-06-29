#!/usr/bin/env bash
# Renames the cmux tab to "<cwd-basename>: <codex-session-name>" when CMUX_SURFACE_ID is set.
# Hook input: Codex lifecycle JSON on stdin with session_id and cwd.

set -euo pipefail

[ -n "${CMUX_SURFACE_ID:-}" ] || exit 0

input=$(cat)

fields=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("session_id","")); print(d.get("cwd",""))' 2>/dev/null || true)
session_id=$(printf '%s\n' "$fields" | sed -n '1p')
cwd=$(printf '%s\n' "$fields" | sed -n '2p')
[ -n "$cwd" ] || cwd="$PWD"

base=$(basename "$cwd")
codex_home="${CODEX_HOME:-$HOME/.codex}"
state_db="$codex_home/state_5.sqlite"
session_index="$codex_home/session_index.jsonl"

title=""
if [ -n "$session_id" ]; then
    title=$(SESSION_ID="$session_id" STATE_DB="$state_db" SESSION_INDEX="$session_index" python3 <<'PY' 2>/dev/null || true
import json
import os
import sqlite3

session_id = os.environ.get("SESSION_ID", "")
state_db = os.environ.get("STATE_DB", "")
session_index = os.environ.get("SESSION_INDEX", "")
title = ""

if state_db and os.path.isfile(state_db):
    try:
        with sqlite3.connect(f"file:{state_db}?mode=ro", uri=True, timeout=1.0) as conn:
            row = conn.execute(
                "select title from threads where id = ?",
                (session_id,),
            ).fetchone()
            if row:
                title = (row[0] or "").strip()
    except (OSError, sqlite3.Error):
        pass

if not title and session_index and os.path.isfile(session_index):
    try:
        with open(session_index, encoding="utf-8") as index:
            for line in index:
                try:
                    entry = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if entry.get("id") == session_id:
                    candidate = (entry.get("thread_name") or "").strip()
                    if candidate:
                        title = candidate
    except OSError:
        pass

print(title)
PY
)
fi

title=$(printf '%s' "$title" | tr '\r\n\t' '   ' | sed -E 's/[[:space:]]+/ /g; s/^ //; s/ $//')

if [ -n "$title" ]; then
    new_title="$base: $title"
else
    new_title="$base"
fi

cmux rename-tab --surface "$CMUX_SURFACE_ID" "$new_title" >/dev/null 2>&1 || true
