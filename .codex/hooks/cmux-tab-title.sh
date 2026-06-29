#!/usr/bin/env bash
# Renames the cmux tab to the Codex session name when CMUX_SURFACE_ID is set.
# Hook input: Codex lifecycle JSON on stdin with session_id, prompt, transcript_path, and cwd.

set -euo pipefail

[ -n "${CMUX_SURFACE_ID:-}" ] || exit 0

input=$(cat)

session_id=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("session_id",""))' 2>/dev/null || true)
cwd=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("cwd",""))' 2>/dev/null || true)
[ -n "$cwd" ] || cwd="$PWD"
project_name=$(basename "$cwd")
prompt_excerpt=$(INPUT_JSON="$input" python3 <<'PY' 2>/dev/null || true
import json
import os

USER_MESSAGE_BEGIN = "## My request for Codex:"
MAX_TITLE_CHARS = 32


def clean_prompt(value):
    if not isinstance(value, str):
        return ""
    text = value.strip()
    if USER_MESSAGE_BEGIN in text:
        text = text.split(USER_MESSAGE_BEGIN, 1)[1].strip()
    text = " ".join(text.split())
    return text[:MAX_TITLE_CHARS].strip()


def last_prompt_from_transcript(path):
    if not path or not os.path.isfile(path):
        return ""
    last_prompt = ""
    try:
        with open(path, encoding="utf-8") as transcript:
            for line in transcript:
                try:
                    item = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if item.get("type") != "event_msg":
                    continue
                payload = item.get("payload") or {}
                if payload.get("type") == "user_message":
                    last_prompt = payload.get("message") or ""
    except OSError:
        return ""
    return last_prompt


payload = json.loads(os.environ.get("INPUT_JSON", "{}"))
excerpt = clean_prompt(payload.get("prompt"))
if not excerpt:
    excerpt = clean_prompt(last_prompt_from_transcript(payload.get("transcript_path")))

print(excerpt)
PY
)

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
            columns = {
                row[1]
                for row in conn.execute("pragma table_info(threads)").fetchall()
            }
            first_user_expr = "first_user_message" if "first_user_message" in columns else "NULL"
            row = conn.execute(
                f"select title, {first_user_expr} from threads where id = ?",
                (session_id,),
            ).fetchone()
            if row:
                sqlite_title = (row[0] or "").strip()
                first_user_message = (row[1] or "").strip()
                if sqlite_title and sqlite_title != first_user_message:
                    title = sqlite_title
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
    new_title="$title"
elif [ -n "$prompt_excerpt" ]; then
    new_title="$prompt_excerpt"
elif [ -n "$project_name" ]; then
    new_title="$project_name"
else
    exit 0
fi

cmux rename-tab --surface "$CMUX_SURFACE_ID" "$new_title" >/dev/null 2>&1 || true
