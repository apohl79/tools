#!/usr/bin/env bash
# Renames the cmux tab to the Claude session title when CMUX_SURFACE_ID is set.
# Reads the latest custom-title entry from the session transcript (set by /rename).
# Hook input: JSON on stdin with transcript_path, session_id, and cwd.

set -euo pipefail

[ -n "${CMUX_SURFACE_ID:-}" ] || exit 0

input=$(cat)

fields=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("transcript_path","") or ""); print(d.get("session_id","") or ""); print(d.get("cwd","") or "")' 2>/dev/null || true)
transcript_path=$(printf '%s\n' "$fields" | sed -n '1p')
session_id=$(printf '%s\n' "$fields" | sed -n '2p')
cwd=$(printf '%s\n' "$fields" | sed -n '3p')
[ -n "$cwd" ] || cwd="$PWD"
project_name=$(basename "$cwd")

title=""
if [ -n "$transcript_path" ] && [ -f "$transcript_path" ]; then
    title=$(grep '"type":"custom-title"' "$transcript_path" 2>/dev/null \
        | tail -1 \
        | python3 -c 'import json,sys; line=sys.stdin.read().strip(); print(json.loads(line).get("customTitle","")) if line else None' 2>/dev/null || true)
fi

if [ -n "$title" ]; then
    new_title="$title"
elif [ -n "$session_id" ]; then
    new_title="$session_id"
elif [ -n "$transcript_path" ]; then
    new_title=$(basename "$transcript_path")
elif [ -n "$project_name" ]; then
    new_title="$project_name"
else
    exit 0
fi

cmux rename-tab --surface "$CMUX_SURFACE_ID" --title "$new_title" >/dev/null 2>&1 || true
