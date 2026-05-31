#!/usr/bin/env bash
# Renames the cmux tab to "<cwd-basename>: <session-title>" when CMUX_SURFACE_ID is set.
# Reads the latest custom-title entry from the session transcript (set by /rename).
# Hook input: JSON on stdin with transcript_path and cwd.

set -euo pipefail

[ -n "${CMUX_SURFACE_ID:-}" ] || exit 0

input=$(cat)

transcript_path=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("transcript_path",""))' 2>/dev/null || true)
cwd=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("cwd",""))' 2>/dev/null || true)
[ -n "$cwd" ] || cwd="$PWD"

base=$(basename "$cwd")

title=""
if [ -n "$transcript_path" ] && [ -f "$transcript_path" ]; then
    title=$(grep '"type":"custom-title"' "$transcript_path" 2>/dev/null \
        | tail -1 \
        | python3 -c 'import json,sys; line=sys.stdin.read().strip(); print(json.loads(line).get("customTitle","")) if line else None' 2>/dev/null || true)
fi

if [ -n "$title" ]; then
    new_title="$base: $title"
else
    new_title="$base"
fi

cmux rename-tab --surface "$CMUX_SURFACE_ID" --title "$new_title" >/dev/null 2>&1 || true
