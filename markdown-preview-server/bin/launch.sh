#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
SERVER_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

usage() {
  cat >&2 <<'EOF'
Usage:
  launch.sh start --doc <path> --session-dir <path> [--main-jsonl <path>]
  launch.sh wait  --session-dir <path>

  start  Boot the server detached, print the URL on stdout, exit 0.
         Writes url.txt and server.pid into --session-dir.
         --main-jsonl is optional; if omitted, an empty transcript is used.
  wait   Block until the server exits. Exits 0 either way (no result.json
         is required for markdown preview).
EOF
  exit 1
}

do_start() {
  local doc="" main_jsonl="" session_dir=""
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --doc) doc="$2"; shift 2;;
      --main-jsonl) main_jsonl="$2"; shift 2;;
      --session-dir) session_dir="$2"; shift 2;;
      *) echo "unknown arg: $1" >&2; exit 1;;
    esac
  done

  [[ -n "$doc" ]] || { echo "--doc required" >&2; exit 2; }
  [[ -n "$session_dir" ]] || { echo "--session-dir required" >&2; exit 1; }

  [[ -f "$doc" ]] || { echo "doc missing at $doc" >&2; exit 2; }
  local real_doc
  real_doc="$(cd "$(dirname "$doc")" && pwd -P)/$(basename "$doc")"

  mkdir -p "$session_dir"

  # Markdown preview has no upstream Claude Code session. If the caller did
  # not pass --main-jsonl, synthesise an empty one so the server's transcript
  # bootstrap has something to read.
  if [[ -z "$main_jsonl" ]]; then
    main_jsonl="$session_dir/empty-main.jsonl"
    : > "$main_jsonl"
  fi
  [[ -f "$main_jsonl" ]] || { echo "main-jsonl missing at $main_jsonl" >&2; exit 3; }

  node -v | awk -F. '{gsub(/[^0-9]/, "", $1); exit ($1+0 >= 20) ? 0 : 1}' \
    || { echo "Node >= 20 required (got $(node -v))" >&2; exit 5; }

  local install_marker="$SERVER_DIR/node_modules/.markdown-preview-install-complete"
  if [[ ! -f "$install_marker" ]]; then
    command -v npm >/dev/null 2>&1 \
      || { echo "npm required to install server deps (missing $install_marker)" >&2; exit 5; }
    echo "markdown-preview: installing server dependencies (first launch)…" >&2
    (cd "$SERVER_DIR" && npm install --silent --no-audit --no-fund) >&2 \
      || { echo "npm install failed" >&2; exit 5; }
    : > "$install_marker"
  fi
  if [[ ! -f "$SERVER_DIR/dist/server.js" ]]; then
    echo "markdown-preview: building server bundle…" >&2
    (cd "$SERVER_DIR" && node esbuild.config.mjs) >&2 \
      || { echo "build failed" >&2; exit 5; }
  fi

  # Stale artefacts from a previous run would confuse `wait` — remove them
  # before (re)booting.
  rm -f "$session_dir/url.txt" "$session_dir/server.pid" "$session_dir/result.json"
  : > "$session_dir/server.log"

  export IND_DOC="$real_doc"
  export IND_MAIN_JSONL="$main_jsonl"
  export IND_SESSION_DIR="$session_dir"
  export IND_STATIC_DIR="$SERVER_DIR/dist"

  local server_js="${IND_SERVER_JS:-$SERVER_DIR/dist/server.js}"
  nohup node "$server_js" >"$session_dir/server.log" 2>&1 </dev/null &
  local server_pid=$!
  disown "$server_pid" 2>/dev/null || true
  echo "$server_pid" > "$session_dir/server.pid"

  local url="" deadline=$((SECONDS + 30))
  while (( SECONDS < deadline )); do
    if [[ -s "$session_dir/server.log" ]]; then
      url="$(grep -m1 '^http://' "$session_dir/server.log" 2>/dev/null || true)"
      [[ -n "$url" ]] && break
    fi
    if ! kill -0 "$server_pid" 2>/dev/null; then
      tail -n 30 "$session_dir/server.log" >&2 || true
      rm -f "$session_dir/server.pid"
      exit 7
    fi
    sleep 0.1
  done

  if [[ -z "$url" ]]; then
    echo "server did not emit URL within 30s" >&2
    tail -n 30 "$session_dir/server.log" >&2 || true
    kill "$server_pid" 2>/dev/null || true
    rm -f "$session_dir/server.pid"
    exit 7
  fi

  echo "$url" > "$session_dir/url.txt"
  echo "$url"
}

do_wait() {
  local session_dir=""
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --session-dir) session_dir="$2"; shift 2;;
      *) echo "unknown arg: $1" >&2; exit 1;;
    esac
  done

  [[ -n "$session_dir" ]] || { echo "--session-dir required" >&2; exit 1; }
  [[ -f "$session_dir/server.pid" ]] || { echo "no server.pid in $session_dir — was start run?" >&2; exit 1; }

  local server_pid
  server_pid="$(cat "$session_dir/server.pid")"

  while kill -0 "$server_pid" 2>/dev/null; do
    sleep 1
  done

  rm -f "$session_dir/server.pid"
  return 0
}

[[ $# -ge 1 ]] || usage
cmd="$1"; shift
case "$cmd" in
  start) do_start "$@" ;;
  wait)  do_wait "$@" ;;
  -h|--help) usage ;;
  *) echo "unknown command: $cmd" >&2; usage ;;
esac
