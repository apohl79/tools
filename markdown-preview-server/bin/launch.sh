#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
SERVER_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

usage() {
  cat >&2 <<'EOF'
Usage:
  launch.sh start --doc <path> --session-dir <path> [--main-jsonl <path>] [--agent claude|codex] [--cwd <path>] [--hold]
  launch.sh wait  --session-dir <path>

  start  Boot the server detached, print the URL on stdout, exit 0.
         Writes url.txt and server.pid into --session-dir.
         --main-jsonl is optional; if omitted, an empty transcript is used.
         With --hold, print the URL and keep the launcher alive until the server exits.
  wait   Block until the server exits (user clicked Finish). Exits 0 if
         <session-dir>/result.json was produced; the caller reads it.
EOF
  exit 1
}

do_start() {
  local doc="" main_jsonl="" session_dir="" agent="claude" agent_cwd="$PWD" hold="false"
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --doc) doc="$2"; shift 2;;
      --main-jsonl) main_jsonl="$2"; shift 2;;
      --session-dir) session_dir="$2"; shift 2;;
      --agent) agent="$2"; shift 2;;
      --cwd) agent_cwd="$2"; shift 2;;
      --hold) hold="true"; shift;;
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
  [[ "$agent" == "claude" || "$agent" == "codex" ]] || { echo "--agent must be claude or codex" >&2; exit 2; }
  [[ -d "$agent_cwd" ]] || { echo "--cwd directory missing at $agent_cwd" >&2; exit 2; }
  local real_agent_cwd
  real_agent_cwd="$(cd "$agent_cwd" && pwd -P)"

  node -v | awk -F. '{gsub(/[^0-9]/, "", $1); exit ($1+0 >= 20) ? 0 : 1}' \
    || { echo "Node >= 20 required (got $(node -v))" >&2; exit 5; }
  if [[ "$agent" == "codex" ]]; then
    command -v codex >/dev/null 2>&1 \
      || { echo "codex CLI required for --agent $agent" >&2; exit 5; }
  fi

  # First-launch dependency install. The bundle keeps runtime deps (marked,
  # jsdom, the agent SDK, …) external, so they must resolve from node_modules/
  # at startup. The install-complete marker is written only after npm install
  # finishes cleanly; its absence means either a fresh clone/plugin update or
  # a previous install that was interrupted and left node_modules/ in a partial
  # state.
  #
  # Dev deps are installed too (no --omit=dev) because esbuild is required to
  # build dist/ on first launch and on any src/ change.
  local install_marker="$SERVER_DIR/node_modules/.markdown-preview-install-complete"
  if [[ ! -f "$install_marker" ]]; then
    command -v npm >/dev/null 2>&1 \
      || { echo "npm required to install server deps (missing $install_marker)" >&2; exit 5; }
    echo "markdown-preview: installing server dependencies (first launch)…" >&2
    (cd "$SERVER_DIR" && npm install --silent --no-audit --no-fund) >&2 \
      || { echo "npm install failed" >&2; exit 5; }
    : > "$install_marker"
  fi

  # Build dist/ if it's missing (first launch) or any src/ file is newer
  # (the user pulled new code without rebuilding).
  local dist_marker="$SERVER_DIR/dist/index.html"
  local needs_build=0
  if [[ ! -f "$dist_marker" || ! -f "$SERVER_DIR/dist/server.js" ]]; then
    needs_build=1
  elif find "$SERVER_DIR/src" -type f -newer "$dist_marker" -print -quit 2>/dev/null | grep -q .; then
    needs_build=1
  fi
  if (( needs_build )); then
    command -v npm >/dev/null 2>&1 \
      || { echo "npm required to rebuild dist/" >&2; exit 5; }
    echo "markdown-preview: rebuilding dist/ (src/ newer than dist/)…" >&2
    (cd "$SERVER_DIR" && npm run --silent build) >&2 \
      || { echo "npm run build failed" >&2; exit 5; }
  fi

  # Stale artefacts from a previous run would confuse `wait` — remove them
  # before (re)booting.
  rm -f "$session_dir/url.txt" "$session_dir/server.pid" "$session_dir/result.json"
  : > "$session_dir/server.log"

  export IND_DOC="$real_doc"
  export IND_MAIN_JSONL="$main_jsonl"
  export IND_SESSION_DIR="$session_dir"
  export IND_STATIC_DIR="$SERVER_DIR/dist"
  export IND_AGENT="$agent"
  export IND_AGENT_CWD="$real_agent_cwd"

  local server_js="${IND_SERVER_JS:-$SERVER_DIR/dist/server.js}"
  nohup node "$server_js" >"$session_dir/server.log" 2>&1 </dev/null &
  local server_pid=$!
  if [[ "$hold" != "true" ]]; then
    disown "$server_pid" 2>/dev/null || true
  fi
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

  if [[ "$hold" == "true" ]]; then
    wait "$server_pid" || true
  fi
}

notify_apply_monitoring() {
  local session_dir="$1"
  local url_file="$session_dir/url.txt"
  [[ -f "$url_file" ]] || return 0
  local url
  url="$(cat "$url_file" 2>/dev/null || true)"
  [[ -n "$url" ]] || return 0
  node -e 'fetch(process.argv[1], { method: "POST" }).catch(() => {})' "${url%/}/api/apply/monitoring" >/dev/null 2>&1 || true
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
  notify_apply_monitoring "$session_dir"

  # Poll the session dir for either result.json (Finish) or apply-N.json
  # (Apply). Print the first matching path, exit 0. Also bail if the
  # server PID disappears without producing either file.
  local found=""
  while :; do
    if [[ -f "$session_dir/result.json" ]]; then
      found="$session_dir/result.json"
      break
    fi
    # apply-*.json — pick the lowest-numbered one in case of multiple.
    local first_apply
    first_apply="$(ls -1 "$session_dir"/apply-*.json 2>/dev/null | sort -V | head -n1 || true)"
    if [[ -n "$first_apply" ]]; then
      found="$first_apply"
      break
    fi
    if ! kill -0 "$server_pid" 2>/dev/null; then
      echo "server died without producing a signal file" >&2
      tail -n 30 "$session_dir/server.log" >&2 || true
      rm -f "$session_dir/server.pid"
      exit 7
    fi
    sleep 0.2
  done

  # If we found a Finish, the server will (or has) called process.exit;
  # remove server.pid so a stale file doesn't confuse later wait calls.
  if [[ "$(basename "$found")" == "result.json" ]]; then
    # Wait briefly for the server to exit cleanly after finish.
    local deadline=$((SECONDS + 5))
    while (( SECONDS < deadline )); do
      kill -0 "$server_pid" 2>/dev/null || break
      sleep 0.1
    done
    rm -f "$session_dir/server.pid"
  fi

  echo "$found"
}

[[ $# -ge 1 ]] || usage
cmd="$1"; shift
case "$cmd" in
  start) do_start "$@" ;;
  wait)  do_wait "$@" ;;
  -h|--help) usage ;;
  *) echo "unknown command: $cmd" >&2; usage ;;
esac
