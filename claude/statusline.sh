#!/bin/bash
# Claude Code status line script
# Queries proxy API via curl+jq for model/context resolution (no CLI binary needed)
#
# Feature flags — set via environment to override defaults (1=on, 0=off)
# Project directory basename (blue)
ENABLE_DIR=${ENABLE_DIR:-1}
# Git branch + dirty indicator (green)
ENABLE_GIT=${ENABLE_GIT:-1}
# Model name (magenta) — see MODEL_DISPLAY
ENABLE_MODEL=${ENABLE_MODEL:-1}
# Context window usage percentage (cyan)
ENABLE_CONTEXT=${ENABLE_CONTEXT:-1}
# Session cost in USD (yellow)
ENABLE_COST=${ENABLE_COST:-1}
# Wall-clock session duration (white)
ENABLE_DURATION=${ENABLE_DURATION:-0}
# Time spent waiting for API responses (dim white)
ENABLE_API_DURATION=${ENABLE_API_DURATION:-0}
# Lines added/removed in session (green/red)
ENABLE_LINES=${ENABLE_LINES:-0}
# Cumulative input/output token counts (dim)
ENABLE_TOKENS=${ENABLE_TOKENS:-1}
# Claude Code version (dim gray)
ENABLE_VERSION=${ENABLE_VERSION:-0}
# Vim mode indicator — NORMAL/INSERT (bold yellow)
ENABLE_VIM_MODE=${ENABLE_VIM_MODE:-0}
# Agent name when running with --agent (italic cyan)
ENABLE_AGENT=${ENABLE_AGENT:-0}
# Model display: "id" (default) or "display_name"
MODEL_DISPLAY=${MODEL_DISPLAY:-id}
# Cost source: "claude" (client-side), "proxy" (server-side), or "both"
COST_SOURCE=${COST_SOURCE:-proxy}

input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir')

# Get model and context from proxy API (direct HTTP, no CLI binary needed)
proxy_info=""
proxy_error=""
proxy_cost=""

# Check proxy health first (sets proxy_reachable/proxy_error before session fetch)
proxy_reachable=""
if [ -n "$ANTHROPIC_BASE_URL" ]; then
    if curl -sf --max-time 1 "$ANTHROPIC_BASE_URL/health" >/dev/null 2>&1; then
        proxy_reachable=true
    else
        proxy_error=1
    fi
fi

session_id_raw=$(echo "$input" | jq -r '.session_id // empty')
# Normalize JSON-blob session IDs: {"device_id":"...","session_id":"uuid"} → uuid
session_id=$(echo "$session_id_raw" | jq -r 'if type == "object" then (.session_id // empty) else . end' 2>/dev/null || echo "$session_id_raw")
[ -z "$session_id" ] && session_id="$session_id_raw"
if [ "$proxy_reachable" = "true" ] && [ -n "$session_id" ]; then
    proxy_info=$(curl -sf --max-time 1 "$ANTHROPIC_BASE_URL/_proxy/sessions/$session_id" 2>/dev/null)
fi

if [ -n "$proxy_info" ] && echo "$proxy_info" | jq -e '.session' >/dev/null 2>&1; then
    # Resolve model from proxy; fall back to Claude Code if proxy returns "default"
    proxy_model=$(echo "$proxy_info" | jq -r '
        .session |
        if .modelOverride != null and .modelOverride != "" then
            .modelOverride
        elif (.fallbackState.active // false) == true and .fallbackState.currentFallbackModel != null then
            .fallbackState.currentFallbackModel
        else
            .model // "default"
        end
    ')

    if [ "$proxy_model" = "default" ]; then
        # Passthrough mode: use Claude Code's model name (includes suffixes like [1m])
        if [ "$MODEL_DISPLAY" = "display_name" ]; then
            model=$(echo "$input" | jq -r '(.model.display_name // .model.id // "unknown") | sub("^claude-"; "")')
        else
            model=$(echo "$input" | jq -r '(.model.id // "unknown") | sub("^claude-"; "")')
        fi
    else
        model=$(echo "$proxy_model" | sed 's/^claude-//')
    fi

    # Context remaining: use the model_usage entry with the highest lastInputSnapshot
    context_info=$(echo "$proxy_info" | jq -r '
        .session |
        ([.modelUsage[].lastInputSnapshot // 0] | max // 0) as $fill_tokens |
        if .contextWindowSize > 0 and $fill_tokens > 0 then
            (($fill_tokens / .contextWindowSize * 100)
             | round | [., 100] | min | [., 0] | max) as $fill |
            "\($fill)%"
        else
            "0%"
        end
    ')

    # Cost from the computed field
    proxy_cost=$(echo "$proxy_info" | jq -r '.sessionCostUsd // empty')
else
    # Proxy not available: use Claude Code's stdin data
    if [ "$MODEL_DISPLAY" = "display_name" ]; then
        model=$(echo "$input" | jq -r '(.model.display_name // .model.id // "unknown") | sub("^claude-"; "")')
    else
        model=$(echo "$input" | jq -r '(.model.id // "unknown") | sub("^claude-"; "")')
    fi
    context_info=$(echo "$input" | jq -r '
      if .context_window.used_percentage != null then
        "\(.context_window.used_percentage)%"
      elif .context_window.remaining_percentage != null then
        "\(100 - .context_window.remaining_percentage)%"
      else
        "0%"
      end
    ')
fi

# Helpers
fmt_duration() {
    local ms=$1 s m h
    s=$((ms / 1000))
    h=$((s / 3600)); m=$(( (s % 3600) / 60 )); s=$((s % 60))
    if [ $h -gt 0 ]; then
        printf '%dh%dm' "$h" "$m"
    elif [ $m -gt 0 ]; then
        printf '%dm%ds' "$m" "$s"
    else
        printf '%ds' "$s"
    fi
}

fmt_tokens() {
    local t=$1
    if [ "$t" -ge 1000000 ]; then
        printf '%.1fM' "$(echo "scale=1; $t/1000000" | bc)"
    elif [ "$t" -ge 1000 ]; then
        printf '%.1fk' "$(echo "scale=1; $t/1000" | bc)"
    else
        printf '%d' "$t"
    fi
}

# Separator (dark gray dot)
sep="\033[38;5;240m\xe2\x8f\xba\033[0m"

# Collect visible parts — separators are only rendered between non-empty entries
parts=()

# --- Directory (blue) ---
if [ "$ENABLE_DIR" = "1" ]; then
    # Resolve main repo root for worktrees (git --git-common-dir points to main .git)
    repo_root=$(git -C "$cwd" rev-parse --path-format=absolute --git-common-dir 2>/dev/null | sed 's|/.git$||')
    dir=$(basename "${repo_root:-$cwd}")
    [ -n "$dir" ] && parts+=("\033[34m${dir}\033[0m")
fi

# --- Git info (green) ---
if [ "$ENABLE_GIT" = "1" ] && git -C "$cwd" rev-parse --git-dir >/dev/null 2>&1; then
    branch=$(git -C "$cwd" --no-optional-locks rev-parse --abbrev-ref HEAD 2>/dev/null)
    if ! git -C "$cwd" --no-optional-locks diff --quiet 2>/dev/null || \
       ! git -C "$cwd" --no-optional-locks diff --cached --quiet 2>/dev/null; then
        dirty="*"
    else
        dirty=""
    fi
    git_info="${branch}${dirty}"
    [ -n "$git_info" ] && parts+=("\033[32m${git_info}\033[0m")
fi

# --- Agent name (italic cyan) ---
if [ "$ENABLE_AGENT" = "1" ]; then
    agent=$(echo "$input" | jq -r '.agent.name // empty')
    [ -n "$agent" ] && parts+=("\033[3;36m${agent}\033[0m")
fi

# --- Model (magenta) + proxy/direct indicator ---
if [ "$ENABLE_MODEL" = "1" ] && [ -n "$model" ] && [ "$model" != "unknown" ]; then
    route=""
    [ "$proxy_reachable" = "true" ] && route="\033[33m\xe2\x87\x86\033[0m"  # ⇆ yellow
    [ "$proxy_error" = "1" ] && route="\033[31m\xe2\x9a\xa0\033[0m"   # ⚠ red
    parts+=("\033[35m${model}\033[0m${route}")
fi

# --- Vim mode (bold yellow) ---
if [ "$ENABLE_VIM_MODE" = "1" ]; then
    vim_mode=$(echo "$input" | jq -r '.vim.mode // empty')
    [ -n "$vim_mode" ] && parts+=("\033[1;33m${vim_mode}\033[0m")
fi

# --- Context usage (cyan) ---
if [ "$ENABLE_CONTEXT" = "1" ] && [ -n "$context_info" ]; then
    parts+=("\033[36m${context_info}\033[0m")
fi

# --- Token counts (dim) ---
if [ "$ENABLE_TOKENS" = "1" ]; then
    in_tok=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
    out_tok=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
    if [ "$in_tok" != "0" ] || [ "$out_tok" != "0" ]; then
        parts+=("\033[2m$(fmt_tokens "$in_tok")\xe2\x86\x93$(fmt_tokens "$out_tok")\xe2\x86\x91\033[0m")
    fi
fi

# --- Cost (yellow) ---
if [ "$ENABLE_COST" = "1" ]; then
    claude_cost=$(echo "$input" | jq -r '.cost.total_cost_usd // empty')
    cost_text=""
    case "$COST_SOURCE" in
        proxy)
            if [ -n "$proxy_cost" ] && [ "$proxy_cost" != "null" ] && echo "$proxy_cost" | awk '{exit ($1 == 0)}'; then
                cost_text=$(printf '$%.2f' "$proxy_cost")
            fi
            ;;
        both)
            c_fmt="" p_fmt=""
            if [ -n "$claude_cost" ] && echo "$claude_cost" | awk '{exit ($1 == 0)}'; then
                c_fmt=$(printf '$%.2f' "$claude_cost")
            fi
            if [ -n "$proxy_cost" ] && [ "$proxy_cost" != "null" ] && echo "$proxy_cost" | awk '{exit ($1 == 0)}'; then
                p_fmt=$(printf '$%.2f' "$proxy_cost")
            fi
            if [ -n "$c_fmt" ] && [ -n "$p_fmt" ]; then
                cost_text="${c_fmt}/${p_fmt}"
            elif [ -n "$c_fmt" ]; then
                cost_text="$c_fmt"
            elif [ -n "$p_fmt" ]; then
                cost_text="$p_fmt"
            fi
            ;;
        *)  # claude (default)
            if [ -n "$claude_cost" ] && echo "$claude_cost" | awk '{exit ($1 == 0)}'; then
                cost_text=$(printf '$%.2f' "$claude_cost")
            fi
            ;;
    esac
    [ -n "$cost_text" ] && parts+=("\033[33m${cost_text}\033[0m")
fi

# --- Lines changed (green +N / red -N) ---
if [ "$ENABLE_LINES" = "1" ]; then
    added=$(echo "$input" | jq -r '.cost.total_lines_added // 0')
    removed=$(echo "$input" | jq -r '.cost.total_lines_removed // 0')
    if [ "$added" != "0" ] || [ "$removed" != "0" ]; then
        parts+=("\033[32m+${added}\033[0m/\033[31m-${removed}\033[0m")
    fi
fi

# --- Session duration (white) ---
if [ "$ENABLE_DURATION" = "1" ]; then
    dur_ms=$(echo "$input" | jq -r '.cost.total_duration_ms // empty')
    if [ -n "$dur_ms" ] && [ "$dur_ms" != "0" ]; then
        parts+=("\033[37m$(fmt_duration "$dur_ms")\033[0m")
    fi
fi

# --- API duration (dim white) ---
if [ "$ENABLE_API_DURATION" = "1" ]; then
    api_ms=$(echo "$input" | jq -r '.cost.total_api_duration_ms // empty')
    if [ -n "$api_ms" ] && [ "$api_ms" != "0" ]; then
        parts+=("\033[2;37mapi:$(fmt_duration "$api_ms")\033[0m")
    fi
fi

# --- Version (dim gray) ---
if [ "$ENABLE_VERSION" = "1" ]; then
    version=$(echo "$input" | jq -r '.version // empty')
    [ -n "$version" ] && parts+=("\033[2;37mv${version}\033[0m")
fi

# --- Join parts with separator (only between non-empty elements) ---
output=""
for i in "${!parts[@]}"; do
    [ "$i" -gt 0 ] && output+=" $(printf '%b' "$sep") "
    output+="${parts[$i]}"
done

printf '%b' "$output"
