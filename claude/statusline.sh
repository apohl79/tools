#!/bin/bash
# Claude Code status line script
# Delegates model/context resolution to claude-code-proxy statusline command

input=$(cat)
cwd=$(echo "$input" | jq -r ".workspace.current_dir")

# Get model and context from proxy CLI (reads stdin JSON, queries proxy API)
proxy_info=$(echo "$input" | claude-code-proxy statusline 2>/dev/null)

if [ -n "$proxy_info" ] && echo "$proxy_info" | jq -e '.model' >/dev/null 2>&1; then
    model=$(echo "$proxy_info" | jq -r '.model')
    context_remaining=$(echo "$proxy_info" | jq -r '.context_remaining')
    context_info="$((100 - context_remaining))%"
else
    # Fallback: read directly from Claude Code's input
    model=$(echo "$input" | jq -r '.model.id // "unknown"')
    context_info=$(echo "$input" | jq -r '
      (.context_window.remaining_percentage // 100) as $remaining |
      ((100 - $remaining) | floor | if . < 0 then 0 elif . > 100 then 100 else . end) as $used |
      "\($used)%"
    ')
fi

dir=$(basename "$cwd")

# Git info
if git -C "$cwd" rev-parse --git-dir >/dev/null 2>&1; then
    branch=$(git -C "$cwd" --no-optional-locks rev-parse --abbrev-ref HEAD 2>/dev/null)
    if ! git -C "$cwd" --no-optional-locks diff --quiet 2>/dev/null || \
       ! git -C "$cwd" --no-optional-locks diff --cached --quiet 2>/dev/null; then
        status="*"
    else
        status=""
    fi
    git_info="${branch}${status}"
else
    git_info=""
fi

# Separator (dark gray)
sep="\033[38;5;240m⏺\033[0m"

# Output: directory (blue), git branch (green), model (magenta), context usage (cyan)
printf "\033[34m%s\033[0m $sep \033[32m%s\033[0m $sep \033[35m%s\033[0m $sep \033[36m%s\033[0m" "$dir" "$git_info" "$model" "$context_info"
