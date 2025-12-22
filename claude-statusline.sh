#!/bin/bash
# Claude Code status line script

input=$(cat)
cwd=$(echo "$input" | jq -r ".workspace.current_dir")
model=$(echo "$input" | jq -r ".model.display_name")
# Calculate total current context including all token types
# Add 25k tokens for system tools overhead (more portable across models than percentage-based compression)
tokens_percent=$(echo "$input" | jq -r '
  ((.context_window.current_usage.input_tokens // 0) +
   (.context_window.current_usage.cache_creation_input_tokens // 0) +
   (.context_window.current_usage.cache_read_input_tokens // 0) +
   25000) as $used_with_overhead |
  if .context_window.context_window_size > 0 then
    (($used_with_overhead / .context_window.context_window_size * 100) | floor | if . > 100 then 100 else . end)
  else
    0
  end
')

#user=$(whoami)
#host=$(hostname -s)
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
    git_info=" ⎇ ${branch}${status}"
else
    git_info=""
fi

# Output: directory (blue), git branch (green), model (magenta), context usage (cyan)
printf "\033[34m%s\033[0m\033[32m%s\033[0m\033[35m 𝌭 %s\033[0m\033[36m ⚡ %s%%\033[0m" "$dir" "$git_info" "$model" "$tokens_percent"
