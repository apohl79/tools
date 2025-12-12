#!/bin/bash
# Claude Code status line script

input=$(cat)
cwd=$(echo "$input" | jq -r ".workspace.current_dir")
model=$(echo "$input" | jq -r ".model.display_name")

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

# Output: directory (blue), git branch (green), model (magenta)
printf "\033[34m%s\033[0m\033[32m%s\033[0m\033[35m 𝌭 %s\033[0m" "$dir" "$git_info" "$model"
