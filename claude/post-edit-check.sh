#!/usr/bin/env bash
# Post-edit type checker for Claude Code hooks.
# Detects language from the edited file and runs the appropriate check.
#
# Input: receives JSON via stdin from Claude Code hook with tool_input containing file_path.

set -euo pipefail

# Extract file path from hook JSON
FILE_PATH=$(cat | jq -r '.tool_input.file_path // .tool_input.file_path // empty' 2>/dev/null)

if [[ -z "$FILE_PATH" ]]; then
  exit 0
fi

# Determine language from extension
case "$FILE_PATH" in
  *.rs)
    if command -v cargo &>/dev/null && [[ -f "Cargo.toml" ]]; then
      cargo check --quiet 2>&1 | head -20
    fi
    ;;
  *.ts|*.tsx)
    if command -v npx &>/dev/null && [[ -f "tsconfig.json" ]]; then
      npx tsc --noEmit 2>&1 | head -20
    fi
    ;;
esac
