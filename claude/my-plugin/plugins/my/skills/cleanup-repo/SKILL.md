---
description: Clean up a local repository (remove git-worktrees and local feature brances)
allowed-tools: Write, Bash(git:*), AskUserQuestion
---

Find git-worktrees and local feature branches and remove them.

# STEPS

1. Find all git-worktrees.
2. Find all feature branches.
3. List all findings.
4. Use the AskUserQuestion tool to ask the user if he:
  - Wants to remove ALL of them.
  - Specific worktrees and feature branches. The user should be able to select the ones he wants to remove, NOT type them.
5. Run the clean up based on the user choice.
