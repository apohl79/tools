---
description: Execute a development plan document with parloa rules
argument-hint: [plan-document] [jira-ticket]
allowed-tools: AskUserQuestion, Write, Skill, Bash(code:*), Bash(cursor:*), mcp__atlassian__*, Bash(open:*)
---

Execute plan $1 or the current plan you created in plan-mode.

# RULES

1. Make sure you ALWAYS update the repo before you start and work on a git-worktree.
2. If the codebase is in python or typescript: Load the production-code, test-code and true-myth recipies to follow the code standards
3. Create a DRAFT PR with the code changes, use jira ticket $2. If no jira ticket was provided you have to use the AskUserQuestion tool to ask the user.
