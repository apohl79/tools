---
description: Update the a node dependency
argument-hint: [name] [jira-ticket] [version]
---

Update the dependency $1 to version $3. If no version is provided update to latest.

# STEPS

1. Create a git-worktree.
2. Find the requested version and update package.json
3. Run the install script
4. Create a PR with jira ticket $1. If no jira ticket was provided you have to use the AskUserQuestion tool to ask the user.
