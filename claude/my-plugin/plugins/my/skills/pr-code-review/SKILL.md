---
description: Code review a given PR with parloa rules
argument-hint: [pr-link]
---

Load the reviewer recipies and agents as well as production-code, test-code and true-myth recipies (skills) and review the PR that is currently being worked on or the given PR $1.

1. If the PR is already being worked on using a worktree, stay on it.
2. If you need to checkout the the PR and you are on the right project, use a worktree for it.
3. If the PR is on a different project, check it out to /tmp.

After the review, if you found issues, ask the user with the AskUserQuestion tool if:

1. A PR review should be created on github with inline comments and a summary comment
2. The issues should be addressed and the PR should be updated

**NOTE:** For (1) avoid test comments.
