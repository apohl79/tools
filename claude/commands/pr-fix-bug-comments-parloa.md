---
description: Fix bug comments on a PR
argument-hint: [pr-link]
---

# Bug Fixer

You are fixing bugs in PRs.

## Step

### Preparation

1. Check the language used in this repository

2. Load language specific skills
  - For **Typescript**: production-code, test-code and true-myth
  - For **Python**: production-code, test-code
  - For **Rust**: production-code, test-code

3. Load the reviewer recipies and agents

### Workflow

1. **Find the bug comments** of the PR that is currently being worked on or the given PR $1.

2. **Finish check:** if you find no bug comment from the cursor bot you are finished.

3. **Fix the bugs**:
  - If the PR is already being worked on using a worktree, use the same worktree
  - If you need to checkout the the PR and you are on the right project, use a new worktree
  - If the PR is on a different project, check it out to /tmp

4. **Push** the fixes to the PR

5. **Reply** to the bug comments and resolve the conversations
  - Reply to each comment individually on the thread and resolve the conversation thread before moving on

6. **Monitor** — wait UNTIL the cursor bug bot has FINISHED. There is NO timeout. You poll until the bot is done.
  - Record the **push timestamp** and the **HEAD SHA** of the PR.
  - **How to detect whether the bot is still running:**
    - Call `gh api repos/{owner}/{repo}/commits/{HEAD_SHA}/check-runs` and look for the check run named **"Cursor Bugbot"**.
    - If its `status` is `"in_progress"` or `"queued"`, **the bot is still running**. You MUST keep polling.
    - The bot is done when `status == "completed"`.
  - **Polling schedule:** sleep 90 seconds between each poll. There is NO maximum number of polls. There is NO timeout.
  - **At each poll, do TWO things:**
    1. Check the "Cursor Bugbot" check run status — if NOT `"completed"`, sleep and poll again. **Do NOT stop early.**
    2. Check `get_review_comments` — look for new unresolved threads from `cursor` posted AFTER the push timestamp. If a new thread appears → the bot found a bug → go to step 3 immediately.
  - **The bot is finished when ALL of these are true:**
    1. The "Cursor Bugbot" check run `status` is `"completed"`.
    2. The last poll found NO new unresolved cursor threads posted after the push timestamp.
  - **If the check run is NOT completed, you MUST keep polling. There is NO timeout. NO minimum wait time substitutes for the check run signal.**
  - **If a new cursor thread appears at ANY poll, stop monitoring and go fix it (step 3). Do NOT wait.**

7. **Iterate**: Go back to step 1
  **No iteration limit:** There is no limit.
