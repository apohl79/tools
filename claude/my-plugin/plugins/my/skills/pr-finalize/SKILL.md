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

Use `/loop 5m` to run the following check-and-fix cycle. Record the **push timestamp** and **HEAD SHA** of the PR before starting.

#### Each iteration

1. **Check** the PR (currently being worked on or the given PR $1):
  - Call `gh api repos/{owner}/{repo}/commits/{HEAD_SHA}/check-runs` — inspect **all** check run statuses including Cursor Bugbot and SonarCloud.
  - **Check for Bugbot review comments** (these are PR review comments, NOT reviews):
    - Call `gh api repos/{owner}/{repo}/pulls/{PR_NUMBER}/comments --jq '.[] | select(.user.login == "cursor[bot]") | {id: .id, body: .body, created: .created_at}'`
    - Also check for unresolved review threads via GraphQL: `gh api graphql -f query='{ repository(owner: "{owner}", name: "{repo}") { pullRequest(number: {PR_NUMBER}) { reviewThreads(first: 50) { nodes { id isResolved comments(first: 1) { nodes { author { login } body } } } } } } }' --jq '.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved == false and .comments.nodes[0].author.login == "cursor")'`
    - IMPORTANT: Bugbot posts inline review comments, NOT reviews. You MUST check `pulls/{pr}/comments` — checking only `pulls/{pr}/reviews` will MISS Bugbot findings.
  - **Check SonarCloud status**: If SonarCloud failed, fetch the full sonarqubecloud PR comment via `gh pr view {PR_NUMBER} --json comments --jq '.comments[] | select(.author.login == "sonarqubecloud") | .body'` and investigate the specific issues. Do NOT dismiss SonarCloud failures as "pre-existing" without evidence.
  - Inspect PR checks for **compliance** failures posted **after** the push timestamp.

2. **Fix** any found bug comments or compliance issues:
  - If the failure comes from a compliance check, inspect the failed job logs and fix the underlying issue in the PR branch.
  - If the PR is already being worked on using a worktree, use the same worktree.
  - If you need to checkout the PR and you are on the right project, use a new worktree.
  - If the PR is on a different project, check it out to /tmp.

3. **Run** all tests locally and confirm passing.

3. **Push** the fixes to the PR. Update the **push timestamp** and **HEAD SHA**.

4. **Reply** to the bug comments and resolve the conversations.
  - Reply to each comment individually on the thread and resolve the conversation thread before moving on.
  - If the work item came from a compliance check rather than a review thread, add a PR comment summarizing what was fixed.

Do NOT skip any step. Report status of each step.

#### Stop condition

Before you tell me this PR is done, list: (1) all unresolved review threads, (2) CI check status for every check, (3) SonarCloud gate status. Only say 'done' if all are green.

Stop the loop when **all** of the following are true:
1. Cursor Bugbot check run `status` is `"completed"`.
2. No new unresolved `cursor` threads after the push timestamp.
3. No new compliance check failures after the push timestamp.
4. SonarCloud Quality Gate is passing (or the failure is verified as pre-existing on the target branch, not introduced by the PR).
