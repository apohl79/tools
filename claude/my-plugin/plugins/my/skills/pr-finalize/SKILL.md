---
description: Fix bug comments on a PR
argument-hint: [pr-link] [--fix]
---

# PR Finalizer

Finalizes PRs by fixing ALL issues — Bugbot comments, compliance checks, CI failures,
lint errors, SonarCloud findings, and any other failing check.

## Mode Detection

This skill operates in two modes based on arguments:

- **No `--fix`** → **Launcher mode**: starts a background monitor script that polls
  the PR and dispatches non-interactive Claude sessions for fixes. Token-efficient.
- **`--fix`** → **Fixer mode**: called by the monitor script to fix specific issues.

---

## Launcher Mode (default)

### Step 1: Identify the PR

Parse the PR link from $1. If not provided, detect from current branch:
```
gh pr view --json number,headRefName,url
```

Extract owner, repo, PR number, HEAD SHA:
```
PR_NUMBER=$(gh pr view --json number --jq '.number')
HEAD_SHA=$(gh pr view --json headRefOid --jq '.headRefOid')
OWNER=$(gh repo view --json owner --jq '.owner.login')
REPO=$(gh repo view --json name --jq '.name')
```

### Step 2: Launch the monitor script

The monitor script is in the same directory as this skill. Locate it:
```
SKILL_DIR — find via the plugin cache path for my:pr-finalize
```

Create temp files for summary and log:
```
SUMMARY_FILE=$(mktemp /tmp/pr-finalize-summary-XXXXX.md)
LOG_FILE=$(mktemp /tmp/pr-finalize-log-XXXXX.txt)
```

Launch the monitor as a background Bash job:
```bash
bash <skill-dir>/pr-monitor.sh \
  --owner "$OWNER" \
  --repo "$REPO" \
  --pr "$PR_NUMBER" \
  --head-sha "$HEAD_SHA" \
  --push-time "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  --workdir "$(pwd)" \
  --summary-file "$SUMMARY_FILE" \
  --log-file "$LOG_FILE"
```

Run this via `Bash` tool with `run_in_background: true`.

### Step 3: Respond immediately — do NOT wait

After launching the background Bash job, **stop and respond to the user right now**:

> "Monitor running for PR #N. Waiting for it to finish."

Then go idle. **Do NOT call `TaskOutput`. Do NOT sleep. Do NOT poll.**
You will receive a `<task-notification>` when the job completes.

**Red flags — if you find yourself doing any of these, stop immediately:**
- Calling `TaskOutput` with `block: true`
- Calling `TaskOutput` at all before receiving a notification
- Adding a sleep or delay
- Saying "let me check the status"

The monitor handles all polling internally. Your job ends at launching it.

**What "truly done" means (the monitor handles this, not you):**
1. All CI checks passing.
2. No new Bugbot comments since the last push.
3. No unresolved review threads.
4. At least 6 minutes elapsed since the last fix push.

### Step 4: Report results — only after receiving task-notification

When you receive a `<task-notification>` for the background job:
1. Check the exit code. 0 = all checks green, truly done.
2. Read `$SUMMARY_FILE` and display its contents to the user.
3. If exit code is non-zero, inform the user that the PR still has issues and
   show the summary of what was attempted.
4. Clean up temp files.

---

## Fixer Mode (`--fix`)

When invoked with `--fix`, you are running inside a non-interactive Claude session
dispatched by the monitor script. The prompt contains the specific issues to fix.

### Preparation

1. Check the language used in this repository

2. Load language specific skills
  - For **Typescript**: production-code, test-code and true-myth
  - For **Python**: production-code, test-code
  - For **Rust**: production-code, test-code

### Workflow

#### Step 1: Parse the issue description

The prompt contains JSON describing:
- `merge_conflicts` — `{"conflicting": true, "base_branch": "main"}` if the PR has merge conflicts
- `failed_checks` — list of check names and their conclusions
- `new_bugbot_comments` — bugbot review comments with path and body
- `unresolved_threads` — unresolved review threads

#### Step 2: Investigate and fix ALL issues

**Merge conflicts (handle FIRST, before anything else):**

If `merge_conflicts.conflicting` is `true`:
1. Identify the base branch from `merge_conflicts.base_branch`
2. Fetch the base branch and rebase:
   ```bash
   git fetch <remote> <base_branch>
   git rebase <remote>/<base_branch>
   ```
3. If there are conflicts, resolve them:
   - For each conflicted file, inspect the conflict markers and apply the correct resolution
   - Keep the intent of BOTH sides where possible; prefer the PR branch's changes for code it owns
   - `git add <resolved_file>` after resolving each file
   - `git rebase --continue`
4. Force-push the rebased branch:
   ```bash
   git push --force-with-lease
   ```
5. Continue with remaining issues below (failed checks, comments, threads)

For each other issue type:

**Failed checks:**
- Inspect the check's job logs: `gh api repos/{owner}/{repo}/actions/runs/{RUN_ID}/jobs --jq '.jobs[] | select(.conclusion == "failure") | .id'`
- Download logs: `gh api repos/{owner}/{repo}/actions/jobs/{JOB_ID}/logs`
- For Semgrep: download the report artifact
- For SonarCloud: check the bot comment via `gh pr view {PR} --json comments --jq '.comments[] | select(.author.login == "sonarqubecloud") | .body'`
- Fix the code

**Bugbot comments:**
- Read the comment body to understand what needs fixing
- Fix the code at the indicated path/line

**Unresolved threads:**
- Read each thread's comment to understand the request
- Fix the code

#### Step 3: Run all tests locally and confirm passing

#### Step 4: Push the fixes

Commit and push. Use a conventional commit message describing what was fixed.

#### Step 5: Reply to review comments and resolve threads

**This step is mandatory — unresolved threads cause the monitor to loop forever.**

For every Bugbot comment or review thread you addressed:
1. Reply to the thread explaining what was fixed
2. **Resolve the conversation** — use `gh api` to mark it resolved:
   ```bash
   gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "THREAD_ID"}) { thread { isResolved } } }'
   ```
3. Verify it is resolved before finishing — the monitor checks `isResolved == false` to detect remaining issues

- If fixes came from compliance checks, add a PR comment summarizing what was fixed

### Anti-patterns (DO NOT do these)

- Do NOT only check Bugbot and ignore Semgrep/SonarCloud/other checks
- Do NOT dismiss compliance failures without investigating the logs
- Do NOT assume a failing check is "pre-existing" without evidence from the target branch
- Do NOT open any URLs in a browser (Playwright or WebFetch) — use `gh` CLI or the GitHub MCP tools exclusively for all GitHub operations (PR views, check logs, comments, workflow runs)
- Do NOT leave review threads unresolved after fixing — the monitor will re-trigger on the same threads if they stay open
