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

### Step 3: Wait for completion

The background job will automatically notify you when it finishes. Do NOT poll,
sleep, or call `TaskOutput` while it is running — you will be notified.

**What "truly done" means:** The monitor will NOT exit until:
1. All CI checks are passing (no failures or pending checks).
2. No new Bugbot comments since the last push.
3. No unresolved review threads.
4. **At least 6 minutes have elapsed since the last fix push** — this ensures Bugbot has had time to analyze the new commit and post any follow-up comments before we declare victory. Without this wait, the monitor could exit while Bugbot is still running.

### Step 4: Report results

When the job finishes:
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
- `failed_checks` — list of check names and their conclusions
- `new_bugbot_comments` — bugbot review comments with path and body
- `unresolved_threads` — unresolved review threads

#### Step 2: Investigate and fix ALL issues

For each issue type:

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
