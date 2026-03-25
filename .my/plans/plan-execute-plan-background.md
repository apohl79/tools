# Execute-Plan-Background Skill Implementation Plan

**Goal:** Create a new `execute-plan-background` skill that reads a plan document and produces a JSON execution manifest with all sub-task files written upfront — so the `plan-executor` daemon can run each job as a standalone `claude -p` invocation without interactive prompts or sub-agent calls.
**Type:** Feature
**JIRA:** none
**Tech Stack:** Markdown (skill file only)
**Code Standards:** n/a
**Status:** READY
**no-worktree:** [x]
**no-pr:** [x]
**draft-pr:** [ ]
**merge:** [ ]
**merge-admin:** [ ]

---

## Context

The existing `execute-plan` skill orchestrates a plan by spawning sub-agents (via the `Agent`/`Task` tool) interactively. This works when Claude Code runs interactively but cannot work with `claude -p` single-shot runs as used by the `plan-executor` daemon.

The `execute-plan-background` skill solves this by:
1. Running as a non-interactive `claude -p` session (uses Read/Write/Bash/Glob tools but NOT Agent/Task)
2. Performing task decomposition and creating ALL subtask `.md` files upfront
3. Writing a single JSON manifest at `.my/plans/<plan-name>.exec.json`
4. The manifest describes every job across all execution phases so the plan-executor can execute them sequentially/in-parallel

**Plan-executor invocation (current):**
```
claude --dangerously-skip-permissions -p --verbose --output-format stream-json "/my:execute-plan <path>"
```

**Plan-executor invocation (new):**
```
claude --dangerously-skip-permissions -p --verbose --output-format stream-json "/my:execute-plan-background <path>"
```

**Manifest location:** `.my/plans/plan-<name>.exec.json` (same directory as the plan markdown file).

**Key constraints:**
- Skill MUST NOT use Agent or Task tools (no sub-agent calls)
- Skill MUST NOT ask interactive questions (non-interactive `claude -p` run)
- Review agents (gemini, codex) MUST be called via CLI in the manifest, NOT via MCP
- All subtask files must be created before the manifest is written

## Acceptance Criteria

- [ ] `execute-plan-background` skill exists at `claude/my-plugin/plugins/my/skills/execute-plan-background/SKILL.md`
- [ ] Skill accepts a plan path as `$1` (same as execute-plan)
- [ ] Skill reads the plan, parses all flags and metadata
- [ ] Skill creates a git worktree (unless `--no-worktree`) using Bash directly (NOT via sub-agents)
- [ ] Skill performs task decomposition: waves, dependencies, parallel/sequential grouping
- [ ] All subtask files created upfront at `<worktree-or-repo-root>/.tmp-subtask-*.md`
- [ ] Subtask files include complete prompts: implementation details, dependency context, recipes, scope
- [ ] Manifest written to `.my/plans/plan-<name>.exec.json`
- [ ] Manifest covers: all waves, integration tests (conditional), code review (with codex + gemini CLI commands), plan validation, cleanup/PR
- [ ] Manifest includes exact CLI commands for each job (using `@<absolute-path-to-subtask-file>` syntax)
- [ ] Review subtask files instruct reviewers to write structured findings to specific `.tmp-review-*-findings.md` files
- [ ] Fix job subtask files reference specific findings file paths (so plan-executor can run fix after review without dynamic content)
- [ ] Plan status updated to `EXECUTING` in the plan header
- [ ] Skill outputs the manifest path to stdout on last line: `MANIFEST: <absolute-path>`

---

### Task 1: Create the execute-plan-background skill

**Files:**
- Create: `claude/my-plugin/plugins/my/skills/execute-plan-background/SKILL.md`

This is the sole implementation task. The skill is a Markdown file that Claude reads and executes. No other files need to change.

---

**Step 1: Write the SKILL.md**

Create the file at `/Users/andreas.pohl/tools/claude/my-plugin/plugins/my/skills/execute-plan-background/SKILL.md` with the following complete content:

````markdown
---
description: Read a plan document and produce a JSON execution manifest for the plan-executor daemon — creates all subtask files upfront, no sub-agents
argument-hint: [plan-document]
---

You are generating an execution manifest for the `plan-executor` daemon.

**CRITICAL CONSTRAINTS — READ FIRST:**
- You MUST NOT use the `Agent` or `Task` tool. Sub-agent calls are FORBIDDEN.
- You MUST NOT call `AskUserQuestion`. This is a non-interactive run.
- You CAN use: Read, Write, Edit, Bash, Glob, Grep tools.
- You write files and produce output. You do NOT execute the plan yourself.

---

# PHASE 1: RESOLVE AND READ THE PLAN

1. **Resolve the plan path:**
   - If `$1` was provided, use that path directly (resolve to absolute if relative).
   - If `$1` was NOT provided, search `.my/plans/` for any `plan-*.md` file where the header contains `**Status:** READY`. Use the first one found.
   - If no plan found, print `ERROR: No plan path provided and no READY plan found.` and stop.

2. **Read the plan document fully.** Extract:
   - `PLAN_TITLE` — from the `# <Title>` heading
   - `PLAN_TYPE` — from `**Type:** <value>` (e.g., `Feature`, `Bug fix`, `Deployment / Infra change`, `Research`)
   - `JIRA_TICKET` — from `**JIRA:** <value>` (use `VC-0` if `none` or absent)
   - `TECH_STACK` — from `**Tech Stack:** <value>`
   - `CODE_STANDARDS` — from `**Code Standards:** <value>` (comma-separated list of recipe skill names, or `n/a`)
   - `SKIP_CODE_REVIEW` — set `true` if PLAN_TYPE is `Deployment / Infra change` or `Research`
   - `SKIP_PR` — set `true` if PLAN_TYPE is `Deployment / Infra change` or `Research`
   - Execution flags — check each flag field (a flag is enabled when its value is `[x]`):
     - `**no-worktree:** [x]` → `NO_WORKTREE=true`
     - `**no-pr:** [x]` → `SKIP_PR=true`
     - `**draft-pr:** [x]` → `DRAFT_PR=true`
     - `**merge:** [x]` → `MERGE=true`
     - `**merge-admin:** [x]` → `MERGE_ADMIN=true`
   - `ACCEPTANCE_CRITERIA` — all `- [ ] ...` lines under `## Acceptance Criteria`
   - All tasks and their descriptions

3. **Determine the repo root:** Use `git -C <plan-dir> rev-parse --show-toplevel` to find it.

4. **Determine working root:**
   - If `NO_WORKTREE=true`: `WORK_ROOT = <repo-root>`
   - Otherwise: `WORK_ROOT = <repo-root>/.my/worktrees/<repo-name>-<short-plan-title-no-spaces>-<JIRA_TICKET>` (use only alphanumeric, hyphens, underscores in the dir name)

5. **Mark plan as executing:** Update the plan file header — replace `**Status:** READY` with `**Status:** EXECUTING`. (If the status is `WIP`, also change it to `EXECUTING`.)

---

# PHASE 2: SETUP

Unless `NO_WORKTREE=true`:

1. **Sync the main repo:**
   ```bash
   git -C <repo-root> pull
   ```

2. **Check if worktree already exists:**
   ```bash
   git -C <repo-root> worktree list
   ```
   - If the worktree path already exists: rebase its branch onto the latest default branch:
     ```bash
     cd <worktree-path>
     git fetch origin
     git rebase origin/main  # or origin/master
     git push --force-with-lease
     ```
   - If it does NOT exist: create it:
     ```bash
     BRANCH="feature/<JIRA_TICKET>-<short-title-kebab-case>"
     git -C <repo-root> worktree add -b "$BRANCH" "<WORK_ROOT>" origin/main
     ```
     If `origin/main` does not exist, try `origin/master`.

3. Record `BRANCH_NAME` for the manifest.

If `NO_WORKTREE=true`: set `WORK_ROOT = <repo-root>`, `BRANCH_NAME = <current-branch-from-git-rev-parse>`.

---

# PHASE 3: TASK DECOMPOSITION

Analyze all tasks in the plan document. For each task, produce a **complete sub-task description** that a standalone `claude -p` session can execute with NO access to the plan document.

Each sub-task description MUST include:
- **All implementation details** — copy every relevant code snippet, file path, interface definition, type, constant, config value, and acceptance criterion that pertains to this task. Do NOT summarize.
- **Dependency context** — for tasks with dependencies, include a summary of what earlier tasks produce: file paths, exported function signatures, type names, config values. The sub-agent cannot look up the original plan.
- **Scope boundary** — explicitly state what is OUT OF SCOPE for this task.
- **Testing expectations** — state whether the sub-agent must write tests. If the task is tightly coupled and only testable in integration, write `Tests deferred to integration test task`.
- **Code standard recipes** — list exact skill names to load before writing code.
- **Verification step** — the command to run to verify the task succeeded (build, lint, test, or a specific test file).
- **Commit message** — a conventional commit message for this task.

**Wave assignment rules:**
- **Wave 1**: tasks with NO dependencies on other tasks in the plan
- **Wave N**: tasks whose ALL dependencies are in waves 1 through N-1
- Within a wave: tasks touching different files with no shared dependencies → mark `parallel: true`; tasks modifying the same files → mark `parallel: false` (run sequentially within the wave)
- Maximum 5 parallel tasks per wave

Record the wave assignment and parallelism for each task.

**Integration test detection:**
- If any task includes `Tests deferred to integration test task`, set `NEEDS_INTEGRATION_TESTS=true`.
- Collect the list of modules/functions that need integration tests.

---

# PHASE 4: WRITE ALL SUBTASK FILES

Write all subtask files to `<WORK_ROOT>/` before writing the manifest. Use Write tool for each file.

**File naming convention:**
- Implementation tasks: `.tmp-subtask-<N>.md` (N = 1, 2, 3, …)
- Integration tests: `.tmp-subtask-integration-tests.md`
- Code review: `.tmp-subtask-code-review.md`
- Code review fix: `.tmp-subtask-review-fixes.md`
- Plan validation: `.tmp-subtask-plan-validation.md`
- Validation fix: `.tmp-subtask-validation-fixes.md`
- Cleanup/PR: `.tmp-subtask-cleanup-pr.md`

## 4.1 Implementation sub-task files

For each task, write `<WORK_ROOT>/.tmp-subtask-<N>.md`:

```
You are a focused development agent. Your ONLY job is to implement the task described below.

RULES:
- Implement EXACTLY what is described. Nothing more, nothing less.
- Do NOT look for, read, or reference any plan document, roadmap, or other task files.
- Do NOT implement functionality beyond what is described here.
- Load the code standard recipes listed below BEFORE writing any code, by using the Skill tool.
- Write clean, production-quality code following the loaded recipes.
- [Write tests | Tests are deferred to the integration test task — do NOT write tests for this task.]
- After finishing, run the verification command and confirm it passes.
- Working directory: <WORK_ROOT>

## Code Standard Recipes to Load
<list recipe skill names, or "n/a">

## Task: <task title>

<FULL task description with all implementation details, code snippets, file paths, types, config values — copy verbatim from the plan's task section>

## Dependency Context
<For tasks with dependencies: describe what earlier tasks produced. Include file paths, exported function signatures, type names.>

## Out of Scope
<explicitly list what this task must NOT touch>

## Verification
Run: `<build/lint/test command>`
Expected: <expected output or "exits 0">

## Commit
<conventional commit message>
```

## 4.2 Integration test sub-task file (if NEEDS_INTEGRATION_TESTS=true)

Write `<WORK_ROOT>/.tmp-subtask-integration-tests.md`:

```
You are a focused test agent. Your ONLY job is to write integration tests for the modules described below.

RULES:
- Write integration tests ONLY. Do NOT modify production code.
- Load the test-code recipe listed below BEFORE writing any tests.
- Working directory: <WORK_ROOT>

## Code Standard Recipes to Load
<test-code recipe for the detected language>

## Modules Needing Integration Tests
<list each module with its file path, function signatures, and the test scenarios that validate modules working together>

## Verification
Run: `<full test suite command>`
Expected: all tests pass

## Commit
test(<scope>): add integration tests
```

## 4.3 Code review subtask file

Write `<WORK_ROOT>/.tmp-subtask-code-review.md`:

```
You are a code review agent. Review all listed files for bugs, code quality issues, and recipe compliance.

RULES:
- Load ALL listed code standard recipes using the Skill tool BEFORE reviewing.
- Read every file listed below.
- Check for: bugs, logic errors, missing error handling, type safety issues, naming violations, recipe non-compliance, test quality, security concerns.
- Do NOT modify any files. You are a reviewer only.
- Write your findings to the file: <WORK_ROOT>/.tmp-review-claude-findings.md
- Use this exact format for findings:

  ## Code Review Findings

  **Status:** Clean | Issues Found

  ### CRITICAL
  - [file:line] issue description — concrete fix suggestion

  ### IMPORTANT
  - [file:line] issue description — concrete fix suggestion

  ### MINOR
  - [file:line] issue description — concrete fix suggestion

  If no issues: write "No issues found." under each section.

## Code Standard Recipes to Load
<list recipe skill names>

## Files to Review
<list all files created or modified during implementation, with absolute paths>
```

Write **two separate files** for codex and gemini — they differ only in the findings output path:

**`<WORK_ROOT>/.tmp-subtask-code-review-codex.md`** (for codex):

```
Review the code changes for bugs, logic errors, and code quality issues.

Run `git -C <WORK_ROOT> diff origin/main..HEAD` (or `git diff HEAD~<N>..HEAD` for N commits) to get the diff.
Review the output for bugs, logic errors, naming issues, missing error handling, and code quality problems.

Write your findings to the file: <WORK_ROOT>/.tmp-review-codex-findings.md
Use this exact format:

## Code Review Findings

**Status:** Clean | Issues Found

### CRITICAL
- [file:line] issue description — concrete fix suggestion

### IMPORTANT
- [file:line] issue description — concrete fix suggestion

### MINOR
- [file:line] issue description — fix suggestion

If no issues: write "No issues found." under each section.

## Files to Review
<list all modified files with absolute paths>
```

**`<WORK_ROOT>/.tmp-subtask-code-review-gemini.md`** (for gemini — identical content except findings path):

```
Review the code changes for bugs, logic errors, and code quality issues.

Run `git -C <WORK_ROOT> diff origin/main..HEAD` (or `git diff HEAD~<N>..HEAD` for N commits) to get the diff.
Review the output for bugs, logic errors, naming issues, missing error handling, and code quality problems.

Write your findings to the file: <WORK_ROOT>/.tmp-review-gemini-findings.md
Use this exact format:

## Code Review Findings

**Status:** Clean | Issues Found

### CRITICAL
- [file:line] issue description — concrete fix suggestion

### IMPORTANT
- [file:line] issue description — concrete fix suggestion

### MINOR
- [file:line] issue description — fix suggestion

If no issues: write "No issues found." under each section.

## Files to Review
<list all modified files with absolute paths>
```

Note: The skill runs BEFORE implementation, so no diff exists at manifest generation time. The codex/gemini subtask files instruct reviewers to run `git diff` themselves at review time.

## 4.4 Code review fix subtask file

Write `<WORK_ROOT>/.tmp-subtask-review-fixes.md`:

```
You are a code fix agent. Fix all CRITICAL and IMPORTANT issues found by the code reviewers.

RULES:
- Read the findings files listed below.
- Fix ONLY the CRITICAL and IMPORTANT issues. Do NOT fix MINOR issues.
- Do NOT change anything that is not listed as a CRITICAL or IMPORTANT issue.
- Do NOT look for or read any plan document.
- Load the code standard recipes listed below BEFORE making any changes.
- After fixing, run the verification command to confirm no regressions.
- Working directory: <WORK_ROOT>

## Code Standard Recipes to Load
<list recipe skill names>

## Review Findings Files to Read
- <WORK_ROOT>/.tmp-review-claude-findings.md
- <WORK_ROOT>/.tmp-review-codex-findings.md (read if it exists)
- <WORK_ROOT>/.tmp-review-gemini-findings.md (read if it exists)

## Verification
Run: `<build/lint/test command>`
Expected: exits 0

## Commit
fix(<scope>): address code review findings
```

## 4.5 Plan validation subtask file

Write `<WORK_ROOT>/.tmp-subtask-plan-validation.md`:

```
You are a plan validation agent. Verify that the development plan has been FULLY and CORRECTLY implemented.

RULES:
- Read the plan specification below top to bottom.
- Read every implementation file listed below.
- For EACH requirement, feature, behavior, interface, type, config value, and acceptance criterion in the plan: verify it exists and works as specified.
- Do NOT modify any files. You are a validator only.
- Write your validation report to: <WORK_ROOT>/.tmp-validation-findings.md

Use this exact format:

## Validation Report

**STATUS:** PASS | FAIL

### IMPLEMENTED
- [requirement]: [file:line where implemented]

### GAPS
- [requirement quoted from plan]: [what is actually implemented or "missing entirely"] — [file path] — [what needs to be done]

### DEVIATIONS
- [description of deviation] — [acceptable | problematic]

## Plan Specification

<PASTE THE FULL PLAN CONTENT VERBATIM HERE — every section, every task, every acceptance criterion>

## Files to Validate (All Files Created or Modified)
<list absolute paths of all files that implementation tasks will create or modify — derive this from the plan's "Files:" sections>
```

## 4.6 Validation fix subtask file

Write `<WORK_ROOT>/.tmp-subtask-validation-fixes.md`:

```
You are a gap-fix agent. Fix all GAPS identified in the plan validation report.

RULES:
- Read the validation findings file listed below.
- Fix ONLY the GAPS. Do NOT change anything else.
- Do NOT look for or read any plan document.
- Load the code standard recipes listed below BEFORE making any changes.
- After fixing, run the verification command.
- Working directory: <WORK_ROOT>

## Code Standard Recipes to Load
<list recipe skill names>

## Validation Findings File
<WORK_ROOT>/.tmp-validation-findings.md

## Verification
Run: `<build/lint/test command>`
Expected: exits 0

## Commit
fix(<scope>): address plan validation gaps
```

## 4.7 Cleanup/PR subtask file

Write `<WORK_ROOT>/.tmp-subtask-cleanup-pr.md`:

```
You are a cleanup and PR agent. Finalize the implementation and create a pull request.

RULES:
- Do NOT modify any production or test code.
- Working directory: <WORK_ROOT>

## Steps

1. Verify ALL `.tmp-subtask-*.md` and `.tmp-review-*-findings.md` and `.tmp-validation-findings.md` files are deleted. Delete any that remain.

2. Update plan status: in `<absolute-path-to-plan.md>`, replace `**Status:** EXECUTING` with `**Status:** COMPLETED`.

3. Run the full build, lint, and test pipeline:
   `<build-and-test-command>`
   If it fails, fix the issue before proceeding.

4. <IF SKIP_PR: skip steps 4-5. Print "Skipping PR creation (SKIP_PR=true). Implementation complete." and stop.>

5. Commit all changes:
   ```
   git -C <WORK_ROOT> add -A
   git -C <WORK_ROOT> commit -m "feat(<scope>): <summary of implementation> [<JIRA_TICKET>]"
   ```

6. Push and create PR:
   ```
   git -C <WORK_ROOT> push -u origin <BRANCH_NAME>
   ```
   Then create <IF DRAFT_PR: a DRAFT | a ready-for-review> PR using:
   ```
   gh pr create <IF DRAFT_PR: --draft> --title "<JIRA_TICKET>: <plan title>" --body "$(cat <<'EOF'
   ## Summary
   <bullet points summarizing what was implemented>

   ## JIRA
   <JIRA_TICKET>

   🤖 Generated with execute-plan-background
   EOF
   )"
   ```

7. <IF NOT DRAFT_PR: Mark PR ready and run the `my:pr-finalize` skill:
   Use the Skill tool to invoke `my:pr-finalize` and wait for Bugbot to complete.>

8. Print the PR URL.
```

---

# PHASE 5: WRITE THE JSON MANIFEST

After all subtask files are written, write the manifest JSON to `<plan-dir>/plan-<name>.exec.json`.

The manifest file name is derived from the plan file name: if the plan is `plan-foo-bar.md`, the manifest is `plan-foo-bar.exec.json`.

The manifest schema:

```json
{
  "version": "1",
  "plan_path": "<absolute-path-to-plan.md>",
  "plan_title": "<PLAN_TITLE>",
  "jira": "<JIRA_TICKET>",
  "tech_stack": "<TECH_STACK>",
  "code_standards": ["<recipe1>", "<recipe2>"],
  "flags": {
    "no_worktree": <NO_WORKTREE>,
    "no_pr": <SKIP_PR>,
    "draft_pr": <DRAFT_PR>,
    "merge": <MERGE>,
    "merge_admin": <MERGE_ADMIN>,
    "skip_code_review": <SKIP_CODE_REVIEW>
  },
  "worktree": {
    "path": "<WORK_ROOT>",
    "branch": "<BRANCH_NAME>",
    "repo_root": "<repo-root>"
  },
  "phases": [
    /* Wave phases — one per wave */
    {
      "id": "wave-<N>",
      "type": "implementation_wave",
      "wave_number": <N>,
      "parallel": <true|false>,
      "max_concurrent": 5,
      "jobs": [
        {
          "id": "task-<N>",
          "type": "claude",
          "description": "<task title from plan>",
          "subtask_file": "<WORK_ROOT>/.tmp-subtask-<N>.md",
          "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-<N>.md",
          "working_dir": "<WORK_ROOT>"
        }
        /* additional jobs for parallel tasks */
      ]
    },

    /* Integration test phase — only if NEEDS_INTEGRATION_TESTS=true */
    {
      "id": "integration-tests",
      "type": "integration_tests",
      "conditional": true,
      "parallel": false,
      "jobs": [
        {
          "id": "integration-tests",
          "type": "claude",
          "description": "Write integration tests for deferred test cases",
          "subtask_file": "<WORK_ROOT>/.tmp-subtask-integration-tests.md",
          "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-integration-tests.md",
          "working_dir": "<WORK_ROOT>"
        }
      ]
    },

    /* Code review phase — only if SKIP_CODE_REVIEW=false */
    {
      "id": "code-review",
      "type": "code_review",
      "parallel_review": true,
      "max_iterations": 3,
      "review_jobs": [
        {
          "id": "review-claude",
          "type": "claude",
          "description": "Recipe-based code review",
          "subtask_file": "<WORK_ROOT>/.tmp-subtask-code-review.md",
          "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-code-review.md",
          "working_dir": "<WORK_ROOT>",
          "findings_file": "<WORK_ROOT>/.tmp-review-claude-findings.md"
        },
        {
          "id": "review-codex",
          "type": "codex",
          "description": "Codex general code quality review",
          "subtask_file": "<WORK_ROOT>/.tmp-subtask-code-review-codex.md",
          "command": "codex --quiet --model o4-mini --approval-policy on-failure --output-format stream-json @<WORK_ROOT>/.tmp-subtask-code-review-codex.md",
          "working_dir": "<WORK_ROOT>",
          "findings_file": "<WORK_ROOT>/.tmp-review-codex-findings.md"
        },
        {
          "id": "review-gemini",
          "type": "gemini",
          "description": "Gemini code quality review",
          "subtask_file": "<WORK_ROOT>/.tmp-subtask-code-review-gemini.md",
          "command": "gemini --yolo @<WORK_ROOT>/.tmp-subtask-code-review-gemini.md",
          "working_dir": "<WORK_ROOT>",
          "findings_file": "<WORK_ROOT>/.tmp-review-gemini-findings.md"
        }
      ],
      "fix_job": {
        "id": "review-fix",
        "type": "claude",
        "description": "Fix CRITICAL and IMPORTANT review findings",
        "subtask_file": "<WORK_ROOT>/.tmp-subtask-review-fixes.md",
        "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-review-fixes.md",
        "working_dir": "<WORK_ROOT>"
      }
    },

    /* Plan validation phase */
    {
      "id": "plan-validation",
      "type": "plan_validation",
      "max_iterations": 5,
      "validation_job": {
        "id": "plan-validation",
        "type": "claude",
        "description": "Validate plan fully implemented against acceptance criteria",
        "subtask_file": "<WORK_ROOT>/.tmp-subtask-plan-validation.md",
        "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-plan-validation.md",
        "working_dir": "<WORK_ROOT>",
        "findings_file": "<WORK_ROOT>/.tmp-validation-findings.md"
      },
      "fix_job": {
        "id": "validation-fix",
        "type": "claude",
        "description": "Fix gaps identified in plan validation",
        "subtask_file": "<WORK_ROOT>/.tmp-subtask-validation-fixes.md",
        "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-validation-fixes.md",
        "working_dir": "<WORK_ROOT>"
      }
    },

    /* Cleanup and PR phase */
    {
      "id": "cleanup-pr",
      "type": "cleanup_pr",
      "parallel": false,
      "jobs": [
        {
          "id": "cleanup-pr",
          "type": "claude",
          "description": "Delete temp files, update plan status, commit, push, create PR",
          "subtask_file": "<WORK_ROOT>/.tmp-subtask-cleanup-pr.md",
          "command": "claude --dangerously-skip-permissions -p --verbose --output-format stream-json @<WORK_ROOT>/.tmp-subtask-cleanup-pr.md",
          "working_dir": "<WORK_ROOT>"
        }
      ]
    }
  ]
}
```

**Important notes on manifest generation:**

- If `SKIP_CODE_REVIEW=true`: omit the `code-review` phase from `phases`.
- If `NEEDS_INTEGRATION_TESTS=false`: omit the `integration-tests` phase from `phases`.
- If `SKIP_PR=true`: keep `cleanup-pr` phase but its subtask file should skip PR steps (the flag is reflected in the subtask content).
- All paths in the manifest MUST be absolute paths (not relative).
- The `command` field contains the exact shell command the plan-executor will run via `std::process::Command` or equivalent.

---

# PHASE 6: OUTPUT

After writing the manifest:

1. Print a summary to stdout:
   ```
   Execution manifest generated:
   - Plan: <plan title>
   - Waves: <N> implementation wave(s), <M> total implementation tasks
   - Integration tests: <yes | no>
   - Code review: <yes | skipped (Deployment/Research)>
   - Plan validation: yes
   - Cleanup/PR: <yes | skipped>
   - Subtask files written: <list all .tmp-subtask-*.md files created>

   MANIFEST: <absolute-path-to-exec-json>
   ```

2. The last line MUST be exactly `MANIFEST: <absolute-path>` so the plan-executor can parse it from the stream-json output.
````

**Step 2: Verify**

Run:
```
ls -la /Users/andreas.pohl/tools/claude/my-plugin/plugins/my/skills/execute-plan-background/SKILL.md
```
Expected: file exists with non-zero size.

**Step 3: Commit**

(No commit needed — `--no-pr` is set for this plan. The skill file is added to git normally.)

---

## Task Dependency Graph

```
Task 1 (only task)
```

## Open Questions

- **Codex command flags**: The manifest uses `codex --quiet --model o4-mini --approval-policy on-failure --output-format stream-json @<file>` — if the codex CLI does not support `--output-format stream-json`, this flag may need to be removed. The implementing agent should check `codex --help` first.
- **Gemini command flags**: The manifest uses `gemini --yolo @<file>` — verify this is the correct syntax for non-interactive gemini CLI invocation.
- **Codex CLI command**: The manifest uses `codex --quiet --model o4-mini --approval-policy on-failure --output-format stream-json @<file>` — check `codex --help` first and adjust flags if needed (e.g., some versions use `codex exec` subcommand or lack `--output-format stream-json`).
