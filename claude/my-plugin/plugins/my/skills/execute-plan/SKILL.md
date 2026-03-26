---
name: execute-plan
description: Use when a READY implementation plan should be executed in the current repository or worktree.
argument-hint: [plan-document] [jira-ticket] [--non-interactive] [--no-worktree] [--no-pr] [--draft-pr]
---

**PLANNING MODE CHECK (execute FIRST, before anything else):**
If you are currently in planning mode (i.e., you have access to `ExitPlanMode`), STOP IMMEDIATELY. Do NOT proceed with any phase of this command. Inform the user:
"This command cannot run in planning mode. Please exit planning mode first, then re-run this command with an explicit existing plan path. If no READY plan exists yet, create it first through the planning workflow."
Do NOT attempt to exit planning mode yourself — let the user handle it.
**CRITICAL: Do NOT call `ExitPlanMode`. Do NOT call `AskUserQuestion` about exiting plan mode. Simply print the message above and STOP. Take NO further action.**

---

Execute the explicitly provided plan path, or in interactive mode resolve a single READY plan during Phase 1 setup.

You are the ORCHESTRATOR. You coordinate the execution of a development plan by delegating individual tasks to focused sub-agents. You NEVER write production code or test code yourself — you only manage the workflow.

# PHASE 1: SETUP

1. **Establish execution mode first:**
   - Default mode is the current interactive behavior.
   - If `--non-interactive` is present, set `NON_INTERACTIVE=true`; otherwise set `NON_INTERACTIVE=false`.
   - When `NON_INTERACTIVE=true`, Phase 1 MUST NOT call `AskUserQuestion`.
   - When `NON_INTERACTIVE=true`, later phases MUST branch on this mode.
   - Non-interactive mode means no Task/Agent sub-agent launches and no `AskUserQuestion`; instead, emit deterministic handoff instructions and wait for continuation input.
2. **Resolve the plan document:**
   - If $1 was provided, use that path directly.
   - If $1 was NOT provided:
     a. In non-interactive mode (`NON_INTERACTIVE=true`), do NOT scan for candidates. Print this deterministic error and stop: `Non-interactive mode requires a valid explicit plan path. Re-run /my:execute-plan with [plan-document] that points to an existing READY plan and try again.`
     b. In normal interactive mode (`NON_INTERACTIVE=false`), search for READY plans by checking `.my/plans/` for any `plan-*.md` files where the header contains `**Status:** READY`. List them all. Do NOT list plans with any other status (WIP, EXECUTING, COMPLETED).
      c. In normal interactive mode (`NON_INTERACTIVE=false`), if multiple READY plans are found, sort them deterministically by filename and present them through AskUserQuestion in batches that respect the 4-option limit:
        - For each non-final batch, show the next 2 candidate files, then `Show more READY plans`, then `Other / enter path manually`.
        - For the final batch, show the remaining candidate files (up to 3), then `Other / enter path manually` as the last choice.
        - Each candidate option uses label = filename and description = first line of the file or the Goal field if present.
       - Repeat until the user chooses a candidate or selects manual path entry.
     d. In normal interactive mode (`NON_INTERACTIVE=false`), if exactly one READY plan is found, proceed with it automatically (announce which file you chose).
     e. In normal interactive mode (`NON_INTERACTIVE=false`), if none are found, ask the user to provide a plan path.
   - Before continuing in non-interactive mode, verify that the explicit plan path resolves cleanly and that no remaining Phase 1 branch requires clarification; if clarification would be needed for any reason, print the same deterministic error and stop.
   - Read the chosen plan document fully and understand all tasks, their dependencies, and their order.
   - **Deterministically validate executability before continuing Phase 1**:
     - If the chosen file does not exist or cannot be read, stop with a deterministic error.
     - The chosen file MUST satisfy the executable plan document format expected by this skill: it must include the required plan headers (at minimum `**Goal:**`, `**Type:**`, and `**Status:**`), plus executable task sections that this skill can follow deterministically. Missing execution-flag headers in an otherwise valid READY legacy plan MUST be treated as unchecked defaults (`[ ]`) rather than as a rejection reason. If any other required elements are missing, stop immediately with a deterministic error instead of proceeding to any later Phase 1 step.
    - The chosen file MUST currently contain `**Status:** READY`; otherwise stop immediately and print: `Selected plan is not READY. Re-run /my:plan to finish and explicitly accept the plan, then try /my:execute-plan again with a READY plan.`
     - This READY validation applies even when the user supplied an explicit plan path.
    - **Detect the plan type** from the `**Type:**` header field. If the type is `Deployment / Infra change` or `Research`, set `SKIP_CODE_REVIEW=true` and `SKIP_PR=true` for the rest of execution.
   - **Read execution flags from plan header**: Treat any missing execution-flag header as unchecked (`[ ]`). For present headers, a flag is enabled when its value is `[x]`. Command-line arguments always take precedence over plan header flags:
     - `**no-worktree:** [x]` → treat as `--no-worktree`
     - `**no-pr:** [x]` → skip push/PR/finalization steps only; do NOT suppress the local commit step unless `SKIP_PR=true` was already set by plan type (`Deployment / Infra change` or `Research`)
     - `**draft-pr:** [x]` → treat as `--draft-pr`
3. **CRITICAL — sync before starting:**
   - Run `git fetch origin --prune` in the repository root. Do NOT use `git pull` for this preflight sync.
   - Resolve the repository default branch deterministically in this order:
     1. read `refs/remotes/origin/HEAD` and extract the target branch name when it exists;
     2. otherwise, if `refs/remotes/origin/main` exists, use `main`;
     3. otherwise, if `refs/remotes/origin/master` exists, use `master`;
     4. otherwise, print a deterministic error and stop: `Could not determine the default branch from origin/HEAD, origin/main, or origin/master. Re-run /my:execute-plan with the repository synced and try again.`
   - Determine the jira ticket for the branch/worktree name:
     - If $2 was provided, use that.
     - Otherwise read the `**JIRA:**` field from the plan header.
     - If the plan's JIRA field is `none` or absent, use `VC-0` — do NOT ask the user.
   - Unless `--no-worktree` is in effect, the expected worktree path is `.my/worktrees/[repo_name]-[short_title_without_spaces]-[jira_ticket]` under the repository root.
   - **If the expected worktree already exists**: reuse it. Do NOT recreate it. Before any fetch or rebase, run `git status --short` in that worktree. If it shows tracked or untracked changes, stop immediately and print: `Existing worktree is dirty. Clean or stash the worktree at <worktree-path> before re-running /my:execute-plan.`
     If later phases do NOT require a normal push/PR path (that is, `--no-pr` is in effect or `SKIP_PR=true`), this remote-branch guard does NOT apply and MUST NOT block the run.
    If the reused worktree branch already exists on origin before the sync step, later phases still require a normal push/PR path (that is, `--no-pr` is not in effect and `SKIP_PR!=true`), AND the planned sync would leave the local branch needing a rewritten-history push after rebase, stop before execution and print: `Existing worktree branch already exists on origin and rebasing it onto the latest default branch would require a rewritten-history push, which /my:execute-plan will not do automatically. Choose a fresh worktree branch or handle the branch manually, then re-run /my:execute-plan.`
     Then run:
     ```bash
     cd <worktree-path>
     git fetch origin
     git rebase origin/<resolved-default-branch>
     ```
     If `git fetch origin` fails, stop immediately and print: `Failed to fetch origin for existing worktree at <worktree-path>. Resolve the fetch failure and re-run /my:execute-plan.`
     If `git rebase origin/<resolved-default-branch>` fails for any reason, stop immediately after leaving the worktree in the post-failure state and print: `Failed to rebase existing worktree branch onto origin/<resolved-default-branch> at <worktree-path>. Resolve the rebase state manually and re-run /my:execute-plan.`
     After a successful rebase, verify with `git status` and `git log origin/<resolved-default-branch>..HEAD`.
     Do NOT force-push automatically as part of this sync step or any later automatic push path.
     **This is ESSENTIAL.** Starting work on a stale branch that is behind the default branch guarantees merge conflicts on the PR. Do NOT skip this step even if the worktree looks up-to-date.
4. **UNLESS --no-worktree**: use the `workflows:worktree-recipe` skill to ensure the expected worktree exists at `.my/worktrees/[repo_name]-[short_title_without_spaces]-[jira_ticket]`.
   - If that worktree already exists, reuse it after the sync/rebase step above.
   - Only create the worktree when it does not already exist.
5. **IF `--no-worktree` IS in effect**: perform a deterministic preflight on the current branch before any execution work starts.
  - Run `git fetch origin` from the repository root / current working directory. If it fails, stop immediately and print: `Failed to fetch origin for --no-worktree execution in <repo-root>. Resolve the fetch failure and re-run /my:execute-plan.`
  - Compare the current branch to `origin/<resolved-default-branch>`.
  - If the current branch is behind `origin/<resolved-default-branch>` or has diverged from it, stop immediately and print: `Current branch is not safe for --no-worktree execution because it is behind or has diverged from origin/<resolved-default-branch>. Sync the branch manually, then re-run /my:execute-plan.`
  - If the current branch is up to date with or strictly ahead of `origin/<resolved-default-branch>`, continue.
  - Do NOT auto-rebase, auto-merge, or otherwise mutate the current branch in this `--no-worktree` preflight.
6. If the codebase is in Python, TypeScript or Rust: note which recipe skills (production-code, test-code, true-myth) sub-agents should load — you will instruct them to do so. For TypeScript, include `typescript-services:true-myth-recipe` only when the plan or the touched codebase already uses true-myth, or when the change intentionally introduces it.
7. **Only after Phase 1 setup succeeds, mark the plan as executing**: update the plan file header by replacing `**Status:** READY` with `**Status:** EXECUTING`.
8. **If execution stops after promoting the plan to `EXECUTING` but before replacing that header with `**Status:** COMPLETED` in Phase 7, return the plan to a recoverable terminal state only for terminal abort/failure stops**: replace `**Status:** EXECUTING` with `**Status:** READY` before stopping, unless Phase 7 already finished successfully. Do NOT do this for normal non-interactive pauses that are waiting for resumed `# output sub-agent <N>:` blocks; those pauses are in-progress waits, so the plan remains `EXECUTING` while `.tmp-execute-plan-state.json` tracks the outstanding batch. This READY recovery path is the only intended terminal state for interrupted runs. A later rerun is deliberate, not unconditional: if the rerun would reuse an existing pushed worktree branch and hits the Phase 1 remote-branch guard, the operator must choose a fresh worktree branch or handle that branch manually before re-running `/my:execute-plan`.

# PHASE 2: TASK DECOMPOSITION

Break the plan into discrete, ordered sub-tasks. For each sub-task, produce a DETAILED sub-task description that includes:

- **Granularity** - A sub-task should not take more than 3-5min to execute.
- **All implementation details for THIS task only** — copy every relevant detail, code snippet, file path, interface definition, type, constant, config value, and acceptance criterion from the plan that pertains to this task. Do NOT summarize or abbreviate — the sub-agent will have NO access to the original plan.
- **Dependency context** — for tasks that depend on earlier tasks, include a summary of what the earlier tasks produced (e.g. "Task 1 created `src/utils/parser.ts` exporting `parseInput(raw: string): ParsedInput`"). Include relevant type signatures, file paths, and export names so the sub-agent can import/use them without guessing.
- **What is OUT OF SCOPE** — explicitly state that the sub-agent must NOT work on any other task and must NOT explore the plan document.
- **Testing expectations** — state whether the sub-agent should write tests for this task. If the task is a small utility or self-contained module, tests should be included. If the task is tightly coupled with other tasks and only testable in integration, mark it as "tests deferred to integration test task".
- **Code standard recipes to load** — list the exact skill names the sub-agent must load before writing code (e.g. `typescript-services:production-code-recipe`, `typescript-services:test-code-recipe`, `typescript-services:true-myth-recipe`).

After producing the sub-task list, organize tasks into **execution waves** based on their dependency graph:

- **Wave 1**: all sub-tasks with NO dependencies - eg. tasks from different tracks (can run in parallel).
- **Wave 2**: sub-tasks that depend ONLY on Wave 1 tasks (can run in parallel once Wave 1 completes).
- **Wave N**: sub-tasks that depend ONLY on tasks from earlier waves.

Within each wave, tasks that touch completely different files and have no shared dependencies can run in parallel (up to 5 concurrent sub-agents). Tasks within the same wave that modify the same files or share dependencies MUST run sequentially within that wave.

Present the numbered sub-task list WITH the wave grouping to the user, then immediately create the **full progress tasklist** in order using TaskCreate — one call per item:
1. One task per wave: `"Implementation: Wave N – <N> task(s)"` (e.g. `"Implementation: Wave 1 – 3 tasks"`, `"Implementation: Wave 2 – 1 task"`) — create all wave tasks first, in wave order
2. `"Integration Testing"`
3. `"Code Review"`
4. `"Plan Validation"`
5. `"Cleanup and PR"`
6. `"Execution Summary"`

Then proceed to Phase 3 without waiting for approval.

## Shared handoff contract for non-interactive mode

`HANDOFF_PROTOCOL.md` in this skill directory is the single source of truth for the non-interactive handoff contract. Do not duplicate or restate the normative protocol in this file beyond phase-specific reminders.

Define the **execution root** once and use it consistently in all later phases:
- If a worktree exists, the execution root is the worktree root.
- If `--no-worktree` is in effect, the execution root is the repository root / current working directory.
- All non-interactive prompt files and `.tmp-execute-plan-state.json` live in the execution root.

Later phases MUST refer back to `HANDOFF_PROTOCOL.md` for batch emission, handoff line format, agent-type semantics, iteration-safe naming, `.tmp-execute-plan-state.json` lifecycle, `claude -p --continue` payload format, failure handling, and executor obligations.

Compact operator example for `--non-interactive`:

```text
Generated prompt files:
- /abs/path/.tmp-subtask-wave-1-batch-1-1.md
- /abs/path/.tmp-subtask-wave-1-batch-1-2.md

call sub-agent 1 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-1.md
call sub-agent 2 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-2.md

# output sub-agent <N>:
[resumed output for the corresponding implementation handoff]

# output sub-agent <N>:
[resumed output for the corresponding implementation handoff]
```

For the exact contract that governs these emitted lines, prompt files, resumed output blocks, and state-file updates, follow `HANDOFF_PROTOCOL.md`.

# PHASE 3: WAVE-BASED EXECUTION

**For each wave: mark its task `in_progress` before launching sub-agents, and `completed` once all sub-agents in that wave finish and the wave review passes.**

Execute sub-tasks wave by wave. Within each wave, run independent sub-tasks in parallel (up to 5 concurrent sub-agents). Wait for the entire wave to complete before starting the next wave. The phase flow stays the same in both modes; only the dispatch mechanism changes.

## Normal interactive mode (`NON_INTERACTIVE=false`)

**For each sub-task in the current wave:**

1. **Create a temporary sub-task file** at `.tmp-subtask-<N>.md` in the execution root containing ONLY the sub-task description from Phase 2 (not the full plan). This file is the sub-agent's sole instruction source.

2. **Launch a sub-agent** using the Agent tool with a general-purpose agent and the following prompt structure:

   ```
   You are a focused development agent. Your ONLY job is to implement the task described in the file at <absolute-path-to-.tmp-subtask-N.md>.

   RULES:
   - Read the sub-task file and implement EXACTLY what it describes. Nothing more, nothing less.
   - Do NOT look for, read, or reference any plan document, roadmap, or other task files.
   - Do NOT implement functionality beyond what your sub-task file specifies.
   - Load the code standard recipes listed in your sub-task file BEFORE writing any code, by using the Skill tool.
   - Write clean, production-quality code following the loaded recipes.
   - If your sub-task file says to write tests, write them. If it says tests are deferred, do NOT write tests.
   - After finishing, report what files you created or modified and any exports/interfaces other tasks might need.
   - Working directory: <absolute-path-to-execution-root>
   ```

   Use a general-purpose agent for each sub-agent. **IMPORTANT: You MUST explicitly set the `model` parameter on every Agent tool call to match YOUR current model (e.g. if you are Opus, pass `model: "opus"`). Do NOT omit it — sub-agents may silently downgrade to a smaller model otherwise.**

   If multiple sub-tasks in the current wave are independent (no shared files, no shared dependencies), launch them in parallel by making multiple Agent tool calls in a single message. Maximum 5 concurrent sub-agents.

**After ALL sub-agents in the current wave complete:**

3. **Review each sub-agent's output:**
   - Verify each sub-agent stayed in scope (did not modify unrelated files).
   - Check for file conflicts — if two parallel sub-agents modified the same file unexpectedly, stop direct orchestration for that conflict. Delegate the resolution through a dedicated follow-up fix handoff/sub-agent or rerun the affected tasks with corrected scope before proceeding.
   - Run any existing linter/type-check/test commands to catch regressions early (`npm run lint`, `npm run build`, `npm test`, or equivalent).
   - Record what was produced (files, exports, interfaces) so you can include this as dependency context for subsequent waves.
   - If a sub-agent failed or produced incorrect output, fix the sub-task description and re-run it before moving to the next wave.

4. **Delete all temporary sub-task files** (`.tmp-subtask-<N>.md`) for the completed wave.

5. **Proceed to the next wave.** Update dependency context for upcoming sub-tasks with the outputs from all completed waves.

## Non-interactive mode (`NON_INTERACTIVE=true`)

In non-interactive mode, mirror the same per-wave execution and review logic, but do NOT launch Task/Agent sub-agents directly. Instead, emit handoff batches that follow `HANDOFF_PROTOCOL.md` exactly.

**For the next batch in the current wave:**

1. **Write iteration-safe prompt files** for the next batch in the execution root using the implementation naming contract from the handoff protocol, for example `.tmp-subtask-wave-<wave>-batch-<batch>-<N>.md`.
   - Each prompt file MUST contain only the sub-task instructions.
   - Prompt files MUST stay free of transport metadata such as `agent-type`.
   - Emit at most 5 prompt files in one batch. Maximum 5 handoffs may be emitted in one batch.
   - If more work remains in the same wave, do NOT emit the next batch yet; produce it only after continuing with the previous batch's outputs.

2. **Print operator handoff instructions** in this exact shape, one line per emitted prompt file:

   ```
   call sub-agent 1 (agent-type: claude): <absolute-path-to-.tmp-subtask-wave-1-batch-1-1.md>
   call sub-agent 2 (agent-type: claude): <absolute-path-to-.tmp-subtask-wave-1-batch-1-2.md>
   ```

3. **Write or update `.tmp-execute-plan-state.json` before stopping** so it records the current execution point and the ordered expected handoffs for the most recently emitted batch. This file is the source of truth for resume processing.

4. **Stop for continuation input.** The resumed main run MUST reread `.tmp-execute-plan-state.json` before parsing any resumed outputs.

**When the main run resumes for the current batch:**

5. **Parse the continued conversation input** by reading each `# output sub-agent N:` block.
   - Map those blocks to the ordered expected handoffs recorded in `.tmp-execute-plan-state.json`.
   - Do NOT infer a different order.
   - Treat missing output blocks as an incomplete batch and stop with a clear retry instruction.

6. **Review each resumed sub-agent output using the same wave review standard as normal mode:**
   - Verify each sub-agent stayed in scope (did not modify unrelated files).
   - Check for file conflicts — if two parallel sub-agents modified the same file unexpectedly, stop direct orchestration for that conflict. Delegate the resolution through a dedicated follow-up fix handoff/sub-agent or rerun the affected tasks with corrected scope before proceeding.
   - Run any existing linter/type-check/test commands to catch regressions early (`npm run lint`, `npm run build`, `npm test`, or equivalent).
   - Record what was produced (files, exports, interfaces) so you can include this as dependency context for subsequent waves.
   - If a sub-agent failed or produced incorrect output, fix the sub-task description and emit a corrected follow-up batch for the same execution point before moving on.

7. **Update `.tmp-execute-plan-state.json` before proceeding** to the next batch in the same wave or the next wave.
   - If more work remains in the same wave, emit the next batch only after this update.
   - Once the current wave is fully reviewed and complete, delete that wave's temporary prompt files.
   - Once the full `/my:execute-plan` run finishes successfully or aborts cleanly, delete `.tmp-execute-plan-state.json`.

8. **Proceed to the next wave using the same completion logic as normal mode.** Update dependency context for upcoming sub-tasks with the outputs from all completed waves.

# PHASE 4: INTEGRATION TESTING (if needed)

**Mark the Phase 4 task `in_progress` if integration tests are needed; otherwise mark it `completed` immediately. Mark it `completed` when done.**

If any sub-tasks had "tests deferred to integration test task", create a dedicated integration test sub-agent.

Mode-specific rule:
- In normal interactive mode (`NON_INTERACTIVE=false`), preserve the current integration test sub-agent launch behavior.
- In non-interactive mode (`NON_INTERACTIVE=true`), write an iteration-safe integration-test prompt file such as `.tmp-subtask-integration-attempt-<attempt>-1.md`, emit a `call sub-agent N (agent-type: claude): <absolute-path>` instruction, and require a resumed `# output sub-agent N:` block before evaluating results.
- If no integration tests are needed, keep the current skip/completed behavior unchanged.
- All non-interactive integration-test handoffs, continuation parsing, and `.tmp-execute-plan-state.json` handling MUST follow `HANDOFF_PROTOCOL.md`.

Track `integration_attempt` starting at 1 for integration-test handoff naming and retries.

1. Write the integration test prompt file describing:
   - Which modules/functions need integration tests.
   - The interfaces and file paths of all relevant modules (gathered from sub-agent outputs).
   - The test scenarios that validate the modules working together.
   - The test-code recipe to load.

2. Dispatch the integration test work using the mode-appropriate path above.

3. After completion, run the full test suite to verify everything passes.
   - If verification passes, continue to step 4.
   - If verification fails, dispatch a dedicated fix sub-agent in interactive mode or emit a dedicated non-interactive handoff for the required fix work; do NOT fix the code yourself. In non-interactive mode, use the integration-fix execution point defined by `HANDOFF_PROTOCOL.md`.
   - After the integration-fix pass completes, increment `integration_attempt`, rerun the integration test step for that next attempt, and rerun the full test-suite verification.
   - Do NOT proceed to Phase 5 until integration verification passes or execution stops for another explicit reason.

4. Delete the temporary file.

# PHASE 5: CODE REVIEW LOOP (MANDATORY — except for Deployment/Infra and Research plans)

**Mark the Code Review task `in_progress` before starting. Mark it `completed` when the loop exits clean.**

**If `SKIP_CODE_REVIEW=true` (plan type is `Deployment / Infra change` or `Research`): mark the task `completed` immediately and skip to Phase 6.**

**Otherwise, THIS PHASE IS MANDATORY. You MUST execute Phase 5 before proceeding to Phase 6 or Phase 7. Skipping this phase is a BLOCKING violation — under NO circumstances may you proceed to Phase 6 or Phase 7 without completing Phase 5 first. There are NO exceptions, regardless of time pressure, context length, or how confident you are in the code quality.**

After all implementation and integration testing is complete, run a code-review loop. The loop repeats until ALL reviewers selected once at Phase 5 start by the interactive reviewer-selection logic return no CRITICAL or IMPORTANT findings in the same iteration. Track `review_attempt` starting at 1. Run a maximum of 15 attempts. If the last attempt has CRITICAL or IMPORTANT findings you HAVE to treat `--merge` or `--merge-admin` as obsolete and you CAN'T merge a PR even if requested. A manuall review IS NEEDED.

**Preparation (once, before the loop):**

1. **Determine the language and freeze the reviewer set for the entire Phase 5 loop** — if TypeScript, the reviewer loads `typescript-services:production-code-recipe`, `typescript-services:test-code-recipe`, and `typescript-services:true-myth-recipe` only when the plan or the touched codebase already uses true-myth, or when the change intentionally introduces it. If Python, the reviewer loads `python-services:production-code-recipe` and `python-services:test-code-recipe`. If Rust, the reviewer loads `rust-services:production-code-recipe` and `rust-services:test-code-recipe`.
   - Reviewer selection MUST be deterministic in both interactive and non-interactive mode:
     - Claude recipe reviewer is ALWAYS required unless `SKIP_CODE_REVIEW=true`.
     - Codex review is included only when `NON_INTERACTIVE=true` or the exact tool `mcp__codex__codex` is available.
     - Gemini review is included only when `NON_INTERACTIVE=true` or the exact tool `mcp__gemini-cli__ask-gemini` is available.
     - Record the chosen reviewer set once before dispatching the first review attempt, then reuse that same reviewer set for every attempt in this Phase 5 review loop. Do not silently add or drop reviewers mid-loop.

**Each loop iteration:**

2. **Create review prompt files for the current attempt.**
   - Always write the Claude recipe reviewer prompt file for the current attempt at `.tmp-subtask-review-attempt-<attempt>-claude.md` containing:
     - A list of ALL files created or modified during Phases 3 and 4 (absolute paths). On re-reviews, also include files touched by fix sub-agents in prior iterations.
     - The exact recipe skill names to load (determined in step 1).
     - The following instructions for the reviewer:

     ```
     You are a code review agent. Your job is to review all listed files for bugs, regressions, code quality, or security issues, and recipe compliance.

     RULES:
     - Load ALL listed code standard recipes using the Skill tool BEFORE reviewing any code.
     - Read every file listed in this review task.
     - Check for: bugs, logic errors, missing error handling, type safety issues, naming convention violations, recipe non-compliance, test quality issues (missing edge cases, weak assertions, improper mocking), and security concerns.
     - Do NOT modify any files. You are a reviewer, not an implementer.
     - Do NOT look for, read, or reference any plan document.
     - Produce a structured review report with:
       - **CRITICAL** — bugs, logic errors, security issues that MUST be fixed.
       - **IMPORTANT** — recipe violations, type safety gaps, missing error handling that SHOULD be fixed.
       - **MINOR** — style nits, naming suggestions that are NICE TO HAVE.
     - For each finding, include: the file path, line number or code snippet, what the issue is, and a concrete suggestion for how to fix it.
     - If there are no issues, explicitly state "No issues found."
     ```
   - If the frozen reviewer set for this Phase 5 loop includes Codex or Gemeini review, also write `.tmp-subtask-review-attempt-<attempt>-other.md` containing:
     - The absolute execution root path.
     - The absolute paths of every file created or modified during Phases 3 and 4 for this attempt's review scope.
     - The following review prompt:

     ```
     You are a code review agent. Your job is to review all listed files for bugs, regressions, code quality, or security issues.

     RULES:
     - Read every file listed in this review task.
     - Check for: bugs, logic errors, missing error handling, type safety issues, naming convention violations, test quality issues (missing edge cases, weak assertions, improper mocking), and security concerns.
     - Do NOT modify any files.
     - Do NOT read or rely on any plan document.
     - Produce exactly these sections:
       - **CRITICAL** — must-fix issues that would break behavior, introduce regressions, or create serious operational/security risk.
       - **IMPORTANT** — should-fix issues that materially weaken correctness, robustness, or maintainability.
       - **MINOR** — optional quality suggestions.
     - For every finding, include the file path, line number or snippet, why it matters, and a concrete fix suggestion.
     - If there are no issues, explicitly state "No issues found."
     ```
   - In non-interactive mode, you MUST mirror the same reviewer-selection logic used by interactive mode to determine the frozen reviewer set at Phase 5 start. Do NOT invent a different inclusion rule for which reviewers run, and do NOT recompute the reviewer set on later attempts in the same review loop.

3. **Dispatch the selected reviewers using the mode-appropriate path:**

  - **Normal interactive mode (`NON_INTERACTIVE=false`):** preserve the current behavior.
    - Launch the review sub-agent using the Agent tool with a general-purpose agent. Pass `<absolute-path-to-.tmp-subtask-review-attempt-<attempt>-claude.md>` to the sub-agent.
    - Launch a Codex review agent only when the exact tool `mcp__codex__codex` is available. Run it in parallel with the recipe reviewer and ask it to review the deterministic review artifact for bugs, logic errors, and code quality issues. The two reviewers serve different purposes (recipe compliance vs general code quality). Pass `<absolute-path-to-.tmp-subtask-review-attempt-<attempt>-other.md>` to the sub-agent. **IMPORTANT: Do NOT pass a `model` parameter to the Codex tool — always use the default model. Passing unsupported model names will cause the call to fail.**
    - Launch a Gemini review agent only when the exact tool `mcp__gemini-cli__ask-gemini` is available. Run it in parallel with the other reviewers, pass the deterministic review artifact, and ask it to review for bugs, logic errors, and code quality issues. Pass `<absolute-path-to-.tmp-subtask-review-attempt-<attempt>-other.md>` to the sub-agent.

  - **Non-interactive mode (`NON_INTERACTIVE=true`):** do NOT launch review agents directly.
    - Emit handoff lines only for the reviewers in the frozen reviewer set chosen at Phase 5 start.
    - Use the review-attempt state recorded per `HANDOFF_PROTOCOL.md` so resumed runs know the full reviewer set frozen for the review loop and which batches for that attempt have already been completed.
    - Use these exact filename patterns and agent types:
      - `call sub-agent N (agent-type: claude): <absolute-path-to-.tmp-subtask-review-attempt-<attempt>-claude.md>`
      - `call sub-agent N (agent-type: codex): <absolute-path-to-.tmp-subtask-review-attempt-<attempt>-other.md>`
      - `call sub-agent N (agent-type: gemini): <absolute-path-to-.tmp-subtask-review-attempt-<attempt>-other.md>`
    - When Claude, Codex, and Gemini reviewers are all required, prioritize them into the earliest batch for that review attempt.
    - Require resumed `# output sub-agent N:` blocks for every reviewer emitted of that review attempt.
    - Do NOT evaluate the review round until outputs from ALL required reviewers for that attempt have been collected.

4. **Delete the temporary review prompt files for that attempt after their outputs have been collected and processed.**

5. **Evaluate the combined review results** from ALL reviewers selected for that attempt:
   - If **no CRITICAL or IMPORTANT findings** remain across all required reviewers: exit the loop and proceed to Phase 6.
   - If **CRITICAL or IMPORTANT issues were found**:
     a. Create a fix prompt file for this attempt containing:
        - Every CRITICAL and IMPORTANT finding from ALL reviewers in this iteration (copy them verbatim — file paths, code snippets, issue descriptions, and fix suggestions).
        - The recipe skill names to load.
        - Clear instruction: "Fix each issue listed below. Do NOT change anything else. Do NOT look for or read any plan document."
     b. In normal interactive mode (`NON_INTERACTIVE=false`), preserve the current behavior by launching a fix sub-agent using the Agent tool with a general-purpose agent.
     c. In non-interactive mode (`NON_INTERACTIVE=true`), write an iteration-safe fix prompt file such as `.tmp-subtask-review-fix-attempt-<attempt>-1.md` and emit the handoff required by `HANDOFF_PROTOCOL.md` for the review-fix execution point.
         Then require a resumed `# output sub-agent 1:` block before proceeding.
     d. After the fix work completes, run the full build, lint, and test pipeline to verify no regressions. If that verification finds new issues, dispatch a dedicated follow-up fix sub-agent in interactive mode or emit a dedicated non-interactive handoff that uses the review-fix execution point from `HANDOFF_PROTOCOL.md`; do NOT edit code directly.
     e. Delete the temporary review-fix prompt file(s).
     f. Increment `review_attempt` and **return to step 2** (start a new review iteration on the updated code).

# PHASE 6: PLAN VALIDATION LOOP (MANDATORY — DO NOT SKIP)

**Mark the Phase 6 task `in_progress` before starting. Mark it `completed` only when validation passes, or when an interactive operator explicitly decides to proceed after 5 failed attempts. If the interactive operator aborts after validation failures, stop after printing the required Phase 6 failure summary and leave this task incomplete. In non-interactive mode, a fifth failed attempt is a deterministic stop and this task remains incomplete.**

**THIS PHASE IS MANDATORY. You MUST execute Phase 6 before proceeding to Phase 7. Skipping this phase is a BLOCKING violation — under NO circumstances may you proceed to Phase 7 without completing Phase 6 first. There are NO exceptions, regardless of time pressure, context length, or how confident you are in the implementation correctness.**

**ANTI-SHORTCUT RULE: You MUST use a dedicated validation agent. You are FORBIDDEN from performing the validation yourself — no "quick targeted validation", no "checking acceptance criteria myself", no "the changes are straightforward so I'll verify directly". The orchestrator NEVER validates. A dedicated sub-agent validates. This is non-negotiable. If you catch yourself writing validation checks instead of dispatching the validator, STOP and dispatch it.**

Validate that the ENTIRE plan has been implemented correctly according to its specification. This phase can repeat up to **5 times**. Track the current iteration as `validation_attempt` (starting at 1).

1. **Create a temporary validation file** for the current attempt in the execution root, using an iteration-safe name such as `.tmp-subtask-plan-validation-attempt-<attempt>.md`, containing:
   - The FULL original plan content (copy it verbatim — this is the ONE exception where the plan is shared, because the validator needs to compare the spec against the implementation).
   - A list of ALL files created or modified during the entire execution (absolute paths).
   - The following instructions for the validator:

   ```
   You are a plan validation agent. Your job is to verify that a development plan has been FULLY and CORRECTLY implemented.

   RULES:
   - Read the plan specification provided in this file top to bottom.
   - Read every implementation file listed in this file.
   - For EACH requirement, feature, behavior, interface, type, config value, and acceptance criterion in the plan, verify it exists in the codebase and works as specified.
   - Do NOT modify any files. You are a validator, not an implementer.
   - Produce a structured validation report:
     - **STATUS**: either "PASS" (everything implemented correctly) or "FAIL" (gaps or issues found).
     - **IMPLEMENTED** — list each plan requirement and confirm it is correctly implemented, with the file path where it lives.
     - **GAPS** — for each missing or incorrectly implemented requirement:
       - What the plan specifies (quote the relevant section).
       - What is actually implemented (or "missing entirely").
       - The file path(s) involved.
       - A concrete description of what needs to be done to fix it.
     - **DEVIATIONS** — any implementation that diverges from the plan (different naming, different approach, extra functionality not in the plan). Note whether the deviation is acceptable or problematic.
   - Be thorough. Check types, function signatures, exports, error handling, edge cases, config values — everything the plan specifies.
   - If everything is correctly implemented, set STATUS to "PASS" and leave GAPS empty.
   - In non-interactive mode, validation-fix work MUST mirror the same batching discipline used elsewhere: create one dedicated fix handoff per GAP, emit at most 5 validation-fix handoffs in a batch, process batches sequentially for the same `validation_attempt`, and resume according to `HANDOFF_PROTOCOL.md` before emitting the next batch or re-running validation.
   ```

2. **Dispatch the validation work using the mode-appropriate path.**
   - In normal interactive mode (`NON_INTERACTIVE=false`), preserve the current rule that you launch an actual validation sub-agent using the Agent tool with a general-purpose agent.
   - In non-interactive mode (`NON_INTERACTIVE=true`), replace the launch with:
     - write the iteration-safe validation prompt file from step 1
     - emit a single `call sub-agent 1 (agent-type: claude): <absolute-path-to-.tmp-subtask-plan-validation-attempt-<attempt>.md>` handoff instruction
     - require the resumed conversation to include `# output sub-agent 1:` before evaluating PASS/FAIL
     - follow `HANDOFF_PROTOCOL.md` for continuation parsing and `.tmp-execute-plan-state.json` updates

3. **Delete the temporary validation prompt file after its output has been collected and processed.**

4. **Evaluate the validation results:**

   - If STATUS is **"PASS"**: proceed to Phase 7.

   - If STATUS is **"FAIL"** and `validation_attempt < 5`:
     a. For each GAP in the report, create a sub-task description (same format as Phase 2) containing:
        - The exact requirement from the plan that is missing or incorrect (quoted from the validation report).
        - What currently exists (from the validation report).
        - What needs to be done to fix it (from the validation report).
        - The relevant file paths.
        - The code standard recipes to load.
        - Explicit scope: "Fix ONLY the issue described here. Do NOT change anything else."
     b. Execute these fix sub-tasks using the same process as Phase 3 in normal interactive mode.
     c. In non-interactive mode (`NON_INTERACTIVE=true`), use the validation-fix execution point defined by `HANDOFF_PROTOCOL.md`: write one iteration-safe dedicated fix prompt file per GAP using names such as `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md`, emit at most 5 validation-fix handoffs in the current batch, require the resumed `# output sub-agent <N>:` blocks for that batch before continuing, then emit the next validation-fix batch only if more GAPs remain for the same attempt.
     d. After all fix sub-agents or validation-fix batches for that attempt complete, run the full build, lint, and test pipeline. If verification still fails, dispatch another dedicated fix pass for the failing issues rather than editing code directly.
     e. If `SKIP_CODE_REVIEW=false`, re-run the full Phase 5 (Code Review) loop using the same frozen reviewer set from that Phase 5 loop; do NOT scope the re-review to only the newly modified files.
     f. If `SKIP_CODE_REVIEW=true`, skip re-running Phase 5 and continue the validation flow directly.
     g. Increment `validation_attempt` and re-run Phase 6 from step 1.

   - If STATUS is **"FAIL"** and `validation_attempt >= 5`:
     a. Do NOT attempt further fixes.
     b. Present the remaining GAPS with a clear summary:
        - How many validation attempts were made (5).
        - Which requirements are still not met.
        - The validator's description of each remaining gap.
     c. In normal interactive mode (`NON_INTERACTIVE=false`), ask the user whether to proceed with the PR as-is or abort.
     d. In non-interactive mode (`NON_INTERACTIVE=true`), stop deterministically, print the required Phase 6 failure summary showing that validation failed after 5 attempts and what gaps remain, instruct the operator to review the reported gaps and re-run intentionally with a new explicit decision, and do NOT enter Phase 7 or Phase 8.
     e. If interactive mode proceeds, continue to Phase 7.
     f. If interactive mode aborts, print the required Phase 6 failure summary, stop execution immediately after that summary, clean up all temporary files, and do NOT enter Phase 7 or Phase 8.

# PHASE 7: CLEANUP AND PR

**Mark the Phase 7 task `in_progress` before starting. Mark it `completed` only after the final build/lint/test verification passes, PR work is finished, and any required `my:pr-finalize` run is fully complete.**

1. Clean up temporary prompt/state files coherently for the active mode before final PR work.
   - In normal interactive mode (`NON_INTERACTIVE=false`), preserve the current Task-based launch behavior. Delete any completed wave/integration/review/validation temporary prompt files that this run created once they are no longer needed.
   - In non-interactive mode (`NON_INTERACTIVE=true`), prompt files continue to be created for emitted handoffs. Delete a prompt file only after its corresponding output has been collected and processed.
   - Preserve findings, review, validation, and state temp files long enough to support any follow-up fix loop that still depends on them.
   - Once an attempt has been resolved and its follow-up loop is complete, delete all superseded attempt-specific integration, integration-fix, review, review-fix, validation, validation-fix, and cleanup-fix prompt files from earlier iterations, not just a single temporary file.
   - Once the full run finishes successfully or aborts cleanly, verify no obsolete `.tmp-subtask-*.md` prompt files remain and delete `.tmp-execute-plan-state.json` if it still exists.
2. Run the full build, lint, and test pipeline one final time. If any issues are found, dispatch a dedicated fix sub-agent in interactive mode or emit a dedicated non-interactive handoff that uses the cleanup-fix execution point from `HANDOFF_PROTOCOL.md`, then re-run the pipeline.
   - Track `cleanup_attempt` starting at 1 for this Phase 7 loop.
   - Use iteration-safe names such as `.tmp-subtask-cleanup-fix-attempt-<attempt>-1.md`.
   - After each cleanup-fix pass, increment `cleanup_attempt`, re-run the full pipeline, and continue until the pipeline passes or execution stops for another explicit reason.
   - The orchestrator must not edit code directly.
3. **UNLESS `SKIP_PR=true`**: Commit all changes with a meaningful commit message referencing the resolved JIRA ticket from Phase 1 (`JIRA_TICKET`).
   - `--no-pr` skips remote PR work only; it does NOT skip the local commit.
4. **UNLESS --no-pr or `SKIP_PR=true`**: Push the branch and create a DRAFT PR using `gh pr create --draft`.
   - This normal push path is allowed only when Phase 1 did not stop on the reused-worktree rewritten-history guard.
   - If a reused worktree would require a rewritten-history push, execution must already have stopped in Phase 1; do NOT continue to Phase 7 expecting a normal push to succeed.
   - The PR title must include the jira ticket. The PR body should summarize what was implemented, organized by sub-task. If there were unresolved gaps from Phase 6, include them in a "Known Gaps" section of the PR body.
   - **If `SKIP_PR=true`** (Deployment/Infra or Research plan): skip PR creation. Instead, summarize the completed work to the user directly and note any output artifacts (docs, config files, analysis).
5. **UNLESS --no-pr, --draft-pr, or `SKIP_PR=true`**: Mark the PR ready and run the `my:pr-finalize` skill to finalize it. **This step is NOT optional whenever the normal PR path is still enabled. If neither `--no-pr` nor `--draft-pr` was passed and `SKIP_PR!=true`, you MUST invoke `my:pr-finalize`. Do NOT skip it, forget it, or rationalize that the PR is "already done". The PR is NOT done until `my:pr-finalize` has run.**

   **PR FINALIZATION COMPLETION RULE (NON-NEGOTIABLE):**
   The `my:pr-finalize` skill runs a check-and-fix loop that polls for Bugbot completion and review comments. You MUST NOT consider the PR finalized, mark Phase 7 complete, mark the plan `COMPLETED`, print the execution summary, or end your work until ALL of the following are confirmed true:
   1. The Cursor Bugbot check run `status` is `"completed"` (not `"in_progress"`, not `"queued"`).
   2. There are ZERO unresolved review threads from the `cursor` bot posted after the most recent push.
   3. There are ZERO failing compliance checks posted after the most recent push.
6. **Only after all required Phase 7 work is truly finished** — final build/lint/test verification is green, PR work is done or intentionally skipped, and any required `my:pr-finalize` loop has fully completed — if the plan file is in `.my/plans/`, update its header by replacing `**Status:** EXECUTING` with `**Status:** COMPLETED`.
7. **Only after step 6 succeeds**, mark the Phase 7 task `completed`.

   **If Bugbot is slow:** Keep polling. Use `/loop 2m` or manual checks every 2-3 minutes. Do NOT cancel the loop, do NOT rationalize stopping early, do NOT cite "polling limits" as a reason to abandon. The CLAUDE.md guidance about spacing out polls means "wait between checks" — it does NOT mean "give up after N checks." You wait as long as it takes.

   **If Bugbot posts bug comments:** Fix them in the execution root, push, reply to the threads, resolve them, then re-enter the polling loop for the NEW commit's Bugbot run. Repeat until a clean Bugbot run completes with no new comments.

   **The execution summary (Phase 8) MUST NOT be printed until this rule is fully satisfied.** Printing the summary is your signal that ALL work is done. If you print it while Bugbot is still running, you have violated this rule.

# CRITICAL RULES

- **MODEL INHERITANCE FOR NORMAL MODE:** Every Agent-launched sub-agent in normal interactive mode (`NON_INTERACTIVE=false`) MUST run on the same model as the orchestrator. On every Agent tool call, you MUST explicitly set the `model` parameter to match YOUR current model (e.g. if you are Opus, set `model: "opus"`; if Sonnet, set `model: "sonnet"`). Do NOT omit the model parameter — without it, sub-agents may silently downgrade to a smaller model. This applies to EVERY Agent tool invocation throughout ALL phases that use the Agent tool in normal mode (implementation, review, validation, and fix sub-agents). This rule does NOT apply to Codex MCP review calls in Phase 5, where you must omit the `model` parameter exactly as instructed there.
- **AGENT-TYPE HANDOFF METADATA:** In non-interactive mode, implementation, integration-test, fix, cleanup, and validation handoff entries default to `agent-type: claude`. `agent-type: codex` and `agent-type: gemini` are used only where this skill already uses those reviewer channels in Phase 5. This metadata is transport metadata for the executor and MUST be emitted outside the prompt file body.
- You are the ORCHESTRATOR. You do NOT write production code or test code. In normal interactive mode, you delegate coding/review/validation work via Agent-launched sub-agents. In non-interactive mode, you delegate the same work by emitting the required handoffs and resuming only after the corresponding outputs are provided.
- Sub-agents must NEVER see the full plan document. They only see their individual sub-task file. The ONLY exception is the plan validation agent in Phase 6, which needs the full plan to compare against the implementation.
- Sub-task files must be COMPREHENSIVE — include every detail the sub-agent needs. The sub-agent cannot ask follow-up questions about the plan.
- Execute sub-tasks SEQUENTIALLY in dependency order. Do not parallelize unless tasks are explicitly independent.
- Always clean up temporary files before creating the PR.
- If a sub-agent's work is unsatisfactory, you may re-run it with a corrected sub-task description, but do NOT take over and write the code yourself.
- The validation loop (Phase 6) runs a MAXIMUM of 5 times. After 5 failed attempts, interactive mode may ask the user whether to proceed or abort. If the user aborts, print the required Phase 6 failure summary, leave the Phase 6 task incomplete, and skip Phases 7-8. In non-interactive mode, stop deterministically, leave the Phase 6 task incomplete, skip Phases 7-8, and print the Phase 6 failure summary instead of an execution summary.
- **MANDATORY PHASE ORDERING: Phases MUST execute in strict order: 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8. Phase 5 (Code Review Loop) MAY be skipped only when `SKIP_CODE_REVIEW=true` (plan type is Deployment/Infra or Research). PR creation in Phase 7 MAY be skipped when `SKIP_PR=true`. In all other cases these are NON-NEGOTIABLE prerequisites. No other exceptions — not for time, context length, or confidence.**
- **NO SELF-VALIDATION: The orchestrator MUST NOT perform validation checks itself. Phase 5 (Code Review Loop) and Phase 6 (Plan Validation) MUST each use dedicated review/validation sub-agents rather than self-review. In normal interactive mode, satisfy this by launching the required reviewers/validators directly. In non-interactive mode, satisfy the same independence requirement by emitting the required external handoffs and waiting for resumed output blocks before evaluating results. "Doing it myself because it's faster/simpler/straightforward" is explicitly forbidden. The whole point of these phases is independent verification by separate agents with fresh context.**
- **CODE REVIEW LOOP MUST CONVERGE: Phase 5 loops until every reviewer in the set frozen at Phase 5 start (Claude recipe reviewer plus Codex and/or Gemini when available at that point) returns zero CRITICAL or IMPORTANT findings in the same iteration. Finding fixes in iteration N do NOT count as a clean pass — a new review iteration must confirm they are resolved. Do NOT exit the loop after fixing — always re-review.**
- **NO SKIPPING PR FINALIZATION: Unless `--no-pr` or `--draft-pr` was explicitly passed, and unless `SKIP_PR=true`, Phase 7 step 5 MUST invoke `my:pr-finalize` via the Skill tool after marking the PR ready. The PR is incomplete without finalization whenever the normal PR path is still enabled. Do NOT stop at "PR created" or "PR marked ready" — you MUST run the finalize skill. Forgetting this step means the PR is unfinished.**
- **NO EARLY TERMINATION OF PR FINALIZATION: Once `my:pr-finalize` is invoked, you MUST wait for the Bugbot check to reach `status: "completed"` and all bug comments to be fixed and resolved BEFORE moving to Phase 8. You are FORBIDDEN from canceling the polling loop, marking Phase 7 complete, marking the plan `COMPLETED`, printing the execution summary, or declaring the work done while Bugbot is still `in_progress`. "It's taking too long" is NOT a valid reason to stop. "Polling limits" means space out your checks — it does NOT mean give up. You poll until Bugbot completes, fix any bugs it finds, and only THEN proceed to Phase 8.**

# PHASE 8: EXECUTION SUMMARY (MANDATORY — DO NOT SKIP)

**Mark the "Execution Summary" task `in_progress` before preparing the summary. Mark it `completed` after the summary is printed.**

**After Phase 7 completes, you MUST print a structured execution summary.** This gives the user a clear overview of what happened across all phases. If Phase 6 ends in an interactive abort after validation failures, stop after printing the required Phase 6 failure summary and do NOT enter Phase 8. In non-interactive mode, a deterministic stop after 5 failed validation attempts does not enter Phase 8; print the Phase 6 failure summary and stop instead.

Print the following summary in markdown format:

```
## Execution Summary

### Phase 1: Setup
- Branch name and execution root / worktree location
- Any setup issues encountered

### Phase 2: Task Decomposition
- Total number of sub-tasks
- Number of execution waves
- Brief list of sub-tasks with their wave assignments

### Phase 3: Wave-Based Execution
For each wave:
- Which sub-tasks ran (number, name, parallel vs sequential)
- Sub-agent outcomes (success/failure/retry)
- Files created or modified
- Build/test results after the wave

### Phase 4: Integration Testing
- Whether integration tests were needed
- If run: test results and any issues found

### Phase 5: Code Review
- Number of CRITICAL / IMPORTANT / MINOR findings
- Which findings required fixes vs were accepted
- Fix sub-agent outcomes (if any)

### Phase 6: Plan Validation
- Number of validation attempts
- Final STATUS (PASS/FAIL)
- Any gaps or deviations noted

### Phase 7: Cleanup and PR
- Final build/test status (tests passed / total)
- Commit hash
- PR URL
- Any known gaps included in PR

### Totals
- Files changed: N (list them)
- Lines added/removed: +X / -Y
- Execution dispatches: N interactive sub-agents spawned and/or N non-interactive handoff batches generated/resumed
- Validation attempts: N
- Final result: PASS / FAIL (with gap count if applicable)
```

Adapt the summary to what actually happened using one consistent rule: if a phase in the Phase 8 template was skipped, keep that phase heading and include a single explicit line saying it was skipped and why. Always include Phases 1-3, 6, and 7 when Phase 8 is reached. Include Phase 5 even when code review did not run; if `SKIP_CODE_REVIEW=true`, report under Phase 5 that code review was skipped for the plan type instead of describing review findings. In non-interactive mode, describe generated/resumed handoff batches instead of claiming sub-agents were launched directly.
