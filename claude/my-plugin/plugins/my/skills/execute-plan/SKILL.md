---
description: Execute a development plan by orchestrating focused sub-agents for each task
argument-hint: [plan-document] [jira-ticket] [--no-worktree] [--no-pr] [--draft-pr]
allowed-tools: AskUserQuestion, Read, Write, Edit, Glob, Grep, Bash(code:*), Bash(cursor:*), Bash(git *), Bash(gh *), Bash(npm *), Bash(npx *), Bash(pnpm *), Bash(yarn *), Bash(bun *), Bash(pip *), Bash(poetry *), Bash(python *), Bash(node *), Bash(rm *), Bash(mkdir *), Bash(ls *), Bash(cat *), Bash(open:*), Task, Skill, mcp__atlassian__*
---

**PLANNING MODE CHECK (execute FIRST, before anything else):**
If you are currently in planning mode (i.e., you have access to `ExitPlanMode`), STOP IMMEDIATELY. Do NOT proceed with any phase of this command. Instead, write the plan into `.claude/plans/plan-[title].md` (create the directory if needed) and inform the user:
"This command cannot run in planning mode. Please exit planning mode first, then re-run this command with [name of the new plan doc]."
Do NOT attempt to exit planning mode yourself — let the user handle it.
**CRITICAL: Do NOT call `ExitPlanMode`. Do NOT call `AskUserQuestion` about exiting plan mode. Simply print the message above and STOP. Take NO further action.**

---

Execute plan $1 or the current plan you created in plan-mode.

You are the ORCHESTRATOR. You coordinate the execution of a development plan by delegating individual tasks to focused sub-agents. You NEVER write production code or test code yourself — you only manage the workflow.

# PHASE 1: SETUP

1. **Resolve the plan document:**
   - If $1 was provided, use that path directly.
   - If $1 was NOT provided, search for an unexecuted plan:
     a. Check `.claude/plans/` for any `plan-*.md` files where the header contains `**Executed:** [ ]` (unchecked). List them all.
     b. Also check `docs/superpowers/specs/` for any `*-design.md` spec files (these are brainstorming outputs ready for implementation).
     c. If multiple candidates are found, call AskUserQuestion with:
        - question: "Which plan would you like to execute?"
        - one option per candidate file (label = filename, description = first line of the file or the Goal field if present)
        - an "Other / enter path manually" option as the last choice
     d. If exactly one unexecuted plan is found, proceed with it automatically (announce which file you chose).
     e. If none are found, ask the user to provide a plan path.
   - Read the chosen plan document fully and understand all tasks, their dependencies, and their order.
2. Update the repo (`git pull`) before starting.
3. **UNLESS --no-worktree**: Create a git worktree for this work using the `workflows:worktree-recipe` skill. Use jira ticket $2 for the branch name. If no jira ticket was provided, use the AskUserQuestion tool to ask the user for one. Create the worktree in the repo directory under `.claude/worktrees/[repo_name]-[short_title_without_spaces]-[jira_ticket_if_available]`.
4. If the codebase is in Python, TypeScript or Rust: note which recipe skills (production-code, test-code, true-myth) sub-agents should load — you will instruct them to do so.
5. **Create a progress tasklist** using the TaskCreate tool — one task per remaining phase so the user can track execution at a glance:
   - "Phase 2: Task Decomposition"
   - "Phase 3: Wave-Based Execution"
   - "Phase 4: Integration Testing"
   - "Phase 5: Code Review"
   - "Phase 6: Plan Validation"
   - "Phase 7: Cleanup and PR"

# PHASE 2: TASK DECOMPOSITION

**Mark the Phase 2 task `in_progress` before starting. Mark it `completed` when done.**

Break the plan into discrete, ordered sub-tasks. For each sub-task, produce a DETAILED sub-task description that includes:

- **Granularity** - A sub-task should not take more than 5min to execute.
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

Present the numbered sub-task list WITH the wave grouping to the user, then immediately proceed to Phase 3 without waiting for approval.

# PHASE 3: WAVE-BASED EXECUTION

**Mark the Phase 3 task `in_progress` before starting. Mark it `completed` when the final wave finishes.**

Execute sub-tasks wave by wave. Within each wave, run independent sub-tasks in parallel (up to 5 concurrent sub-agents). Wait for the entire wave to complete before starting the next wave.

**For each sub-task in the current wave:**

1. **Create a temporary sub-task file** at `.tmp-subtask-<N>.md` in the worktree root containing ONLY the sub-task description from Phase 2 (not the full plan). This file is the sub-agent's sole instruction source.

2. **Launch a sub-agent** using the Task tool with the following prompt structure:

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
   - Working directory: <absolute-path-to-worktree>
   ```

   Use `subagent_type: "general-purpose"` for each sub-agent. **IMPORTANT: You MUST explicitly set the `model` parameter on every Task tool call to match YOUR current model (e.g. if you are Opus, pass `model: "opus"`). Do NOT omit it — sub-agents may silently downgrade to a smaller model otherwise.**

   If multiple sub-tasks in the current wave are independent (no shared files, no shared dependencies), launch them in parallel by making multiple Task tool calls in a single message. Maximum 5 concurrent sub-agents.

**After ALL sub-agents in the current wave complete:**

3. **Review each sub-agent's output:**
   - Verify each sub-agent stayed in scope (did not modify unrelated files).
   - Check for file conflicts — if two parallel sub-agents modified the same file unexpectedly, resolve the conflict before proceeding.
   - Run any existing linter/type-check/test commands to catch regressions early (`npm run lint`, `npm run build`, `npm test`, or equivalent).
   - Record what was produced (files, exports, interfaces) so you can include this as dependency context for subsequent waves.
   - If a sub-agent failed or produced incorrect output, fix the sub-task description and re-run it before moving to the next wave.

4. **Delete all temporary sub-task files** (`.tmp-subtask-<N>.md`) for the completed wave.

5. **Proceed to the next wave.** Update dependency context for upcoming sub-tasks with the outputs from all completed waves.

# PHASE 4: INTEGRATION TESTING (if needed)

**Mark the Phase 4 task `in_progress` if integration tests are needed; otherwise mark it `completed` immediately. Mark it `completed` when done.**

If any sub-tasks had "tests deferred to integration test task", create a dedicated test sub-agent:

1. Write a `.tmp-subtask-integration-tests.md` file describing:
   - Which modules/functions need integration tests.
   - The interfaces and file paths of all relevant modules (gathered from sub-agent outputs).
   - The test scenarios that validate the modules working together.
   - The test-code recipe to load.

2. Launch the test sub-agent the same way as development sub-agents.

3. After completion, run the full test suite to verify everything passes.

4. Delete the temporary file.

# PHASE 5: CODE REVIEW (MANDATORY — DO NOT SKIP)

**Mark the Phase 5 task `in_progress` before starting. Mark it `completed` when done.**

**THIS PHASE IS MANDATORY. You MUST execute Phase 5 before proceeding to Phase 6 or Phase 7. Skipping this phase is a BLOCKING violation — under NO circumstances may you proceed to Phase 6 or Phase 7 without completing Phase 5 first. There are NO exceptions, regardless of time pressure, context length, or how confident you are in the code quality.**

After all implementation and integration testing is complete, run a code-review sub-agent to catch issues before the PR.

1. **Determine the language** — if TypeScript, the reviewer loads `typescript-services:production-code-recipe`, `typescript-services:test-code-recipe`, and `typescript-services:true-myth-recipe`. If Python, the reviewer loads `python-services:production-code-recipe` and `python-services:test-code-recipe`. If Rust, the reviewer loads `rust-services:production-code-recipe` and `rust-services:test-code-recipe`.

2. **Create a temporary review file** at `.tmp-subtask-code-review.md` in the worktree root containing:
   - A list of ALL files created or modified during Phases 3 and 4 (absolute paths).
   - The exact recipe skill names to load (determined in step 1).
   - The following instructions for the reviewer:

   ```
   You are a code review agent. Your job is to review all listed files for bugs, code quality issues, and recipe compliance.

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

3. **Launch the review sub-agent** using the Task tool with `subagent_type: "general-purpose"`.

4. **Launch a Codex review agent (if available):** Check your available tools for `mcp__codex__codex`. If it exists, you MUST launch a parallel Codex review by calling `mcp__codex__codex` with a prompt asking it to review the diff on the current branch for bugs, logic errors, and code quality issues. Run this IN PARALLEL with the sub-agent from step 3 — do NOT skip it just because you already have a recipe-based reviewer. The two reviewers serve different purposes (recipe compliance vs general code quality). If `mcp__codex__codex` is not in your tool list, skip this step. **IMPORTANT: Do NOT pass a `model` parameter to `mcp__codex__codex` — always use the default model. Passing unsupported model names (e.g. `o4-mini`) will cause the call to fail.**

5. **Evaluate the review results** (from BOTH the recipe reviewer AND Codex, if launched):
   - If the reviewer found **CRITICAL** or **IMPORTANT** issues:
     a. Create a `.tmp-subtask-review-fixes.md` file containing:
        - Every CRITICAL and IMPORTANT finding from the review (copy them verbatim — file paths, code snippets, issue descriptions, and fix suggestions).
        - The recipe skill names to load.
        - Clear instruction: "Fix each issue listed below. Do NOT change anything else. Do NOT look for or read any plan document."
     b. Launch a fix sub-agent using the Task tool with `subagent_type: "general-purpose"`.
     c. After the fix sub-agent completes, run the full build, lint, and test pipeline to verify no regressions.
     d. Delete `.tmp-subtask-review-fixes.md`.
   - If the reviewer found only **MINOR** issues or no issues, proceed without fixes.

6. **Delete `.tmp-subtask-code-review.md`.**

# PHASE 6: PLAN VALIDATION LOOP (MANDATORY — DO NOT SKIP)

**Mark the Phase 6 task `in_progress` before starting. Mark it `completed` when validation passes (or when the user decides to proceed after 5 failed attempts).**

**THIS PHASE IS MANDATORY. You MUST execute Phase 6 before proceeding to Phase 7. Skipping this phase is a BLOCKING violation — under NO circumstances may you proceed to Phase 7 without completing Phase 6 first. There are NO exceptions, regardless of time pressure, context length, or how confident you are in the implementation correctness.**

**ANTI-SHORTCUT RULE: You MUST launch an actual validation sub-agent using the Agent tool. You are FORBIDDEN from performing the validation yourself — no "quick targeted validation", no "checking acceptance criteria myself", no "the changes are straightforward so I'll verify directly". The orchestrator NEVER validates. A dedicated sub-agent validates. This is non-negotiable. If you catch yourself writing validation checks instead of launching a sub-agent, STOP and launch the sub-agent.**

Validate that the ENTIRE plan has been implemented correctly according to its specification. This phase can repeat up to **5 times**. Track the current iteration as `validation_attempt` (starting at 1).

1. **Create a temporary validation file** at `.tmp-subtask-plan-validation.md` in the worktree root containing:
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
   ```

2. **Launch the validation sub-agent** using the Task tool with `subagent_type: "general-purpose"`.

3. **Delete `.tmp-subtask-plan-validation.md`.**

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
     b. Execute these fix sub-tasks using the same process as Phase 3 (create temp file, launch sub-agent, verify, delete temp file).
     c. After all fix sub-agents complete, run the full build, lint, and test pipeline.
     d. Re-run Phase 5 (Code Review) on the newly modified files only.
     e. Increment `validation_attempt` and re-run Phase 6 from step 1.

   - If STATUS is **"FAIL"** and `validation_attempt >= 5`:
     a. Do NOT attempt further fixes.
     b. Present the remaining GAPS to the user with a clear summary:
        - How many validation attempts were made (5).
        - Which requirements are still not met.
        - The validator's description of each remaining gap.
     c. Ask the user whether to proceed with the PR as-is or abort.
     d. If the user chooses to proceed, continue to Phase 7. If the user chooses to abort, stop execution and clean up all temporary files.

# PHASE 7: CLEANUP AND PR

**Mark the Phase 7 task `in_progress` before starting. Mark it `completed` after PR finalization is done.**

1. Verify ALL temporary `.tmp-subtask-*.md` files are deleted. If any remain, delete them now.
1a. **Mark the plan as executed**: if the plan file is in `.claude/plans/`, update its header by replacing `**Executed:** [ ]` with `**Executed:** [x]`.
2. Run the full build, lint, and test pipeline one final time. Fix any issues.
3. **UNLESS --no-pr**: Commit all changes with a meaningful commit message referencing jira ticket $2.
4. **UNLESS --no-pr**: Push the branch and create a DRAFT PR using `gh pr create --draft`. The PR title must include the jira ticket. The PR body should summarize what was implemented, organized by sub-task. If there were unresolved gaps from Phase 6, include them in a "Known Gaps" section of the PR body.
5. **UNLESS --draft-pr**: Mark the PR ready and run the `my:pr-finalize` skill to finalize it. **This step is NOT optional. If the user did not pass `--draft-pr`, you MUST invoke `my:pr-finalize`. Do NOT skip it, forget it, or rationalize that the PR is "already done". The PR is NOT done until `my:pr-finalize` has run.**

   **PR FINALIZATION COMPLETION RULE (NON-NEGOTIABLE):**
   The `my:pr-finalize` skill runs a check-and-fix loop that polls for Bugbot completion and review comments. You MUST NOT consider the PR finalized, print the execution summary, or end your work until ALL of the following are confirmed true:
   1. The Cursor Bugbot check run `status` is `"completed"` (not `"in_progress"`, not `"queued"`).
   2. There are ZERO unresolved review threads from the `cursor` bot posted after the most recent push.
   3. There are ZERO failing compliance checks posted after the most recent push.

   **If Bugbot is slow:** Keep polling. Use `/loop 2m` or manual checks every 2-3 minutes. Do NOT cancel the loop, do NOT rationalize stopping early, do NOT cite "polling limits" as a reason to abandon. The CLAUDE.md guidance about spacing out polls means "wait between checks" — it does NOT mean "give up after N checks." You wait as long as it takes.

   **If Bugbot posts bug comments:** Fix them in the worktree, push, reply to the threads, resolve them, then re-enter the polling loop for the NEW commit's Bugbot run. Repeat until a clean Bugbot run completes with no new comments.

   **The execution summary (Phase 8) MUST NOT be printed until this rule is fully satisfied.** Printing the summary is your signal that ALL work is done. If you print it while Bugbot is still running, you have violated this rule.

# CRITICAL RULES

- **MODEL INHERITANCE: ALL sub-agents MUST run on the same model as the orchestrator.** On every Task tool call, you MUST explicitly set the `model` parameter to match YOUR current model (e.g. if you are Opus, set `model: "opus"`; if Sonnet, set `model: "sonnet"`). Do NOT omit the model parameter — without it, sub-agents may silently downgrade to a smaller model. This applies to EVERY Task tool invocation throughout ALL phases (implementation, review, validation, and fix sub-agents).
- You are the ORCHESTRATOR. You do NOT write production code or test code. You delegate ALL coding to sub-agents.
- Sub-agents must NEVER see the full plan document. They only see their individual sub-task file. The ONLY exception is the plan validation agent in Phase 6, which needs the full plan to compare against the implementation.
- Sub-task files must be COMPREHENSIVE — include every detail the sub-agent needs. The sub-agent cannot ask follow-up questions about the plan.
- Execute sub-tasks SEQUENTIALLY in dependency order. Do not parallelize unless tasks are explicitly independent.
- Always clean up temporary files before creating the PR.
- If a sub-agent's work is unsatisfactory, you may re-run it with a corrected sub-task description, but do NOT take over and write the code yourself.
- The validation loop (Phase 6) runs a MAXIMUM of 5 times. After 5 failed attempts, report to the user and let them decide.
- **MANDATORY PHASE ORDERING: Phases MUST execute in strict order: 1 → 2 → 3 → 4 → 5 → 6 → 7. You MUST NOT skip Phase 5 (Code Review) or Phase 6 (Plan Validation). These review phases are NON-NEGOTIABLE prerequisites for Phase 7 (PR creation). Proceeding to Phase 7 without completing both Phase 5 AND Phase 6 is a critical workflow violation. No exceptions — not for time, context length, confidence level, or any other reason.**
- **NO SELF-VALIDATION: The orchestrator MUST NOT perform validation checks itself. Phase 5 (Code Review) and Phase 6 (Plan Validation) MUST each launch a dedicated sub-agent via the Agent tool. "Doing it myself because it's faster/simpler/straightforward" is explicitly forbidden. The whole point of these phases is independent verification by a separate agent with fresh context.**
- **NO SKIPPING PR FINALIZATION: Unless `--draft-pr` was explicitly passed, Phase 7 step 5 MUST invoke `my:pr-finalize` via the Skill tool after marking the PR ready. The PR is incomplete without finalization. Do NOT stop at "PR created" or "PR marked ready" — you MUST run the finalize skill. Forgetting this step means the PR is unfinished.**
- **NO EARLY TERMINATION OF PR FINALIZATION: Once `my:pr-finalize` is invoked, you MUST wait for the Bugbot check to reach `status: "completed"` and all bug comments to be fixed and resolved BEFORE moving to Phase 8. You are FORBIDDEN from canceling the polling loop, printing the execution summary, or declaring the work done while Bugbot is still `in_progress`. "It's taking too long" is NOT a valid reason to stop. "Polling limits" means space out your checks — it does NOT mean give up. You poll until Bugbot completes, fix any bugs it finds, and only THEN proceed to Phase 8.**

# PHASE 8: EXECUTION SUMMARY (MANDATORY — DO NOT SKIP)

**After Phase 7 completes (or after Phase 6 if the user aborts), you MUST print a structured execution summary.** This gives the user a clear overview of what happened across all phases.

Print the following summary in markdown format:

```
## Execution Summary

### Phase 1: Setup
- Branch name and worktree location
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
- Sub-agents spawned: N
- Validation attempts: N
- Final result: PASS / FAIL (with gap count if applicable)
```

Adapt the summary to what actually happened — omit sections for phases that were skipped (e.g., Phase 4 if no integration tests were needed) but always include Phases 1-3, 5, 6, and 7.
