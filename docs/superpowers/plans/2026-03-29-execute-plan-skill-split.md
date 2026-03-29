# Execute Plan Skill Split Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split `my:execute-plan` into symmetric interactive and non-interactive execution, review, and validation skills with explicit boundaries and persisted helper state.

**Architecture:** Keep execution orchestration in `my:execute-plan` and `my:execute-plan-non-interactive`, and move Phase 5 review logic and Phase 6 validation logic into dedicated helper skills. The helper skills run in the same agent as the orchestrator, but they generate narrow reviewer and validator prompts and manage their own state, retry loops, and prompt files. Shared non-interactive transport rules stay in `HANDOFF_PROTOCOL.md`, while review-specific and validation-specific prompt rules move into their respective helper skills.

**Tech Stack:** Claude skill markdown files, existing `HANDOFF_PROTOCOL.md`, repository docs under `docs/superpowers/`

---

## File Structure

### Existing files to modify
- `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md` — reduce to interactive orchestration only and replace embedded review/validation sections with helper-skill invocation contracts.
- `claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md` — keep shared transport contract, remove concern-specific rules that belong in review/validation helpers if needed.

### New files to create
- `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md` — non-interactive execution orchestrator.
- `claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md` — interactive review helper.
- `claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md` — non-interactive review helper.
- `claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md` — interactive validation helper.
- `claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md` — non-interactive validation helper.

### Existing docs to create/update during planning work
- `docs/superpowers/plans/2026-03-29-execute-plan-skill-split.md` — this plan file.

## Task 1: Carve interactive orchestration out of the current execute-plan skill

**Files:**
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md`
- Test: manual readback of `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md`

- [ ] **Step 1: Write the failing test**

Create a checklist of content that must disappear from the interactive skill after the split:

```text
The interactive skill must no longer contain:
- --non-interactive mode selection as a supported execution path
- non-interactive-only deterministic stop rules
- prompt-file emission for review and validation helpers
- resumed-output parsing instructions for review and validation helpers
- validation/review helper internal prompt templates

The interactive skill must still contain:
- interactive setup
- interactive task decomposition
- interactive wave execution
- interactive integration testing orchestration
- helper-skill invocation points for review and validation
- cleanup / PR / summary flow
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
grep -n "NON_INTERACTIVE=true\|# output sub-agent\|review-attempt\|validation_attempt" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: PASS with multiple matches showing the current file still mixes interactive and non-interactive review/validation logic.

- [ ] **Step 3: Write minimal implementation**

Rewrite `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md` so its frontmatter and body match this shape:

```markdown
---
description: Use when a READY interactive implementation plan should be executed in the current repository or worktree.
argument-hint: [plan-document] [jira-ticket] [--no-worktree] [--no-pr] [--draft-pr]
---

Execute the explicitly provided plan path, or resolve a single READY plan during setup.

You are the INTERACTIVE ORCHESTRATOR. You coordinate execution of a development plan by delegating implementation tasks to focused sub-agents. You NEVER write production code or test code yourself.

# PHASE 1: SETUP
- Resolve the plan document interactively.
- Validate required headers and `**Status:** READY`.
- Resolve plan type and execution flags.
- Sync git and setup/reuse worktree unless `--no-worktree`.
- Record language recipe requirements.
- Mark the plan `EXECUTING` only after setup succeeds.

# PHASE 2: TASK DECOMPOSITION
- Produce numbered sub-tasks and wave groupings.
- Create progress tasks.

# PHASE 3: WAVE-BASED EXECUTION
- Create `.tmp-subtask-<N>.md` files in the execution root.
- Launch interactive implementation sub-agents only.
- Review outputs, rerun failed task descriptions when needed, and update dependency context.

# PHASE 4: INTEGRATION TESTING
- Launch integration-test sub-agent only when deferred tests exist.
- Rerun with dedicated fix sub-agent if verification fails.

# PHASE 5: CODE REVIEW
- If `SKIP_CODE_REVIEW=true`, mark Code Review completed and continue.
- Otherwise invoke `my:review-execution-output` in the current agent.
- Pass the review helper explicit inputs: plan path, execution root, changed files, language, recipe list, current review notes, and review-relevant state.
- The helper owns reviewer freezing, prompt generation, triage, retries, and regression verification.

# PHASE 6: PLAN VALIDATION
- Invoke `my:validate-execution-plan` in the current agent.
- Pass the validation helper explicit inputs: full plan path, execution root, changed files, language, recipe list, current validation state, and any prior gap notes.
- The helper owns validator prompts, gap batching, retries, and proceed/abort behavior.

# PHASE 7: CLEANUP AND PR
- Delete obsolete temp files.
- Run final verification.
- Commit changes.
- Create or skip PR based on flags.
- Invoke `my:pr-finalize` when normal PR flow is enabled.
- Mark the plan `COMPLETED` only after all Phase 7 work finishes.

# PHASE 8: EXECUTION SUMMARY
- Print the structured execution summary.
```

Preserve the existing interactive safety rules that still apply, including:
- READY plan validation
- plan type detection and `SKIP_CODE_REVIEW` / `SKIP_PR`
- git fetch and default-branch resolution
- worktree dirty checks and rewritten-history guard
- no direct code writing by the orchestrator
- mandatory phase ordering
- mandatory summary

Delete interactive-file content that only exists to support non-interactive execution, review, or validation.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
grep -n "NON_INTERACTIVE=true\|# output sub-agent\|call sub-agent .*agent-type" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: FAIL with no matches for non-interactive transport language.

Then run:
```bash
grep -n "my:review-execution-output\|my:validate-execution-plan" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: PASS with helper-skill invocation instructions present.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
git commit -m "refactor: isolate interactive execute-plan orchestration"
```

## Task 2: Create the non-interactive execution orchestrator skill

**Files:**
- Create: `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md`
- Test: manual readback of the new skill and shared handoff protocol

- [ ] **Step 1: Write the failing test**

Create a checklist for the new non-interactive skill:

```text
The new skill must contain:
- explicit plan-path requirement
- no AskUserQuestion path
- no direct Task/Agent orchestration for implementation work
- prompt-file emission for execution batches
- execution state persistence rules
- helper-skill invocation points for non-interactive review and validation
- deterministic stop behavior
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
test -f "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md"
```

Expected: FAIL because the file does not exist yet.

- [ ] **Step 3: Write minimal implementation**

Create `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md` with this structure:

```markdown
---
description: Use when a READY implementation plan should be executed through deterministic non-interactive handoffs with an explicit plan path.
argument-hint: [plan-document] [jira-ticket] [--no-worktree] [--no-pr] [--draft-pr]
---

You are the NON-INTERACTIVE ORCHESTRATOR. You coordinate execution by writing prompt files, updating persisted state, and stopping for resumed outputs. You NEVER write production code or test code yourself.

# PHASE 1: SETUP
- Require an explicit plan path.
- Reject missing or unreadable plans with deterministic errors.
- Validate `**Status:** READY`, plan type, and execution flags.
- Resolve execution root and worktree behavior.
- Initialize persisted execution state.

# PHASE 2: TASK DECOMPOSITION
- Produce numbered sub-tasks and wave groupings.
- Record wave metadata in persisted state.

# PHASE 3: WAVE-BASED EXECUTION
- Emit iteration-safe `.tmp-subtask-wave-<wave>-batch-<batch>-<N>.md` files.
- Print `call sub-agent N (agent-type: claude): <absolute-path>` lines.
- Persist expected outputs before stopping.
- On resume, reread state, map `# output sub-agent N:` blocks deterministically, review the batch, and continue.

# PHASE 4: INTEGRATION TESTING
- Emit integration prompt files and state updates when needed.
- Use non-interactive cleanup-fix style reruns when verification fails.

# PHASE 5: CODE REVIEW
- If `SKIP_CODE_REVIEW=true`, skip deterministically.
- Otherwise invoke `my:review-execution-output-non-interactive` in the current agent.
- Pass explicit structured state instead of relying on ambient context.

# PHASE 6: PLAN VALIDATION
- Invoke `my:validate-execution-plan-non-interactive` in the current agent.
- Pass explicit structured state instead of relying on ambient context.

# PHASE 7: CLEANUP AND PR
- Delete obsolete prompt files only after outputs are processed.
- Emit cleanup-fix handoffs if final verification fails.
- Commit locally unless `SKIP_PR=true` suppresses the whole PR path.
- Create draft PRs or deterministic non-PR summaries based on flags.

# PHASE 8: EXECUTION SUMMARY
- Print the same structured phase summary, describing handoff batches instead of direct sub-agent launches.
```

Update `claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md` so it remains transport-only. Keep rules for:
- prompt-file naming contracts
- `call sub-agent` line format
- output-block mapping
- `.tmp-execute-plan-state.json` lifecycle
- continuation parsing
- allowed `agent-type` metadata

Remove any prompt-body policy that belongs specifically to review or validation helpers.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
test -f "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md"
```

Expected: PASS.

Then run:
```bash
grep -n "AskUserQuestion\|Agent tool" "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md"
```

Expected: FAIL for `AskUserQuestion` and no direct implementation-agent launch instructions.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md"
git commit -m "feat: add non-interactive execute-plan skill"
```

## Task 3: Extract interactive review logic into a dedicated helper skill

**Files:**
- Create: `claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md`
- Test: manual readback of helper file and invocation contract in execute-plan skill

- [ ] **Step 1: Write the failing test**

Create a checklist of review behaviors that must move out of the orchestrator:

```text
Move out of execute-plan interactive skill:
- reviewer-set freezing
- review prompt templates
- prior-review-context rules
- triage buckets VERIFIED_FIX / REJECTED / DEFERRED
- review-fix prompt generation
- review retry cap
- post-fix regression verification
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
grep -n "VERIFIED_FIX\|REJECTED\|DEFERRED\|review-attempt\|No issues found" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: PASS with matches showing the interactive orchestrator still embeds review helper logic.

- [ ] **Step 3: Write minimal implementation**

Create `claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md` with this structure:

```markdown
---
description: Use when interactive execute-plan orchestration has finished implementation and integration work and needs a dedicated review loop.
argument-hint: [plan-path] [execution-root] [changed-files-json-or-path] [language] [review-state-json-or-path]
---

You are the INTERACTIVE REVIEW HELPER. You run in the same agent as the orchestrator. You do NOT write production code yourself.

## Required inputs
- plan path
- execution root
- changed files
- language
- recipe list
- skip-code-review flag
- prior review notes
- persisted helper metadata when resuming the same loop

## Responsibilities
- freeze the reviewer set once
- build narrow reviewer prompts
- launch the recipe reviewer and optional Codex/Gemini reviewers
- collect review results
- triage each CRITICAL and IMPORTANT finding into `VERIFIED_FIX`, `REJECTED`, or `DEFERRED`
- generate review-fix prompts for verified issues only
- run regression verification after fixes
- repeat until a clean iteration or review cap is reached

## Reviewer prompt contract
- reviewers receive only file lists, recipe lists, and prior triage context
- reviewers do not read the full plan
- reviewers must not modify files

## Completion contract
- return updated review notes and final status to the orchestrator
- if the cap is reached with unresolved verified issues, return a failure summary instead of silently proceeding
```

Then reduce the review section in `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md` to a helper invocation contract only.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
grep -n "VERIFIED_FIX\|REJECTED\|DEFERRED\|review-attempt\|No issues found" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: FAIL with those review-template details removed from the orchestrator.

Then run:
```bash
grep -n "freeze the reviewer set\|VERIFIED_FIX\|REJECTED\|DEFERRED" "claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md"
```

Expected: PASS in the helper skill.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
git commit -m "refactor: extract interactive review helper skill"
```

## Task 4: Extract non-interactive review logic into a dedicated helper skill

**Files:**
- Create: `claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md`
- Test: manual readback of helper file and invocation contract in non-interactive execute-plan skill

- [ ] **Step 1: Write the failing test**

Create a checklist for the non-interactive review helper:

```text
The helper must own:
- frozen reviewer-set persistence
- review prompt-file naming
- review handoff emission
- resumed-output parsing for a full reviewer batch
- triage persistence across attempts
- review-fix handoff generation
- review cap and deterministic stop behavior
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
test -f "claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md"
```

Expected: FAIL because the helper does not exist yet.

- [ ] **Step 3: Write minimal implementation**

Create `claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md` with this structure:

```markdown
---
description: Use when non-interactive execute-plan orchestration needs a dedicated deterministic review loop with persisted reviewer state.
argument-hint: [plan-path] [execution-root] [changed-files-json-or-path] [language] [review-state-json-or-path]
---

You are the NON-INTERACTIVE REVIEW HELPER. You run in the same agent as the orchestrator. You do NOT write production code yourself.

## Required inputs
- plan path
- execution root
- changed files
- language
- recipe list
- skip-code-review flag
- state file path
- prior review notes

## Responsibilities
- freeze reviewer set once and persist it
- write `.tmp-subtask-review-attempt-<attempt>-claude.md` and related reviewer prompt files
- emit review handoff lines with `agent-type` metadata outside the prompt body
- reread persisted state before parsing resumed outputs
- require all outputs for the emitted reviewer batch before evaluating the round
- triage into `VERIFIED_FIX`, `REJECTED`, and `DEFERRED`
- emit `.tmp-subtask-review-fix-attempt-<attempt>-1.md` when verified issues remain
- stop deterministically when the cap is reached
```

Update the non-interactive execution skill so Phase 5 only invokes this helper and no longer embeds the internal review prompt templates.

Keep `HANDOFF_PROTOCOL.md` transport-only while ensuring its naming rules still cover review attempt and review-fix prompt files.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
test -f "claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md"
```

Expected: PASS.

Then run:
```bash
grep -n "review-attempt-<attempt>-claude\|VERIFIED_FIX\|DEFERRED" "claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md"
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md"
git commit -m "feat: add non-interactive review helper skill"
```

## Task 5: Extract interactive validation logic into a dedicated helper skill

**Files:**
- Create: `claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md`
- Test: manual readback of helper file and invocation contract in execute-plan skill

- [ ] **Step 1: Write the failing test**

Create a checklist of validation behaviors that must move out of the interactive orchestrator:

```text
Move out of execute-plan interactive skill:
- validation prompt template
- validation report structure details
- gap batching rules
- validation-fix task prompt generation
- validation attempt loop
- proceed/abort decision behavior after 5 failures
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
grep -n "STATUS\|IMPLEMENTED\|GAPS\|DEVIATIONS\|validation_attempt\|Fix ONLY the issue described here" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: PASS with matches showing the interactive orchestrator still embeds validation helper logic.

- [ ] **Step 3: Write minimal implementation**

Create `claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md` with this structure:

```markdown
---
description: Use when interactive execute-plan orchestration needs a dedicated validation loop that compares the full plan against implementation output.
argument-hint: [plan-path] [execution-root] [changed-files-json-or-path] [language] [validation-state-json-or-path]
---

You are the INTERACTIVE VALIDATION HELPER. You run in the same agent as the orchestrator. You do NOT write production code yourself.

## Required inputs
- full plan path
- execution root
- changed files
- language
- recipe list
- skip-code-review flag
- current validation attempt
- prior validation gaps and deviations

## Responsibilities
- build the validator prompt with the full plan and changed-file list
- launch the validator sub-agent
- evaluate `PASS` or `FAIL`
- convert each GAP into a narrow validation-fix prompt
- dispatch validation-fix sub-agents
- rerun full verification
- trigger re-review when code review is not skipped
- enforce the 5-attempt cap and return proceed/abort control to the orchestrator when needed
```

Then reduce the validation section in `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md` to a helper invocation contract only.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
grep -n "STATUS\|IMPLEMENTED\|GAPS\|DEVIATIONS\|validation_attempt\|Fix ONLY the issue described here" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
```

Expected: FAIL with validation-template details removed from the orchestrator.

Then run:
```bash
grep -n "GAPS\|DEVIATIONS\|5-attempt cap" "claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md"
```

Expected: PASS in the helper skill.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md"
git commit -m "refactor: extract interactive validation helper skill"
```

## Task 6: Extract non-interactive validation logic into a dedicated helper skill

**Files:**
- Create: `claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md`
- Test: manual readback of helper file and invocation contract in non-interactive execute-plan skill

- [ ] **Step 1: Write the failing test**

Create a checklist for the non-interactive validation helper:

```text
The helper must own:
- validation prompt-file naming
- validator handoff emission
- resumed-output parsing for validator output
- GAP-to-fix prompt generation
- validation-fix batching
- persisted validation attempt state
- deterministic stop summary after the cap is reached
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
test -f "claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md"
```

Expected: FAIL because the helper does not exist yet.

- [ ] **Step 3: Write minimal implementation**

Create `claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md` with this structure:

```markdown
---
description: Use when non-interactive execute-plan orchestration needs a deterministic validation loop with persisted validation state.
argument-hint: [plan-path] [execution-root] [changed-files-json-or-path] [language] [validation-state-json-or-path]
---

You are the NON-INTERACTIVE VALIDATION HELPER. You run in the same agent as the orchestrator. You do NOT write production code yourself.

## Required inputs
- full plan path
- execution root
- changed files
- language
- recipe list
- skip-code-review flag
- state file path
- current validation attempt
- prior validation gaps and deviations

## Responsibilities
- write `.tmp-subtask-plan-validation-attempt-<attempt>.md`
- emit `call sub-agent 1 (agent-type: claude): <absolute-path>` for the validator
- reread persisted state before parsing resumed outputs
- require `# output sub-agent 1:` before evaluating validation status
- convert GAPs into `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md` files
- batch validation fixes with at most 5 handoffs at a time
- rerun code review through the review helper when code review is not skipped
- stop deterministically after 5 failed attempts and return the required failure summary
```

Update the non-interactive execution skill so Phase 6 only invokes this helper and no longer embeds validator prompt templates.

Keep `HANDOFF_PROTOCOL.md` transport-only while ensuring its naming rules still cover validation attempt and validation-fix prompt files.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
test -f "claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md"
```

Expected: PASS.

Then run:
```bash
grep -n "plan-validation-attempt\|validation-fix-attempt\|5 failed attempts" "claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md"
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md"
git commit -m "feat: add non-interactive validation helper skill"
```

## Task 7: Add explicit helper-state contracts and final consistency checks

**Files:**
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md`
- Modify: `claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md`
- Test: manual readback of all six skill files and the handoff protocol

- [ ] **Step 1: Write the failing test**

Create a consistency checklist:

```text
All six skills must agree on:
- helper inputs
- state ownership
- temp-file naming
- phase boundaries
- who freezes reviewer sets
- who owns validation caps
- that helper skills run in the same agent as the orchestrator
- that reviewer/validator isolation happens in focused sub-agents underneath helpers
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
grep -R -n "same agent as the orchestrator\|state file path\|frozen reviewer set\|current validation attempt" "claude/my-plugin/plugins/my/skills"
```

Expected: FAIL or incomplete coverage because the new helper files are not all aligned yet.

- [ ] **Step 3: Write minimal implementation**

Update the six skill files and `HANDOFF_PROTOCOL.md` so they all state these contracts explicitly:

```text
Shared helper contract:
- plan path
- execution root
- changed-files list
- language
- recipe list
- skip flags
- prior helper notes
- persisted helper state path when applicable

State isolation contract:
- execution orchestration state is separate from review state and validation state
- persisted metadata includes skill version, current phase, current attempt, and frozen reviewer set where relevant

Isolation contract:
- helper skills run in the same agent as the orchestrator
- reviewers and validators run as focused sub-agents or deterministic external handoffs underneath helpers
```

Ensure `HANDOFF_PROTOCOL.md` documents only transport concerns and references helper skills for concern-specific prompt content.

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
grep -R -n "same agent as the orchestrator\|state file path\|frozen reviewer set\|current validation attempt" "claude/my-plugin/plugins/my/skills"
```

Expected: PASS with matches in the relevant helper files.

Then run:
```bash
grep -n "transport-only\|prompt-file naming\|call sub-agent" "claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md"
```

Expected: PASS.

- [ ] **Step 5: Commit**

Run:
```bash
git add "claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md" "claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md" "claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md" "claude/my-plugin/plugins/my/skills/execute-plan/HANDOFF_PROTOCOL.md"
git commit -m "refactor: align execute-plan helper contracts"
```

## Self-Review

### Spec coverage
- Responsibility split: covered by Tasks 1-6.
- Symmetric interactive/non-interactive skill set: covered by Tasks 2, 4, and 6.
- Helper-skill same-agent boundary: covered by Tasks 3, 5, and 7.
- Explicit helper inputs and persisted state: covered by Tasks 2, 4, 6, and 7.
- Transport-only handoff protocol: covered by Tasks 2, 4, 6, and 7.

### Placeholder scan
- No `TODO`, `TBD`, or “similar to” placeholders remain.
- Every code-changing step names exact files and exact command checks.

### Type consistency
- Helper names are consistent across all tasks:
  - `my:execute-plan`
  - `my:execute-plan-non-interactive`
  - `my:review-execution-output`
  - `my:review-execution-output-non-interactive`
  - `my:validate-execution-plan`
  - `my:validate-execution-plan-non-interactive`
- Review triage bucket names are consistent: `VERIFIED_FIX`, `REJECTED`, `DEFERRED`.
- Validation temp-file names are consistent: `.tmp-subtask-plan-validation-attempt-<attempt>.md` and `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md`.
