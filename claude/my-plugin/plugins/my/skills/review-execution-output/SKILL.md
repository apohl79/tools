---
description: Use when interactive execute-plan Phase 5 must run a deterministic review loop for completed execution output in the current agent.
---

# Review Execution Output

This helper runs the interactive execute-plan code-review loop.

It runs in the same agent as the interactive orchestrator. It coordinates reviewers, prompts, triage, retries, and verification. It does NOT write production code, test code, or apply fixes directly.

## Required Inputs

The orchestrator must pass, at minimum:

- `plan_path` — absolute plan path
- `execution_root` — absolute execution root
- `changed_files` — list of files created or modified during execution
- `language` — detected language
- `recipe_list` — recipe skills relevant to the changed code
- `skip_code_review` — explicit flag confirming interactive Phase 5 review is required now
- `prior_review_notes` — current review notes and prior review outcomes
- `review_state` — current helper-owned review state when already persisted in memory for this helper loop
- `review_state_path` — persisted helper-owned review-state path when the orchestrator resumes from stored state

If any required input is missing, stop and return `status: blocked` with the missing field in `notes`.

## State ownership and isolation

- This helper runs in the same agent as the interactive execute-plan orchestrator.
- Execution orchestration state remains orchestrator-owned. Review state remains helper-owned and separate from execution orchestration state and validation state.
- Reviewer isolation and review-fix isolation happen only in focused sub-agents underneath this helper.
- Persisted review metadata must include, when relevant, `skill_version`, `current_phase`, `current_attempt`, and the frozen reviewer set.
- Treat persisted helper-owned review state as authoritative when it exists. Never infer missing review state from memory when persisted state is available.

## Responsibilities

1. Freeze the reviewer set before the first review attempt.
   - Select the reviewer set once for this Phase 5 run.
   - The default required reviewer set is exactly three focused reviewers: one Claude reviewer, one Codex reviewer, and one Gemini reviewer.
   - Reuse the same reviewer set for every retry in this helper run.
   - Do not swap reviewers mid-loop unless the orchestrator restarts Phase 5 from scratch with new state.
   - Freezing the reviewer set is helper-owned, not orchestrator-owned.
   - Reducing the set below these three reviewers, replacing one with a generic fallback reviewer, or letting the orchestrator pick a smaller set is forbidden unless the helper returns `blocked` with a concrete tool-availability reason recorded in `notes`.
   - If the orchestrator already ran any direct reviewer outside this helper-owned set, treat that as out-of-band noise: do not count it as Phase 5 progress, and require the full frozen reviewer set to run anyway.
2. Generate reviewer prompts at the helper level.
   - The helper owns the reviewer prompt templates, reviewer temp-file naming, and reviewer prompt contract.
   - Each reviewer prompt must include the exact review scope, changed files, language and recipe context, prior review context, and the reporting contract.
   - Concern-specific reviewer prompt content belongs here, not in the orchestrator or in any non-interactive transport protocol file.
3. Apply prior-review-context rules.
   - Pass forward prior review outcomes so repeated issues are not rediscovered without context.
   - Distinguish already fixed findings from still-open findings.
   - Preserve rejected or deferred items so later attempts do not silently revive or erase them.
4. Triage every finding into exactly one bucket:
   - `FIX_REQUIRED` — the finding is accepted as real, in-scope, and must be fixed before Phase 5 can exit clean.
   - `VERIFIED_FIX` — a prior `FIX_REQUIRED` issue is now fixed and verified.
   - `REJECTED` — the finding is invalid, out of scope, or based on incorrect reviewer assumptions.
   - `DEFERRED` — the finding is real but intentionally left unresolved for a documented follow-up, user decision, or later phase.
5. Generate review-fix prompts when fixes are required.
   - The helper defines the fix prompt content.
   - The fix prompt must include only unresolved `FIX_REQUIRED` findings, exact files in scope, required verification commands, and instructions to avoid unrelated edits.
   - `VERIFIED_FIX`, `REJECTED`, and `DEFERRED` items must be carried forward as context, not re-fixed.
6. Enforce a review retry cap.
   - Maximum review attempts per Phase 5 run: 3.
   - If unresolved accepted findings remain after the cap, return `status: user_decision_required` with the unresolved findings and the exact decision needed.
7. Require post-fix regression verification.
   - After every fix attempt, rerun review-relevant verification before another review pass.
   - At minimum, rerun any targeted checks needed to confirm the reviewed behavior still works and no regression was introduced in the touched area.

## Reviewer Prompt Contract

Every reviewer prompt must instruct the reviewer to:

- review only the provided plan context, execution outputs, changed files, and prior-review context
- stay within the current Phase 5 scope
- avoid making code changes directly
- report concrete findings with file paths and reasoning
- classify findings so the helper can map them into `FIX_REQUIRED`, `VERIFIED_FIX`, `REJECTED`, or `DEFERRED`
- distinguish blocking `FIX_REQUIRED` findings from informational notes and already-verified fixes
- avoid re-raising findings already marked resolved, rejected, or deferred unless new evidence invalidates the prior decision

The helper must use the full frozen reviewer set for every review attempt, and that frozen set must remain stable across retries in the same run.
The required default frozen reviewer set is exactly three reviewers: Claude, Codex, and Gemini. Any deviation from that set requires a `blocked` result with explicit tool-availability evidence in `notes`.
The orchestrator or any caller must not treat a single reviewer, pair of reviewers, or ad-hoc alternate skill as equivalent to the helper-owned three-reviewer batch.

## Review Loop

1. Validate required inputs.
2. Freeze the reviewer set.
3. Build reviewer prompts using the helper-level contract.
4. Run review and collect findings through the full frozen reviewer set launched underneath this helper.
   - A review attempt is incomplete until all three reviewer outputs have been collected.
   - Do not declare `clean`, `fix_required`, or any other terminal review result from a partial reviewer batch.
5. Triage findings and merge them with prior review context.
   - If the helper detects that the orchestrator bypassed helper-owned review execution, return `status: blocked` unless the helper can still run the full required reviewer set in the current invocation.
   - A helper-owned clean result is valid only after the full frozen reviewer set has completed and been triaged.
6. If no unresolved `FIX_REQUIRED` findings remain, return `status: clean`.
7. If unresolved `FIX_REQUIRED` findings remain and the retry cap is not exhausted:
   - generate a review-fix prompt
   - delegate the review-fix work through a focused sub-agent underneath this helper
   - hand control back to the orchestrator through `status: fix_required`
   - require post-fix regression verification before the next review attempt
8. If the helper cannot continue because input, evidence, or execution state is insufficient, return `status: blocked`.
9. If the retry cap is exhausted and unresolved `FIX_REQUIRED` findings remain, return `status: user_decision_required`.
10. Never blur Phase 5 boundaries with validation logic. Review state changes stay inside helper-owned review state until the helper returns its deterministic result.

## Completion Contract

The helper completes only by returning one deterministic result object with these fields:

- `status`
- `next_step`
- `notes`
- `state_updates`, when the helper persists or advances helper-owned review state

Allowed `status` values:

- `clean`
- `fix_required`
- `blocked`
- `user_decision_required`

## Deterministic Helper Result Contract

### `status: clean`
Use when review is complete and no unresolved `FIX_REQUIRED` findings remain.

- `next_step`: proceed to interactive execute-plan Phase 6
- `notes`: reviewer set used, confirmation that all three required reviewers completed, review attempt count, triage summary, and any non-blocking observations

### `status: fix_required`
Use when accepted findings require a delegated fix pass.

- `next_step`: run one fix pass using the helper-generated review-fix prompt, run post-fix regression verification, then re-enter this helper with updated review state
- `notes`: unresolved accepted findings, affected files, verification required, and updated prior-review context

### `status: blocked`
Use when the helper cannot continue deterministically.

- `next_step`: supply the missing input, resolve the execution-state problem, or correct the review scope before re-entering this helper
- `notes`: exact blocker and the minimum data or action needed

### `status: user_decision_required`
Use when the retry cap is exhausted or a documented deferred item requires an explicit interactive decision.

- `next_step`: obtain the exact user decision recorded in `notes`, then either re-enter Phase 5 with updated state or stop execution as directed
- `notes`: unresolved findings, any `DEFERRED` items needing confirmation, attempt count, and the explicit decision required from the user
