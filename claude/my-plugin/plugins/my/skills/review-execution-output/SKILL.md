---
description: Use when interactive execute-plan Phase 5 must run a deterministic review loop for completed execution output in the current agent.
---

# Review Execution Output

This helper runs the interactive execute-plan code-review loop.

It runs in the same agent as the interactive orchestrator. It coordinates reviewer-team runs, triage, retries, and verification. It does NOT write production code, test code, or apply fixes directly. It does NOT directly launch reviewers — reviewer orchestration is fully delegated to `my:run-reviewer-team`.

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
- Reviewer isolation happens inside `my:run-reviewer-team` sub-invocations. Fix isolation happens in focused sub-agents dispatched by this helper.
- Persisted review metadata must include, when relevant, `skill_version`, `current_phase`, `current_attempt`, and the frozen reviewer set.
- Treat persisted helper-owned review state as authoritative when it exists. Never infer missing review state from memory when persisted state is available.

## Responsibilities

1. Delegate each reviewer-team run to `my:run-reviewer-team`.
   - Pass `plan_context`, `execution_outputs`, `changed_files`, `language`, `recipe_list`, and the accumulated `prior_review_context` for each invocation.
   - Do not freeze reviewer sets, build reviewer prompts, or launch reviewers directly. That is owned by `my:run-reviewer-team`.
   - If `my:run-reviewer-team` returns `status: blocked`, propagate `status: blocked` from this helper with the blocker detail.
   - A review attempt is complete only when `my:run-reviewer-team` returns `status: complete`.
   - Do not declare `clean`, `fix_required`, or any terminal result from a partial or missing reviewer-team report.
2. Apply prior-review-context rules.
   - Pass all prior triage history to each `my:run-reviewer-team` invocation so repeated issues are not rediscovered without context.
   - Distinguish already fixed findings from still-open findings.
   - Preserve rejected or deferred items so later attempts do not silently revive or erase them.
3. Triage the reviewer-team report.
   - Accept the triage produced by `my:run-reviewer-team` as-is.
   - Apply helper-level decisions only for `DEFERRED` items that require an explicit user decision.
4. Generate review-fix prompts when fixes are required.
   - The helper defines the fix prompt content.
   - EXACTLY ONE fix issue per prompt, EXACTLY ONE sub-agent per issue. This is mandatory.
   - If N unresolved `FIX_REQUIRED` issues exist, create N fix prompts and dispatch N fix sub-agents as a batch. Do not merge issues into a single prompt.
   - Each fix prompt must include only the single `FIX_REQUIRED` finding it is responsible for, the exact files in scope, required verification commands, and instructions to avoid unrelated edits.
   - Carry all `VERIFIED_FIX`, `REJECTED`, and `DEFERRED` items forward as context in every fix prompt so fix agents know what has already been handled.
5. Enforce a review retry cap.
   - Maximum review attempts per Phase 5 run: 3.
   - If unresolved accepted findings remain after the cap, return `status: user_decision_required` with the unresolved findings and the exact decision needed.
6. Require post-fix regression verification.
   - After every fix attempt, rerun review-relevant verification before invoking `my:run-reviewer-team` again.
   - At minimum, rerun any targeted checks needed to confirm the reviewed behavior still works and no regression was introduced in the touched area.

## Review Loop

1. Validate required inputs.
2. Invoke `my:run-reviewer-team` with all required inputs and the current `prior_review_context`.
3. If `my:run-reviewer-team` returns `status: blocked`, return `status: blocked` immediately with the blocker detail.
4. Merge the returned findings with prior review context.
   - If the helper detects that the orchestrator bypassed helper-owned review execution, return `status: blocked` unless this invocation can still run a full reviewer-team pass via `my:run-reviewer-team`.
5. If no unresolved `FIX_REQUIRED` findings remain, return `status: clean`.
6. If unresolved `FIX_REQUIRED` findings remain and the retry cap is not exhausted:
   - generate exactly one review-fix prompt per unresolved `FIX_REQUIRED` finding
   - dispatch exactly one focused fix sub-agent per prompt as a batch — never merge multiple issues into a single sub-agent
   - hand control back to the orchestrator through `status: fix_required`
   - require post-fix regression verification before the next reviewer-team run
7. If the helper cannot continue because input, evidence, or execution state is insufficient, return `status: blocked`.
8. If the retry cap is exhausted and unresolved `FIX_REQUIRED` findings remain, return `status: user_decision_required`.
9. Never blur Phase 5 boundaries with validation logic. Review state changes stay inside helper-owned review state until the helper returns its deterministic result.

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
- `notes`: reviewer set used (from `my:run-reviewer-team` report), confirmation that all three required reviewers completed, review attempt count, triage summary, and any non-blocking observations

### `status: fix_required`
Use when accepted findings require a delegated fix pass.

- `next_step`: run the helper-generated fix batch — one sub-agent per `FIX_REQUIRED` finding — then run post-fix regression verification, then re-enter this helper with updated review state
- `notes`: unresolved accepted findings (one entry per finding), affected files per finding, verification required, and updated prior-review context

### `status: blocked`
Use when the helper cannot continue deterministically.

- `next_step`: supply the missing input, resolve the execution-state problem, or correct the review scope before re-entering this helper
- `notes`: exact blocker and the minimum data or action needed

### `status: user_decision_required`
Use when the retry cap is exhausted or a documented deferred item requires an explicit interactive decision.

- `next_step`: obtain the exact user decision recorded in `notes`, then either re-enter Phase 5 with updated state or stop execution as directed
- `notes`: unresolved findings, any `DEFERRED` items needing confirmation, attempt count, and the explicit decision required from the user
