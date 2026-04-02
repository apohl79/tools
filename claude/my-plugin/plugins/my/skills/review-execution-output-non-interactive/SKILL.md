---
description: Use when non-interactive execute-plan Phase 5 must resume deterministic code review batches through prompt-file handoffs and persisted review state in the current agent.
---

# Review Execution Output Non-Interactive

This helper runs the non-interactive execute-plan Phase 5 review loop.

It runs in the same agent as the non-interactive orchestrator. It owns review-state persistence, reviewer freezing, prompt-file generation, handoff emission, resumed-output parsing, triage across retries, deterministic stop behavior, and regression-verification requirements after delegated fixes. It does NOT write production code, test code, or direct review fixes itself.

Execution orchestration state remains orchestrator-owned. Review state remains helper-owned and separate from execution orchestration state and helper-owned validation state.

## Required Inputs

The orchestrator must pass, at minimum:

- `plan_path` — absolute plan path
- `execution_root` — absolute execution root
- `changed_files` — files created or modified during execution
- `language` — detected language for the changed code
- `recipe_list` — recipe skills relevant to the changed code
- `skip_code_review` — explicit review-skip flag
- `state_file_path` — absolute path to `.tmp-execute-plan-state.json`
- `execution_state` — current persisted orchestrator state
- `review_state` — current persisted helper-owned review state, if any
- `review_state_path` — persisted helper-owned review-state path when review state is stored outside the immediate payload
- `prior_review_notes` — prior review findings, triage notes, and fix history

If any required input is missing, unreadable, or inconsistent with persisted state, stop and return `status: blocked`.

## Core Mode Contract

- This helper runs only in the same agent as `my:execute-plan-non-interactive`.
- This helper never writes implementation code, test code, review fixes, or validation fixes.
- This helper owns all Phase 5 review prompt-file content and Phase 5 review-state decisions.
- Reviewer isolation and review-fix isolation happen only in focused sub-agents underneath this helper.
- `execute-plan-non-interactive/HANDOFF_PROTOCOL.md` remains transport-only. This helper must follow its naming and continuation rules without redefining transport semantics.
- Persist helper-owned state before every stop that waits for external handoff output.
- Reread helper-owned state and orchestrator state before parsing resumed outputs.
- Persisted review metadata must include, when relevant, `skill_version`, `current_phase`, `current_attempt`, and the frozen reviewer set.

## Responsibilities

1. Freeze the reviewer set before the first review attempt.
   - Select the reviewer set once for the full Phase 5 run.
   - The default required reviewer set is exactly three focused reviewers: Claude, Codex, and Gemini.
   - Persist the frozen set in helper-owned review state.
   - Reuse the same reviewer set for every retry attempt in that Phase 5 run.
   - Reducing the set below these three reviewers, replacing one with a generic fallback reviewer, or letting the orchestrator choose a smaller set is forbidden unless the helper returns `blocked` with a concrete tool-availability reason recorded in `notes`.
   - If the orchestrator already ran any direct reviewer outside this helper-owned set, treat that as out-of-band noise: do not count it as Phase 5 progress, and require the full frozen reviewer set to run anyway.
2. Own review prompt-file naming.
   - Write exactly one prompt file per reviewer in the frozen three-reviewer set using `.tmp-subtask-review-attempt-<attempt>-<reviewer>.md`.
   - Reviewer files must include `.tmp-subtask-review-attempt-<attempt>-claude.md`, `.tmp-subtask-review-attempt-<attempt>-codex.md`, and `.tmp-subtask-review-attempt-<attempt>-gemini.md`.
   - A review batch is invalid if any of these required reviewer prompt files is missing without a helper-reported `blocked` reason.
3. Own review handoff emission.
   - Emit one transport line per prompt file using the handoff protocol.
   - Persist the exact expected handoffs for the current review batch before stopping.
4. Own resumed-output parsing for a full reviewer batch.
   - Reread `.tmp-execute-plan-state.json` and helper-owned review state before parsing continuation payloads.
   - Require exactly one `# output sub-agent <N>:` block for each expected reviewer handoff in the current batch.
   - Reject incomplete, duplicate, or unexpected output blocks using the transport protocol retry behavior.
   - Do not triage partial reviewer batches.
5. Own triage persistence across attempts.
   - Merge new findings with prior review context.
   - Persist triage history so accepted, rejected, deferred, and verified items remain stable across retries.
   - Never silently resurrect `REJECTED` or `DEFERRED` findings without new evidence recorded in state.
6. Own review-fix handoff generation.
   - When unresolved accepted findings remain, write exactly one fix prompt file per `FIX_REQUIRED` finding using `.tmp-subtask-review-fix-attempt-<attempt>-<N>.md`, where N increments per issue.
   - If N unresolved `FIX_REQUIRED` issues exist, emit N fix prompt files as a single batch. Do not merge multiple issues into one prompt file.
   - Each fix prompt must contain only the single `FIX_REQUIRED` finding it is responsible for, the exact files in scope, and required verification commands.
   - Carry `VERIFIED_FIX`, `REJECTED`, and `DEFERRED` items forward as context in every fix prompt so fix agents know what has already been handled.
7. Own review cap and deterministic stop behavior.
   - Maximum review attempts per Phase 5 run: 3.
   - Stop immediately after emitting a review batch or review-fix batch.
   - Stop immediately on missing inputs, invalid persisted state, incomplete continuation payload, or terminal review result.

## Reviewer Prompt Contract

Every reviewer prompt must instruct the reviewer to:

- review only the provided plan context, changed files, execution output, and prior review context
- stay within current non-interactive Phase 5 scope
- avoid making code changes directly
- report concrete findings with file paths and reasoning
- distinguish new findings from prior findings already triaged
- supply enough detail for deterministic triage into `FIX_REQUIRED`, `VERIFIED_FIX`, `REJECTED`, or `DEFERRED`
- avoid re-raising items already marked `VERIFIED_FIX`, `REJECTED`, or `DEFERRED` unless new evidence invalidates the prior disposition

## Triage Contract

Every concrete review item must be persisted in exactly one state compatible bucket:

- `FIX_REQUIRED` — accepted, in-scope, unresolved, and must be fixed before review can complete cleanly
- `VERIFIED_FIX` — a prior `FIX_REQUIRED` item is now fixed and verified
- `REJECTED` — invalid, out of scope, duplicate, or based on incorrect assumptions
- `DEFERRED` — real but intentionally left unresolved for a documented follow-up, later phase, or explicit decision outside this helper

Triage rules:

- `FIX_REQUIRED` items remain active until promoted to `VERIFIED_FIX`, reclassified to `REJECTED`, or reclassified to `DEFERRED` with documented rationale.
- `VERIFIED_FIX` items must retain the original finding identity and verification evidence.
- `REJECTED` items must retain the rejection rationale.
- `DEFERRED` items must retain the documented owner or follow-up requirement.

## Review Loop

1. Validate required inputs against persisted orchestrator state.
2. This helper runs only when `skip_code_review=false`. If review is skipped by plan type, the orchestrator must bypass this helper entirely.
3. Initialize or reread helper-owned review state.
4. Freeze and persist the reviewer set if this is the first review attempt.
5. Write reviewer prompt files for the current attempt.
6. Emit review handoff lines for the full frozen three-reviewer set, persist expected handoffs, and return `status: waiting_for_handoffs`.
7. On resume, reread state before parsing reviewer outputs.
8. Require a complete reviewer batch before evaluation.
   - A complete batch means exactly one output block for Claude, one for Codex, and one for Gemini, matching the persisted frozen reviewer set.
   - Do not declare `clean`, `fix_required`, or any other terminal review result from a partial reviewer batch.
9. Triage findings and persist merged review state.
10. If no unresolved `FIX_REQUIRED` items remain, return `status: clean`.
   - A clean result is valid only after the full frozen three-reviewer batch completed and was triaged.
11. If the helper detects orchestrator bypass or missing required reviewer outputs without a helper-owned blocked reason, return `status: blocked`.
12. If unresolved `FIX_REQUIRED` items remain and the cap is not exhausted, write exactly one review-fix prompt file per issue, emit one handoff line per prompt file as a batch, persist state, and return `status: fix_required`. The number of emitted fix handoffs MUST equal the number of unresolved `FIX_REQUIRED` items. Never merge multiple issues into a single prompt file or handoff.
13. After the delegated fix pass completes, require explicit regression verification before the next review attempt. Regression verification means rerunning the required review-relevant checks for the touched files or workflow and persisting the verification outcome in `state_updates` before re-entering this helper.
14. If the helper cannot continue deterministically, return `status: blocked`.
15. If the retry cap is exhausted and unresolved `FIX_REQUIRED` items remain, return `status: abort` with deterministic stop notes.

## Deterministic Result Contract

The helper must return exactly one result object with:

- `status`
- `next_step`
- `notes`
- `state_updates`

Allowed `status` values:

- `clean`
- `fix_required`
- `waiting_for_handoffs`
- `blocked`
- `abort`

### `status: clean`
Use when review is complete and no unresolved `FIX_REQUIRED` items remain.

- `next_step`: proceed to non-interactive execute-plan Phase 6
- `notes`: reviewer set, attempt count, triage summary, and any non-blocking review notes
- `state_updates`: authoritative review-state persistence, including frozen reviewers and final triage snapshot

### `status: fix_required`
Use when accepted findings require a delegated review-fix handoff batch.

- `next_step`: execute the emitted review-fix batch — one handoff per `FIX_REQUIRED` finding — persist resulting execution output, then re-enter this helper for the next review attempt
- `notes`: one entry per unresolved `FIX_REQUIRED` finding with its affected files and verification context, plus review-fix batch metadata
- `state_updates`: authoritative review-state persistence, including attempt counters, per-issue active findings, and emitted fix-handoff metadata

### `status: waiting_for_handoffs`
Use when a review batch has been emitted and the helper must stop for the full reviewer batch output.

- `next_step`: provide one output block for each emitted reviewer handoff, then continue non-interactive execution
- `notes`: emitted batch metadata, frozen reviewer set, and continuation requirements
- `state_updates`: authoritative review-state persistence, including current attempt, batch metadata, and expected handoffs

### `status: blocked`
Use when the helper cannot continue because required inputs, persisted state, or continuation data are insufficient or inconsistent.

- `next_step`: provide the missing or corrected input/state, then re-enter Phase 5
- `notes`: exact blocker and the minimum corrective action required
- `state_updates`: any safe persistence needed to preserve the blocker context without advancing review

### `status: abort`
Use when deterministic review cannot continue, including review-cap exhaustion with unresolved `FIX_REQUIRED` items.

- `next_step`: stop execution or restart Phase 5 from a new orchestrator decision point with explicitly updated state
- `notes`: unresolved findings, attempt count, stop reason, and exact condition that caused termination
- `state_updates`: final authoritative review-state snapshot for the aborted run
