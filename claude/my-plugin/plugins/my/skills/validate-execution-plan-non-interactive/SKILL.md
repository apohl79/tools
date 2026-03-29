---
name: validate-execution-plan-non-interactive
description: Use when non-interactive execute-plan orchestration needs a deterministic validation loop with persisted validation state.
argument-hint: [plan-path] [execution-root] [changed-files-json-or-path] [language] [validation-state-json-or-path]
---

# Validate Execution Plan Non-Interactive

You are the NON-INTERACTIVE VALIDATION HELPER. You run in the same agent as the orchestrator. You do NOT write production code directly, and you do NOT write test code directly.

## Required inputs

- `plan_path` — full plan path
- `execution_root` — execution root
- `changed_files` — changed files
- `language` — language
- `recipe_list` — recipe list
- `skip_code_review` — skip-code-review flag
- `state_file_path` — state file path
- `execution_state` — execution orchestration state
- `validation_state` — current helper-owned validation state when already available
- `validation_state_path` — persisted helper-owned validation-state path when state is resumed from storage
- `current_validation_attempt` — current validation attempt
- `prior_validation_notes` — prior validation notes, including prior GAPS and DEVIATIONS
- `prior_helper_outcomes` — prior helper outcomes needed to continue the same validation loop deterministically

## Same-agent boundary

- This helper runs in the same agent as `my:execute-plan-non-interactive`.
- Execution orchestration state remains orchestrator-owned. Validation state remains helper-owned and separate from execution orchestration state and helper-owned review state.
- This helper may emit focused validator or validation-fix handoffs underneath the helper.
- Validator isolation and validation-fix isolation happen only underneath this helper.
- This helper must not directly edit production code or test code.
- All implementation changes required to close validation gaps must be performed by delegated validation-fix handoffs.

## Responsibilities

- Own validation prompt-file naming by writing `.tmp-subtask-plan-validation-attempt-<attempt>.md` for validator attempts.
- Own validator handoff emission by printing `call sub-agent 1 (agent-type: claude): <absolute-path>` for the active validator prompt file.
- Own resumed-output parsing for validator output by rereading persisted state first and requiring exactly one `# output sub-agent 1:` block for the active validator handoff.
- Own GAP-to-fix prompt generation by converting each actionable GAP, and each required DEVIATION correction, into a narrow validation-fix prompt.
- Own validation-fix batching by writing `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md` prompt files and emitting at most 5 validation-fix handoffs per batch.
- Own persisted validation attempt state, including skill version, current phase, current attempt number, active validation batch, parsed validator outcome, unresolved items, completed fix batches, any pending re-review requirement, and final cap summary when relevant.
- Own deterministic stop summary after the cap is reached by returning the same unresolved-item summary every time the helper is re-entered without new authoritative state.
- Trigger re-review through `my:review-execution-output-non-interactive` after validation fixes changed implementation when code review is not skipped, and require that helper to return `status: clean` before the next validation attempt starts.
- If that re-review returns `waiting_for_handoffs`, `fix_required`, `blocked`, or `abort`, propagate that outcome deterministically through this helper instead of silently continuing to the next validation attempt.
- Persist the re-review outcome in `state_updates` so the next validation re-entry knows whether review is complete, waiting, blocked, or terminal.
- Return deterministic helper status, next action, notes, and state updates to the orchestrator after each helper invocation.

## Prompt-file naming contract

- Validator prompt files must use `.tmp-subtask-plan-validation-attempt-<attempt>.md`.
- Validation-fix prompt files must use `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md`.
- `<attempt>` is the persisted validation attempt counter starting at 1.
- `<N>` is the 1-based index within the emitted validation-fix batch.
- Prompt-file bodies must stay concern-specific and must not embed transport metadata such as `agent-type`.

## Persisted validation state contract

Persist or update helper-owned validation state before every deterministic stop. The validation state must remain separate from execution orchestration state and must include at minimum:

- `skill_version`
- `phase`, fixed to `plan_validation` or `validation_fix` while this helper owns the loop
- `validation_attempt`
- `frozen_reviewer_set`, when a required re-review outcome must remain stable across validation-triggered review reruns
- `current_batch`
- `validator_prompt_file`
- `validator_handoff`, when a validator handoff is waiting for output
- `validator_report_status`, once parsed
- `unresolved_gaps`
- `unresolved_deviations`
- `actionable_fix_items`
- `completed_fix_batches`
- `pending_review_rerun`, when validation fixes changed code and code review is required
- `cap_summary`, once 5 failed attempts have been reached
- `post_cap_decision`, one of `proceed`, `abort`, or `unset`, once a fresh rerun records the authoritative decision after the cap

Treat persisted validation state as authoritative on resume. Never infer missing validation state from memory when persisted state is available.

## Validator prompt contract

The validator prompt must:

- include the absolute full plan path or the full plan contents needed for validation
- include the absolute execution root
- include the exact changed-file list
- include prior validation notes when rerunning after failed attempts
- tell the validator to compare plan requirements against the actual implementation only
- tell the validator not to modify files
- require a deterministic report using this structure:

```text
STATUS: PASS | FAIL
IMPLEMENTED:
- <implemented requirement or notable delivered behavior>

GAPS:
- <missing or incomplete requirement>

DEVIATIONS:
- <behavior that differs from the plan>

NOTES:
- <concise context needed for the next attempt>
```

Validation report rules:

- `STATUS` is mandatory and must be either `PASS` or `FAIL`.
- `IMPLEMENTED` is mandatory on every run, even when validation fails.
- `GAPS` is mandatory on every run. Use `- none` only when there are no remaining gaps.
- `DEVIATIONS` is mandatory on every run. Use `- none` only when there are no deviations.
- `NOTES` is mandatory and must stay concise.
- Treat malformed reports as `blocked` because the helper cannot safely continue without the required structure.

## GAP extraction and validation-fix batching

- Extract GAPS and required DEVIATIONS deterministically from the validator report.
- Convert each actionable item into a separate candidate fix item unless multiple items are inseparable and touch the same narrow requirement.
- Never mix unrelated GAPS into one validation-fix prompt.
- Preserve stable numbering for GAPS and DEVIATIONS across retries when possible so later attempts can reference prior outcomes deterministically.
- If more than 5 actionable fix items remain, split them into sequential validation-fix batches of at most 5 handoffs each.
- A validation-fix batch is complete only when every handoff listed in persisted state has exactly one resumed output block.
- Do not rerun validation until every emitted validation-fix batch for the current attempt has been collected and processed.

## Validation-fix prompt generation

Every validation-fix prompt must:

- identify the exact GAP or required DEVIATION correction being addressed
- include only the minimum plan excerpt and implementation context needed for that fix
- state `Fix ONLY the issue described here.`
- forbid unrelated refactors or opportunistic cleanup
- require the sub-agent to report files changed and any follow-up verification it ran

## Validation attempt loop

1. Reread persisted validation state before taking any resume action.
2. If no validator output exists for the current attempt, write `.tmp-subtask-plan-validation-attempt-<attempt>.md`, emit the validator handoff, persist state, and return `waiting_for_handoffs`.
3. When resumed validator output is present, require exactly one `# output sub-agent 1:` block for the recorded validator handoff.
4. Parse the validator report deterministically.
5. If the report is malformed, return `blocked` with the exact repair step.
6. If `STATUS: PASS`, return `pass`.
7. If `STATUS: FAIL`, extract GAPS and required DEVIATIONS, classify every failed item as actionable now or blocked with an explicit reason, and persist the parsed outcome.
8. If there are no actionable fixes, return `blocked` with the documented blocking reason.
9. If validation-fix handoffs for the current attempt have not been emitted yet, generate focused fix prompts, emit the first validation-fix batch, persist state, and return `waiting_for_handoffs`.
10. When resumed validation-fix outputs are present, require every expected output block for the current batch, process them, update persisted batch progress, and either emit the next validation-fix batch or continue.
11. After all validation-fix batches for the current attempt are processed, require verification of the delegated changes.
12. If validation fixes changed implementation and code review was not skipped, invoke `my:review-execution-output-non-interactive`, require `status: clean`, and persist that re-review outcome before the next validation attempt starts.
13. Increment the persisted validation attempt only after the current attempt's fix work, verification, and required re-review are complete.
14. On attempts 1 through 4 with remaining actionable issues, return `fix_required` with the exact next validation re-entry step.
15. On 5 failed attempts with unresolved actionable issues, persist `cap_summary`, return `proceed_decision_required`, and require the orchestrator to stop with the deterministic unresolved-item summary.
16. If the helper is re-entered after the cap with `post_cap_decision: unset`, return the same `proceed_decision_required` summary again.
17. If `post_cap_decision: proceed` is recorded after the cap, return `abort` because non-interactive execution must stop at that decision boundary and may continue only through a fresh intentional rerun with updated authoritative state.
18. If `post_cap_decision: abort` is recorded after the cap, return `abort` with the same persisted cap summary.
19. Never silently continue validation after the cap inside the same non-interactive run.

## Deterministic stop summary after the cap

When 5 failed attempts have been reached, the helper must persist and reuse a deterministic stop summary containing:

- the exact phrase `5 failed attempts`
- the current validation attempt number
- unresolved GAPS
- unresolved DEVIATIONS
- whether code review was rerun after the last validation-fix pass
- the exact next orchestrator decision required

Once persisted, this summary must be returned unchanged on repeated resumes until new authoritative state or an explicit operator decision is provided.

## Helper result contract

Return a deterministic object or equivalent structured result with:

- `status`: one of `pass`, `fix_required`, `waiting_for_handoffs`, `proceed_decision_required`, `blocked`, `abort`
- `next_step`: exact next orchestrator action
- `notes`: concise carry-forward context, including validation attempt, unresolved GAPS, unresolved DEVIATIONS, batching progress, re-review outcome, and cap-summary context when present
- `state_updates`: authoritative validation-state changes that the orchestrator must persist before another batch, resume point, or phase transition

Status meanings:

- `pass` — validation succeeded and Phase 6 may complete.
- `fix_required` — delegated validation-fix work has completed for the current attempt, required verification and any required re-review are done, and the orchestrator must re-enter validation for the next attempt.
- `waiting_for_handoffs` — the helper emitted validator or validation-fix handoffs and the orchestrator must stop for resumed outputs.
- `proceed_decision_required` — the helper reached the cap or an equivalent terminal decision point and the orchestrator must obtain or honor an explicit proceed-or-stop decision.
- `blocked` — the helper cannot continue deterministically because required inputs, resumed outputs, report structure, or execution context are missing or invalid.
- `abort` — a terminal stop condition was reached and validation must not continue.
