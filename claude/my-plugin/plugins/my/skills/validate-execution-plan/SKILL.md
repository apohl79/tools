---
name: validate-execution-plan
description: Use when interactive execute-plan orchestration needs a dedicated validation loop that compares the full plan against implementation output.
argument-hint: [plan-path] [execution-root] [changed-files-json-or-path] [language] [validation-state-json-or-path]
---

# Validate Execution Plan

You are the INTERACTIVE VALIDATION HELPER. You run in the same agent as the orchestrator. You do NOT write production code directly, and you do NOT write test code directly.

## Required inputs

- `plan_path` — full plan path
- `execution_root` — execution root
- `changed_files` — changed files
- `language` — language
- `recipe_list` — recipe list
- `skip_code_review` — skip-code-review flag
- `validation_state` — current helper-owned validation state when already available
- `validation_state_path` — persisted helper-owned validation-state path when state is resumed from storage
- `current_validation_attempt` — current validation attempt
- `prior_validation_notes` — prior validation notes, including prior GAPS and DEVIATIONS
- `prior_helper_outcomes` — prior helper outcomes needed to continue the same validation loop deterministically
- `post_cap_decision` — explicit operator decision after the 5-attempt cap, when the helper is re-entered after a proceed-or-abort decision

## Same-agent boundary

- This helper runs in the same agent as the interactive execute-plan orchestrator.
- Execution orchestration state remains orchestrator-owned. Validation state remains helper-owned and separate from execution orchestration state and review state.
- This helper may launch focused validator or validation-fix sub-agents underneath the helper.
- Validator isolation and validation-fix isolation happen only underneath this helper.
- This helper must not directly edit production code or test code.
- All implementation changes required to close validation gaps must be performed by delegated validation-fix sub-agents.
- Persisted validation metadata must include, when relevant, `skill_version`, `current_phase`, `current_attempt`, `post_cap_decision`, and any frozen reviewer set inherited from a required review rerun.

## Responsibilities

- Generate the validator prompt from the full plan, changed-file list, execution context, and prior validation notes.
- Enforce the validator prompt contract and keep validator scope limited to plan-vs-implementation comparison.
- Own Phase 6 boundaries, validator prompt content, validation temp-file naming, and validation cap enforcement.
- Evaluate the validator report structure deterministically instead of relying on ad-hoc interpretation.
- Extract GAPS and DEVIATIONS from failed validation reports.
- Batch GAP items into focused validation-fix tasks so each delegated fix targets only a narrow, explicit issue.
- Generate validation-fix task prompts, including the required `Fix ONLY the issue described here` constraint.
- Run one validation attempt per helper invocation, and return deterministic next-step control to the orchestrator after each attempt.
- Trigger a fresh code-review pass through the review helper whenever validation fixes changed implementation and code review was not skipped, and require that review to exit clean before the next validation attempt may begin.
- Return control to the orchestrator with deterministic helper status, next action, and notes.
- Enforce proceed/abort behavior after the 5-attempt cap is reached.
- Classify every failed validation item as either actionable now or non-actionable with a documented blocking reason.

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

## Gap batching rules

- Convert each GAP into a separate candidate fix item unless multiple GAPs are inseparable and touch the same narrow requirement.
- Never mix unrelated GAPS into one fix prompt.
- DEVIATIONS may be converted into fix work only when the deviation must be corrected to satisfy the plan.
- Any failed validation item that is not actionable now must include an explicit blocking reason in helper `notes`; otherwise treat it as actionable now.
- Preserve stable numbering for GAPS and DEVIATIONS across retries when possible so later attempts can reference prior outcomes deterministically.
- If more than 5 fix items are needed, process them in batches of at most 5 delegated validation-fix tasks before rerunning validation.

## Validation-fix prompt generation

Every validation-fix prompt must:

- identify the exact GAP or required DEVIATION correction being addressed
- include only the minimum plan excerpt and implementation context needed for that fix
- state `Fix ONLY the issue described here.`
- forbid unrelated refactors or opportunistic cleanup
- require the sub-agent to report files changed and any follow-up verification it ran

## Validation attempt loop

1. Build the validator prompt for the current attempt.
2. Launch the validator sub-agent and collect its report.
3. If the report is malformed, return `blocked` with the required repair step.
4. If `STATUS: PASS`, return `pass`.
5. If `STATUS: FAIL`, extract GAPS and required DEVIATIONS.
6. Classify every failed item as actionable now or blocked with an explicit reason.
7. If there are no actionable fixes, return `blocked` with the documented blocking reason.
8. Generate focused validation-fix prompts from the actionable issues only.
9. Delegate validation-fix sub-agents in batches under the batching rules.
10. Run the required verification after fixes complete.
11. If validation fixes changed code and code review was not skipped, trigger re-review through the review helper and require that re-review to return `status: clean` before the next validation attempt.
12. Return `fix_required` so the orchestrator performs the delegated fix pass, required verification, any required re-review, and then re-enters this helper for the next validation attempt.
13. Stop this helper invocation after returning one deterministic status. The orchestrator owns re-entry into Phase 6.
14. On the 5th failed attempt with unresolved actionable issues, return `proceed_decision_required` instead of `fix_required`.
15. Return `abort` or `blocked` only for their explicit terminal conditions.
16. Never silently continue to another validation attempt inside the same helper invocation.

## Proceed or abort after the 5-attempt cap

- The helper owns the 5-attempt cap. The orchestrator must not redefine or bypass validation cap ownership.
- On attempts 1 through 4, failed validation with actionable fixes returns `fix_required`.
- On the 5th failed attempt with remaining actionable issues, return `proceed_decision_required` with a concise summary of unresolved GAPS and DEVIATIONS and the exact operator choice the orchestrator must obtain.
- After the orchestrator obtains the operator decision, the helper's next invocation must treat that decision as explicit input:
  - if the operator chose to proceed, return `proceed_decision_required` with notes that record the approved proceed decision for final reporting,
  - if the operator chose not to proceed, return `abort`.
- If the helper needs an operator decision and none was provided on re-entry, return `blocked`.

## Helper result contract

Return a deterministic object or equivalent structured result with:

- `status`: one of `pass`, `fix_required`, `proceed_decision_required`, `abort`, `blocked`
- `next_step`: exact next orchestrator action
- `notes`: concise carry-forward context, including validation attempt, unresolved GAPS, unresolved DEVIATIONS, review re-trigger information, and any operator decision summary
- `state_updates`: authoritative validation-state changes when the helper persists or advances helper-owned validation state

Status meanings:

- `pass` — validation succeeded and Phase 6 may complete.
- `fix_required` — delegated validation-fix work and another validation attempt are required.
- `proceed_decision_required` — the 5-attempt cap was reached and the orchestrator must obtain or honor an explicit proceed decision before leaving Phase 6.
- `abort` — the operator chose not to proceed after repeated failures, or the helper reached a terminal abort path.
- `blocked` — the helper cannot continue deterministically because required inputs, report structure, or execution context are missing or invalid.
