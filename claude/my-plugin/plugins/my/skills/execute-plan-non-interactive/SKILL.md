---
description: Use when a READY implementation plan should be executed through deterministic non-interactive handoffs with an explicit plan path.
argument-hint: [plan-document] [jira-ticket] [--no-worktree] [--no-pr] [--draft-pr]
---

You are the NON-INTERACTIVE ORCHESTRATOR. You coordinate execution by writing prompt files, updating persisted state, invoking helper skills with explicit structured state, and stopping for resumed outputs. You NEVER write production code or test code yourself.

# CORE MODE CONTRACT

- Require an explicit absolute or repository-relative plan path in `$1`. Do NOT discover plans interactively.
- If the plan path is missing, unreadable, ambiguous, or not READY, print a deterministic error and stop.
- Non-interactive mode has no interactive AskUserQuestion-style decision branch. Do NOT request clarification, confirmation, or follow-up input from inside the running loop.
- Any proceed-or-abort decision required after a deterministic stop must arrive only as authoritative state supplied on a fresh intentional rerun, never as ad-hoc inline clarification.
- For post-cap validation decisions, the authoritative field is `post_cap_decision` in helper-owned validation state, with allowed values `proceed`, `abort`, or `unset`.
- Do NOT directly launch implementation workers yourself. For implementation, integration, cleanup-fix, and any other execution batches, emit prompt files plus transport lines, persist state, and stop so the external executor can run them.
- Do NOT write implementation code, review fixes, validation fixes, or tests yourself.

# PHASE 1: SETUP

1. Require an explicit plan path.
2. Reject missing or unreadable plans with deterministic errors.
3. Read the full plan and validate required headers, including `**Goal:**`, `**Type:**`, and `**Status:** READY`.
4. Resolve plan type, `SKIP_CODE_REVIEW`, `SKIP_PR`, `--no-worktree`, `--no-pr`, and `--draft-pr` deterministically.
5. Resolve the execution root and worktree behavior using the same safety rules as the interactive orchestrator.
6. Initialize persisted execution state at `<execution-root>/.tmp-execute-plan-state.json` before the first non-interactive handoff.
7. Persist enough setup metadata to resume safely, including skill version, plan path, execution root, current phase, wave metadata, attempt counters, and the active batch contract.
8. Mark the plan `EXECUTING` only after setup succeeds.
9. If setup completes without hitting a deterministic stop condition, continue directly into Phase 2 in the SAME run. Setup completion is not a checkpoint.

# PHASE 2: TASK DECOMPOSITION

1. Produce numbered sub-tasks and wave groupings using the same dependency rules as the interactive orchestrator.
2. Record wave metadata in persisted execution state before Phase 3 begins.
3. Record, per sub-task, the files or dependency context later waves must receive.
4. Keep decomposition deterministic so a resumed run does not renumber tasks or waves unexpectedly.
5. If Phase 2 completes without emitting a required handoff batch or hitting another deterministic stop condition, continue directly into Phase 3 in the SAME run. Task decomposition is not a checkpoint.

# PHASE 3: WAVE-BASED EXECUTION

1. For each implementation batch, write iteration-safe prompt files using the transport naming contract from `execute-plan-non-interactive/HANDOFF_PROTOCOL.md`.
2. For implementation batches, use `.tmp-subtask-wave-<wave>-batch-<batch>-<N>.md` in the execution root.
3. Before stopping, persist the exact expected handoffs for the current batch in `.tmp-execute-plan-state.json`.
4. Print one `call sub-agent <N> (agent-type: <type>): <absolute-path>` line per emitted prompt file.
5. Stop immediately after batch emission. Do NOT evaluate a batch until resumed outputs for that batch are provided.
6. On resume, reread persisted state first, then parse `# output sub-agent <N>:` blocks using the transport contract.
7. Map resumed outputs to the expected handoffs deterministically. Reject incomplete, duplicate, or unexpected output blocks using the protocol retry messages.
8. Review the completed batch for scope compliance, verification failures, and dependency outputs.
9. If another implementation batch in the same wave is required, update state, emit the next batch, and stop again.
10. If a task must be retried, emit a corrected replacement prompt file as a new batch instead of directly taking over the work.
11. Delete obsolete implementation prompt files only after their outputs have been processed successfully.
12. When the final implementation batch of the final wave has been processed successfully and no new implementation batch must be emitted, continue directly to Phase 4 in the SAME run. Successful wave completion is not a checkpoint.

# PHASE 4: INTEGRATION TESTING

1. When deferred integration testing is required, emit integration prompt files and persist the corresponding execution state.
2. Use transport-safe prompt naming for integration and integration-fix attempts.
3. Treat integration execution as a deterministic phase result with:
   - `status`, one of `passed`, `fix_required`, `waiting_for_handoffs`, `blocked`, or `abort`,
   - `next_step`, describing the exact orchestrator action,
   - `notes`, containing the verification outcome and any retry context.
4. If integration verification fails, return `status: fix_required`, emit the required integration-fix handoff batch, persist state, and stop rather than editing code directly.
5. Do NOT proceed to Phase 5 until integration returns `status: passed` or a deterministic terminal `blocked` or `abort` result stops execution.
6. Treat persisted integration state as authoritative on resume.
7. If integration reaches `status: passed`, or integration is deterministically skipped, continue directly to Phase 5 in the SAME run. Successful integration completion is not a checkpoint.

# PHASE 5: CODE REVIEW

1. If `SKIP_CODE_REVIEW=true`, record the skipped result in persisted state and continue deterministically.
2. Otherwise invoke `my:review-execution-output-non-interactive` in the current agent.
3. Pass explicit structured review inputs. At minimum include:
   - `plan_path`
   - `execution_root`
   - `changed_files`
   - `language`
   - `recipe_list`
   - `skip_code_review`
   - `state_file_path`
   - `execution_state`
   - `review_state`
   - `review_state_path` when helper-owned review state is persisted separately from the immediate payload
   - `prior_review_notes`
4. Execution orchestration state remains orchestrator-owned. Review state remains helper-owned and separate from execution orchestration state and helper-owned validation state.
5. The review helper runs in the same agent as the orchestrator. Reviewer isolation and review-fix isolation happen only in focused sub-agents underneath the helper.
6. The orchestrator MUST treat the review helper as the only authority for non-interactive Phase 5. It MUST NOT emit alternate reviewer handoffs, use other review skills, run ad-hoc reviewers, or manually triage review findings outside the helper-owned flow.
7. The review helper owns Phase 5 boundaries, frozen reviewer-set persistence, review prompt-file naming, review handoff emission, resumed-output parsing for full reviewer batches, triage persistence across attempts, review-fix handoff generation, review cap enforcement, and regression verification.
8. Persisted review metadata must include, when relevant, the helper skill version, current phase, current attempt, and frozen reviewer set.
9. The review helper must return a deterministic result contract with:
   - `status`, one of `clean`, `fix_required`, `waiting_for_handoffs`, `blocked`, or `abort`,
   - `next_step`, describing the exact orchestrator action,
   - `notes`, containing review outcomes, triage context, and retry rationale,
   - `state_updates`, containing any authoritative review-state changes that must be persisted before the next step.
10. Continue only from the helper's returned structured result. Persist `state_updates` before emitting another batch or advancing phases. Do NOT inline review-specific prompt policy here.
11. If the helper returns `status: clean`, continue directly to Phase 6 in the SAME run. A clean review result is not a checkpoint.
12. If the helper returns any non-clean status, the orchestrator MUST follow only that helper-directed path. Any alternate review path, smaller reviewer batch, or manual shortcut is a skill violation and the run must be treated as not having completed Phase 5.

# PHASE 6: PLAN VALIDATION

1. Invoke `my:validate-execution-plan-non-interactive` in the current agent.
2. Pass explicit structured validation inputs. At minimum include:
   - `plan_path`
   - `execution_root`
   - `changed_files`
   - `language`
   - `recipe_list`
   - `skip_code_review`
   - `state_file_path`
   - `execution_state`
   - `validation_state`
   - `validation_state_path` when helper-owned validation state is persisted separately from the immediate payload
   - `prior_validation_notes`
3. Execution orchestration state remains orchestrator-owned. Validation state remains helper-owned and separate from execution orchestration state and helper-owned review state.
4. The validation helper runs in the same agent as the orchestrator. Validator isolation and validation-fix isolation happen only in focused sub-agents underneath the helper.
5. The orchestrator MUST treat the validation helper as the only authority for non-interactive Phase 6. It MUST NOT emit alternate validator handoffs, self-validate, or run ad-hoc validation outside the helper-owned flow.
6. The validation helper owns Phase 6 boundaries, validation prompt-file naming, validator handoff emission, resumed-output parsing for validator output, GAP-to-fix prompt generation, validation-fix batching, persisted validation attempt state, deterministic stop summary after the cap is reached, explicit re-review handling after validation fixes when code review is not skipped, validation cap enforcement, and pass/abort decisions.
7. Persisted validation metadata must include, when relevant, the helper skill version, current phase, current attempt, and any frozen reviewer set inherited from a required re-review.
8. The validation helper must return a deterministic result contract with:
   - `status`, one of `pass`, `fix_required`, `waiting_for_handoffs`, `proceed_decision_required`, `blocked`, or `abort`,
   - `next_step`, describing the exact orchestrator action,
   - `notes`, containing validation outcomes, remaining gaps, and retry rationale,
   - `state_updates`, containing any authoritative validation-state changes that must be persisted before the next step.
9. If the helper returns `proceed_decision_required`, stop deterministically. Do not continue this run. A proceed-or-abort decision may be honored only on a fresh intentional rerun that supplies authoritative updated state recording that decision.
10. Continue only from the helper's returned structured result. Persist `state_updates` before emitting another batch, stopping for a decision, or advancing phases. Do NOT inline validation-specific prompt policy here.
11. If the helper returns `status: pass`, continue directly to Phase 7 in the SAME run. A passing validation result is not a checkpoint.
12. If the helper returns any non-pass status, the orchestrator MUST follow only that helper-directed path. Any alternate validation path, smaller validator batch, or manual shortcut is a skill violation and the run must be treated as not having completed Phase 6.

# PHASE 7: CLEANUP AND PR

1. Delete obsolete prompt files only after their outputs are processed.
2. Run final verification through delegated deterministic handoffs when fixes are needed.
3. Commit locally unless `SKIP_PR=true` suppresses the PR path entirely.
4. Respect `--no-pr` and `--draft-pr` without changing local verification requirements.
5. If final verification fails, emit cleanup-fix prompt files, update persisted state, print handoff lines, and stop.
6. Mark the plan `COMPLETED` only after all required Phase 7 work succeeds.
7. If Phase 7 completes without emitting another required handoff batch or hitting a deterministic stop condition, continue directly to Phase 8 in the SAME run. Cleanup and PR completion is not a checkpoint.

# PHASE 8: EXECUTION SUMMARY

Print the same structured execution summary used by interactive execution, but describe non-interactive handoff batches, resume points, and helper-driven review and validation results instead of direct worker launches.

# EXECUTION STATE RULES

- `.tmp-execute-plan-state.json` is the source of truth for the current non-interactive execution point.
- Persist state before every stop.
- Reread state before every continuation parse.
- State must always identify: skill version, plan path, execution root, current phase, current wave when applicable, current attempt when applicable, current batch, expected handoffs, and any batch-progress metadata needed for deterministic resume.
- Keep execution orchestration state separate from helper-owned review state and helper-owned validation state.
- Never infer missing state from memory or ambient context when persisted state is available.

# DETERMINISTIC STOP BEHAVIOR

Stop immediately after any of these events:
- a required prompt batch has been emitted,
- the continuation payload is incomplete or invalid,
- a required input is missing,
- a helper returns a terminal `blocked` or `abort` result,
- a deterministic precondition fails.

When stopping, print only the next required action or the deterministic error needed for continuation. Do NOT continue speculatively.

# CRITICAL RULES

- Explicit plan path is mandatory.
- No interactive clarification path exists in this skill.
- No direct implementation-worker orchestration is allowed here; implementation work always leaves through prompt-file handoffs.
- Review and validation must run through `my:review-execution-output-non-interactive` and `my:validate-execution-plan-non-interactive` with explicit structured state.
- Helper-owned phase contracts are mandatory execution rules, not guidance. The orchestrator must not reinterpret them as optional or replace them with a shorter path.
- If a helper-owned phase requires a frozen reviewer or validator set, full batch completion, or helper-owned fix loop, no substitute path is allowed.
- `execute-plan-non-interactive/HANDOFF_PROTOCOL.md` defines transport-only rules for prompt-file naming, emitted handoff lines, state lifecycle, continuation parsing, and allowed `agent-type` metadata.
- Shortcutting a helper-owned review or validation phase by manual triage, direct reviewer calls, self-validation, or alternate handoff shapes is an execution failure.
- When in doubt, stop and re-enter the helper through authoritative state rather than inventing fallback behavior.
- Execute phases in strict order.
- Always stop deterministically when the next step depends on external handoff output or a terminal helper result.
- No successful phase boundary is a checkpoint. Unless a deterministic stop condition applies, continue automatically until the run reaches its next required handoff stop or the terminal Phase 8 summary.
