# Handoff Protocol Spec

Transport-only contract for `my:execute-plan-non-interactive` and same-session non-interactive helper handoffs.

## 1. Scope

- Applies only to non-interactive execution batches and helper handoffs.
- Execution root is the worktree root when a worktree exists; otherwise the repository root / current working directory.
- Covers only:
  - prompt-file naming
  - emitted handoff lines
  - output-block mapping
  - `.tmp-execute-plan-state.json` lifecycle
  - continuation parsing
  - allowed `agent-type` metadata
- Concern-specific prompt content, triage rules, reviewer policy, validation policy, and fix-decision logic belong in helper skills or orchestrator contracts, not here.
- Non-interactive orchestrators must not launch Task/Agent sub-agents directly and must not use `AskUserQuestion`.

## 2. Handoff batch rules

- Emit a handoff batch whenever non-interactive execution reaches a point where interactive execution would otherwise launch one or more sub-agents.
- A batch MUST contain at most 5 handoffs.
- If more than 5 handoffs are needed for one execution point, split them into sequential batches.
- Do not evaluate an execution point until all handoffs for its current batch have been resumed and processed.

Common execution points:
- wave execution
- integration tests
- integration fixes
- code review
- review fixes
- plan validation
- validation fixes
- cleanup fixes

## 3. Printed handoff format

Each handoff line MUST use this exact format:

`call sub-agent <N> (agent-type: <type>): <absolute-path>`

Rules:
- `<N>` is a 1-based index within the emitted batch.
- `<type>` is one of `claude`, `codex`, `gemini`.
- `<absolute-path>` is the absolute path to the prompt file.

## 4. Prompt-file rules

- Prompt files MUST contain only sub-agent instructions.
- Prompt files MUST NOT contain transport metadata such as `agent-type`.
- Prompt files MUST use iteration-safe names.

Naming patterns:
- implementation: `.tmp-subtask-wave-<wave>-batch-<batch>-<N>.md`
- integration tests: `.tmp-subtask-integration-attempt-<attempt>-<N>.md`
- integration fixes: `.tmp-subtask-integration-fix-attempt-<attempt>-<N>.md`
- review: `.tmp-subtask-review-attempt-<attempt>-<reviewer>.md`
- review fixes: `.tmp-subtask-review-fix-attempt-<attempt>-<N>.md`
- validation: `.tmp-subtask-plan-validation-attempt-<attempt>.md`
- validation fixes: `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md`
- cleanup fixes: `.tmp-subtask-cleanup-fix-attempt-<attempt>-<N>.md`

## 5. State file

After emitting any handoff batch, write:

`<repo-or-worktree-root>/.tmp-execute-plan-state.json`

This file is the source of truth for the current execution point and the most recently emitted batch.

Minimum schema:

```json
{
  "phase": "wave_execution | integration_tests | integration_fix | code_review | review_fix | plan_validation | validation_fix | cleanup_fix",
  "wave": 1,
  "attempt": 1,
  "batch": 2,
  "attemptState": {
    "requiredReviewers": ["claude", "codex", "gemini"],
    "totalBatches": 3,
    "completedBatches": [1],
    "completedHandoffs": [
      "/abs/path/.tmp-subtask-wave-1-batch-1-1.md",
      "/abs/path/.tmp-subtask-wave-1-batch-1-2.md"
    ]
  },
  "handoffs": [
    {
      "index": 1,
      "agentType": "claude",
      "promptFile": "/abs/path/.tmp-subtask-wave-1-batch-2-1.md"
    },
    {
      "index": 2,
      "agentType": "codex",
      "promptFile": "/abs/path/.tmp-subtask-wave-1-batch-2-2.md"
    }
  ]
}
```

Rules:
- `phase` identifies the current execution point.
- `wave` is required for wave execution and may be omitted or null for non-wave phases.
- `attempt` identifies the current review/validation/integration attempt where relevant.
- `batch` identifies the current emitted batch.
- `attemptState` is required for any execution point that may span multiple batches and must contain enough metadata for deterministic resume.
- `attemptState.requiredReviewers` may carry the reviewer set frozen by the helper for transport/resume purposes.
- `handoffs` MUST preserve the exact order of the printed handoff lines.
- Fully update the state file before stopping for continuation input.

## 6. Continuation payload

Continuation input MUST provide one output block per completed handoff using this exact format:

```text
# output sub-agent <N>:
<pasted output>
```

Rules:
- `<N>` MUST match the `index` field in the current state file.
- Reread `.tmp-execute-plan-state.json` before parsing resumed outputs.
- Map each `# output sub-agent <N>:` block to the Nth expected handoff from the current batch.
- Do not infer a different order.
- Do not merge outputs from different batches into one batch evaluation.

Invalid batch rule:
- If any output block is duplicate or uses an unexpected index, stop and print exactly:
  `Invalid batch: provide exactly one # output sub-agent <N>: block for each handoff listed in .tmp-execute-plan-state.json, with no duplicates and no unexpected indices, then continue again.`
- Do not evaluate or advance after printing that message.

Incomplete batch rule:
- If any expected output block is missing, stop and print exactly:
  `Incomplete batch: provide exactly one # output sub-agent <N>: block for every handoff listed in .tmp-execute-plan-state.json, then continue again.`
- Do not advance while the current batch is incomplete.

## 7. Failed handoffs

- A failed external sub-agent run still returns inside the normal `# output sub-agent <N>:` block.
- Treat it as a completed-but-failed handoff result, not a missing output.
- Phase logic decides whether to retry, emit a corrected batch, or stop.

## 8. State lifecycle

- Create `.tmp-execute-plan-state.json` before the first non-interactive stop that expects continuation input.
- The active batch in the state file MUST always match the most recently emitted handoff lines.
- After a batch is fully processed, either:
  - update the state file for the next batch in the same execution point,
  - update it for the next execution point, or
  - remove/finalize it when non-interactive execution is fully complete.
- Concern-specific review/validation state remains helper-owned metadata, not protocol-owned schema.

## 9. Executor obligations

- The executor MUST rely on the printed handoff lines and `.tmp-execute-plan-state.json` for routing and resume matching.
- The executor MUST NOT infer, reorder, or synthesize handoffs outside this protocol.
- The executor MAY use `agent-type` to select the external runner.
- The executor MUST NOT inspect prompt-file bodies to determine routing.

## 10. Compact examples

Wave batch:

```text
call sub-agent 1 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-1.md
call sub-agent 2 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-2.md
```

Review batch:

```text
call sub-agent 1 (agent-type: claude): /abs/path/.tmp-subtask-review-attempt-2-claude.md
call sub-agent 2 (agent-type: codex): /abs/path/.tmp-subtask-review-attempt-2-codex.md
call sub-agent 3 (agent-type: gemini): /abs/path/.tmp-subtask-review-attempt-2-gemini.md
```
