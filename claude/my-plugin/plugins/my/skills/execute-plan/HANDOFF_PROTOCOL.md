# Handoff Protocol Spec

This document defines the normative contract for non-interactive `/my:execute-plan` orchestration.

## 1. Scope

- This protocol applies only when `/my:execute-plan` is invoked with `--non-interactive`.
- For file locations, the execution root is the worktree root when a worktree exists; otherwise it is the repository root / current working directory.
- In normal mode, interactive behavior remains unchanged.
- In non-interactive mode, the orchestrator MUST NOT launch Task/Agent sub-agents directly.
- In non-interactive mode, the orchestrator MUST NOT use `AskUserQuestion`.
- If required inputs are missing or ambiguous in non-interactive mode, the orchestrator MUST print a deterministic error and stop.

## 2. When a handoff batch is emitted

- The orchestrator MUST emit a handoff batch whenever non-interactive mode reaches a point where interactive mode would otherwise launch one or more sub-agents.
- A handoff batch MUST contain at most 5 handoffs.
- If more than 5 handoffs are required for the same execution point, the orchestrator MUST split them into sequential batches.
- The orchestrator MUST NOT evaluate that execution point until all required handoffs for that execution point have been completed and resumed.

Examples of execution points include:
- implementation wave batches
- integration-test dispatch
- integration-fix dispatch
- code-review dispatch
- review-fix dispatch
- validation dispatch
- validation-fix dispatch
- cleanup-fix dispatch

## 3. Printed handoff line format

Each handoff in the emitted batch MUST be printed on its own line using this exact format:

`call sub-agent <N> (agent-type: <type>): <absolute-path>`

Where:
- `<N>` MUST be a 1-based index within the emitted batch
- `<type>` MUST be one of:
  - `claude`
  - `codex`
  - `gemini`
- `<absolute-path>` MUST be the absolute path to the prompt file for that handoff

Example:

```text
call sub-agent 1 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-1.md
call sub-agent 2 (agent-type: codex): /abs/path/.tmp-subtask-review-attempt-1-codex.md
```

## 4. Meaning of `agent-type`

- `agent-type` is transport metadata for the executor.
- The executor MUST use `agent-type` to choose the external runner for the handoff.
- The executor MUST NOT need to inspect the prompt file to determine routing.
- Prompt files MUST NOT embed `agent-type` or other transport metadata in the prompt body.

## 5. Prompt files

- Prompt files MUST contain only the instructions for the sub-agent.
- Prompt files MUST NOT contain transport metadata such as `agent-type`.
- Prompt files MUST use iteration-safe names so repeated waves, attempts, and batches are unambiguous.

Recommended naming patterns:
- implementation: `.tmp-subtask-wave-<wave>-batch-<batch>-<N>.md`
- integration tests: `.tmp-subtask-integration-attempt-<attempt>-<N>.md`
- integration fix: `.tmp-subtask-integration-fix-attempt-<attempt>-<N>.md`
- review: `.tmp-subtask-review-attempt-<attempt>-<reviewer>.md`
- review fix: `.tmp-subtask-review-fix-attempt-<attempt>-<N>.md`
- validation: `.tmp-subtask-plan-validation-attempt-<attempt>.md`
- validation fix: `.tmp-subtask-validation-fix-attempt-<attempt>-<N>.md`
- cleanup fix: `.tmp-subtask-cleanup-fix-attempt-<attempt>-<N>.md`

For cleanup-fix execution points, `<attempt>` is the Phase 7 `cleanup_attempt` counter starting at 1 and incrementing after each cleanup-fix pass before the pipeline is re-run.

## 6. State file

After emitting any handoff batch, the orchestrator MUST write a state file named:

`<repo-or-worktree-root>/.tmp-execute-plan-state.json`

This file MUST be the source of truth for the current execution point, including the most recently emitted batch and any batch-progress metadata needed to resume that execution point safely.

The state file MUST contain at minimum:

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
- `phase` MUST identify the current execution point
- `wave` MUST be present for wave execution and MAY be omitted or null for non-wave phases
- `attempt` MUST identify the current review/validation/integration attempt where relevant
- `batch` MUST identify the current emitted batch within that execution point
- `attemptState` MUST be present when an execution point can span multiple batches for one attempt, and MUST record enough information to resume that attempt deterministically
- For code-review attempts, `attemptState.requiredReviewers` MUST list every reviewer in the set frozen at Phase 5 start for the current code-review loop, and that same reviewer set MUST be reused for every attempt in that loop
- For any multi-batch attempt, `attemptState.totalBatches` MUST record how many batches are required for that attempt
- For any multi-batch attempt, `attemptState.completedBatches` MUST record which earlier batches in that same attempt have already been fully collected and processed
- For any multi-batch attempt, `attemptState.completedHandoffs` MUST record enough stable metadata to know exactly which earlier handoffs in that same attempt have already been collected and processed
- Multi-batch wave execution MUST use the same `attemptState` discipline as multi-batch review attempts; batch-progress metadata is required for wave execution, review-fix, validation-fix, cleanup-fix, or any other execution point that is split across batches
- `handoffs` MUST preserve the exact order of the printed handoff lines

The orchestrator MUST fully update this file before stopping to wait for continuation input.

## 7. Continuation payload format

When execution resumes via `claude -p --continue`, the continuation payload MUST provide one output block per completed handoff using this exact format:

```text
# output sub-agent <N>:
<pasted output>
```

Example:

```text
# output sub-agent 1:
[full output of the first sub-agent run]

# output sub-agent 2:
[full output of the second sub-agent run]
```

Rules:
- `<N>` MUST correspond to the `index` field in the current state file
- The orchestrator MUST reread `.tmp-execute-plan-state.json` before parsing any resumed outputs
- The orchestrator MUST use the current recorded `phase`, `attempt`, `batch`, and any `attemptState` metadata to determine what still remains in the current execution point
- The orchestrator MUST map each `# output sub-agent <N>:` block to the Nth expected handoff from the current recorded batch
- The orchestrator MUST NOT infer a different order
- The orchestrator MUST NOT merge outputs from different batches into one batch evaluation
- If the continuation payload contains duplicate `# output sub-agent <N>:` blocks for the same expected handoff, the batch MUST be rejected.
- If the continuation payload contains any `# output sub-agent <N>:` block whose `<N>` is not present in the current state's `handoffs`, the batch MUST be rejected.
- For either duplicate or unexpected output blocks, the orchestrator MUST stop and print this exact retry instruction: `Invalid batch: provide exactly one # output sub-agent <N>: block for each handoff listed in .tmp-execute-plan-state.json, with no duplicates and no unexpected indices, then continue again.`
- After printing that retry instruction, the orchestrator MUST NOT evaluate the batch, mutate batch progress, or advance to another batch or phase.

## 8. Missing outputs and incomplete batches

- If any expected output block is missing for the current batch, the batch MUST be treated as incomplete.
- For an incomplete batch, the orchestrator MUST stop and print this deterministic retry instruction: `Incomplete batch: provide exactly one # output sub-agent <N>: block for every handoff listed in .tmp-execute-plan-state.json, then continue again.`
- The orchestrator MUST NOT guess which output belongs to which handoff.
- The orchestrator MUST NOT advance to the next batch or phase while the current batch is incomplete.

## 9. Failed handoffs

- A failed external sub-agent run MUST still be returned inside the normal `# output sub-agent <N>:` block.
- The orchestrator MUST treat that as a completed-but-failed handoff result.
- A failed handoff result is NOT the same as a missing output block.
- The orchestrator MUST evaluate failed handoff output according to the current phase logic:
  - retry the same execution point if required
  - emit a corrected follow-up batch if needed
  - stop with a clear error if continuation is not possible

## 10. Reviewer selection in non-interactive mode

- Non-interactive mode MUST mirror the same reviewer-selection logic used by the interactive code-review phase.
- Reviewer selection for a code-review loop MUST be determined once at Phase 5 start from tool availability using the same rule in both modes:
  - Claude recipe reviewer is always required unless code review is skipped.
  - Codex review is included only when the exact tool `mcp__codex__codex` is available.
  - Gemini review is included only when the exact tool `mcp__gemini-cli__ask-gemini` is available.
- If the interactive logic would include the Claude recipe reviewer, non-interactive mode MUST emit the Claude reviewer handoff.
- If the interactive logic would include Codex review, non-interactive mode MUST emit the Codex reviewer handoff.
- If the interactive logic would include Gemini review, non-interactive mode MUST emit the Gemini reviewer handoff.
- Non-interactive mode MUST NOT invent a different reviewer-selection rule.
- The orchestrator MUST record the full required reviewer set once before emitting the first review batch of the loop, and interactive and non-interactive mode MUST use that same frozen reviewer set for every attempt in that same code-review loop.

## 11. Reviewer batching under the 5-handoff cap

- If a review attempt requires more than 5 handoffs, the orchestrator MUST split them into sequential batches for that same review attempt.
- The state file for that attempt MUST record the full frozen reviewer set for the loop and completed-batch progress for the attempt.
- If Claude, Codex, and Gemini reviewers are all required, the orchestrator SHOULD prioritize them in the earliest batch for that attempt.
- The orchestrator MUST NOT evaluate the review round until outputs from all reviewers in the frozen reviewer set have been collected across all batches for that attempt.

## 12. Executor obligations

- The executor MUST rely on the printed handoff lines and `.tmp-execute-plan-state.json` for routing and resume matching.
- The executor MUST NOT infer, reorder, or synthesize handoffs outside this protocol.
- The executor MAY use `agent-type` to choose the external runner.
- The executor MUST NOT require prompt-file introspection to determine routing.
- The executor SHOULD preserve the full sub-agent output so it can be pasted back into the continuation payload unchanged.

## 13. Worked examples

### Wave execution batch

```text
call sub-agent 1 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-1.md
call sub-agent 2 (agent-type: claude): /abs/path/.tmp-subtask-wave-1-batch-1-2.md
```

### Wave execution state file

```json
{
  "phase": "wave_execution",
  "wave": 1,
  "attempt": 1,
  "batch": 1,
  "attemptState": {
    "totalBatches": 1,
    "completedBatches": [],
    "completedHandoffs": []
  },
  "handoffs": [
    {
      "index": 1,
      "agentType": "claude",
      "promptFile": "/abs/path/.tmp-subtask-wave-1-batch-1-1.md"
    },
    {
      "index": 2,
      "agentType": "claude",
      "promptFile": "/abs/path/.tmp-subtask-wave-1-batch-1-2.md"
    }
  ]
}
```

### Split wave execution example

If Wave 1 needs 6 handoffs, emit two sequential batches. After collecting batch 1, update the state before batch 2:

```json
{
  "phase": "wave_execution",
  "wave": 1,
  "attempt": 1,
  "batch": 2,
  "attemptState": {
    "totalBatches": 2,
    "completedBatches": [1],
    "completedHandoffs": [
      "/abs/path/.tmp-subtask-wave-1-batch-1-1.md",
      "/abs/path/.tmp-subtask-wave-1-batch-1-2.md",
      "/abs/path/.tmp-subtask-wave-1-batch-1-3.md",
      "/abs/path/.tmp-subtask-wave-1-batch-1-4.md",
      "/abs/path/.tmp-subtask-wave-1-batch-1-5.md"
    ]
  },
  "handoffs": [
    {
      "index": 1,
      "agentType": "claude",
      "promptFile": "/abs/path/.tmp-subtask-wave-1-batch-2-1.md"
    }
  ]
}
```

### Wave execution continuation payload for batch 2

```text
# output sub-agent 1:
[output from executing /abs/path/.tmp-subtask-wave-1-batch-2-1.md]
```

The orchestrator MUST:
1. reread `.tmp-execute-plan-state.json`
2. map output 1 to handoff index 1
3. evaluate the batch
4. update the state file before emitting the next batch or moving to the next phase

### Non-wave review execution-point batch example

```text
call sub-agent 1 (agent-type: claude): /abs/path/.tmp-subtask-review-attempt-2-claude.md
call sub-agent 2 (agent-type: codex): /abs/path/.tmp-subtask-review-attempt-2-codex.md
call sub-agent 3 (agent-type: gemini): /abs/path/.tmp-subtask-review-attempt-2-gemini.md
```

### Non-wave review state file example

```json
{
  "phase": "code_review",
  "wave": null,
  "attempt": 2,
  "batch": 1,
  "attemptState": {
    "requiredReviewers": ["claude", "codex", "gemini"],
    "completedBatches": []
  },
  "handoffs": [
    {
      "index": 1,
      "agentType": "claude",
      "promptFile": "/abs/path/.tmp-subtask-review-attempt-2-claude.md"
    },
    {
      "index": 2,
      "agentType": "codex",
      "promptFile": "/abs/path/.tmp-subtask-review-attempt-2-codex.md"
    },
    {
      "index": 3,
      "agentType": "gemini",
      "promptFile": "/abs/path/.tmp-subtask-review-attempt-2-gemini.md"
    }
  ]
}
```

This non-wave example shows that `wave` may be `null` while `attempt`, `batch`, and `attemptState` still fully define the current execution point.

### Validation state file example

```json
{
  "phase": "plan_validation",
  "wave": null,
  "attempt": 1,
  "batch": 1,
  "handoffs": [
    {
      "index": 1,
      "agentType": "claude",
      "promptFile": "/abs/path/.tmp-subtask-plan-validation-attempt-1.md"
    }
  ]
}
```
