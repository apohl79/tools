# Execute Plan Skill Split Design

**Goal:** Split the current `my:execute-plan` skill into smaller skills with strict responsibility boundaries, separating interactive and non-interactive execution and extracting review and validation into dedicated helper skills.

## Problem Statement

The current `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md` mixes too many responsibilities in one document: plan resolution, setup, worktree handling, wave execution, integration testing, code review, validation, PR/finalization, cleanup, summary, and a second non-interactive execution model. This creates excessive mutable state, repeated prompt-generation rules, dual-mode branches across phases, and a large edit blast radius.

The main design goal is to remove mixed-mode and mixed-phase orchestration from a single skill while preserving the current execution behavior and review/validation guarantees.

## Target Skill Split

### Interactive skills

- `my:execute-plan`
- `my:review-execution-output`
- `my:validate-execution-plan`

### Non-interactive skills

- `my:execute-plan-non-interactive`
- `my:review-execution-output-non-interactive`
- `my:validate-execution-plan-non-interactive`

This is a symmetric 6-skill split.

## Responsibility Boundaries

### `my:execute-plan`

Interactive orchestrator only.

Owns:
- plan resolution
- READY validation
- setup and worktree handling
- task decomposition
- wave execution
- integration testing
- cleanup
- final verification
- commit / PR / finalization / summary

Does not contain non-interactive branches.

Invokes helper skills in the same agent:
- `my:review-execution-output`
- `my:validate-execution-plan`

Those helper skills then launch focused reviewer or validator sub-agents with narrow prompts and context.

### `my:execute-plan-non-interactive`

Non-interactive orchestrator only.

Owns:
- explicit plan-path resolution
- execution-root setup
- state-file lifecycle for orchestration phases
- task decomposition
- wave handoff batching
- integration-test handoffs
- cleanup/finalization handoffs
- deterministic stop behavior

Does not call `AskUserQuestion` and does not directly orchestrate `Agent`-based implementation work.

Invokes helper skills in the same agent:
- `my:review-execution-output-non-interactive`
- `my:validate-execution-plan-non-interactive`

### `my:review-execution-output`

Interactive review-loop helper only.

Owns:
- reviewer-set freeze
- review prompt construction
- triage into `VERIFIED_FIX`, `REJECTED`, and `DEFERRED`
- review-fix prompt generation
- review loop and cap
- regression verification after review fixes

Runs in the same agent as the orchestrator, then launches focused reviewers underneath it.

### `my:review-execution-output-non-interactive`

Non-interactive review-loop helper only.

Owns the same review logic as the interactive review helper, but implemented through prompt files, handoff emission, resumed-output parsing, and persisted review state.

### `my:validate-execution-plan`

Interactive validation-loop helper only.

Owns:
- validator prompt construction
- validation report evaluation
- gap extraction
- validation-fix prompt generation
- validation loop and cap
- abort / proceed handoff behavior

Runs in the same agent as the orchestrator, then launches a focused validator and validation-fix sub-agents underneath it.

### `my:validate-execution-plan-non-interactive`

Non-interactive validation-loop helper only.

Owns the same validation logic as the interactive validation helper, but implemented through prompt files, handoff emission, resumed-output parsing, persisted validation state, and deterministic stop behavior.

## Why helper skills instead of more sections in the orchestrator

A helper skill creates a real architectural boundary, not just an organizational one.

Benefits:
- removes mixed responsibilities from the main execution skill
- gives review and validation explicit interfaces instead of ambient shared state
- stops cross-phase policy leakage
- allows interactive and non-interactive review/validation logic to diverge cleanly
- reduces edit blast radius
- makes review and validation independently testable
- keeps reviewer and validator prompts small and consistent

The helper skill itself runs in the same agent as the orchestrator. Isolation happens below that layer through focused reviewer and validator sub-agents, not by adding another orchestration agent.

## Execution Flow

### Interactive flow

`my:execute-plan` owns:
1. resolve READY plan
2. run setup and worktree sync
3. mark plan `EXECUTING`
4. decompose into wave tasks
5. execute implementation waves
6. run integration testing if needed
7. invoke `my:review-execution-output`
8. invoke `my:validate-execution-plan`
9. cleanup, final verification, commit, PR/finalize, summary

It passes structured inputs to helper skills rather than expecting them to reconstruct state from the full orchestrator narrative.

### Non-interactive flow

`my:execute-plan-non-interactive` owns:
1. require explicit plan path
2. setup execution root and persisted execution state
3. decompose work
4. emit wave handoff batches
5. emit integration-test handoffs if needed
6. invoke `my:review-execution-output-non-interactive`
7. invoke `my:validate-execution-plan-non-interactive`
8. emit cleanup/finalization handoffs or deterministic stop output

It owns orchestration-phase handoff state, while review and validation helpers own prompt generation and resume parsing for their own concern.

## Helper Input Contracts

Review and validation helpers should accept explicit structured inputs instead of re-deriving state from the full orchestrator context.

Required inputs:
- plan path
- execution root
- changed-files list
- language
- recipe list
- `SKIP_CODE_REVIEW`
- `SKIP_PR`
- current attempt counters
- prior review decisions or prior validation gaps
- non-interactive state-file path when applicable

This contract makes helper behavior deterministic and easier to reason about.

## State Isolation

Execution state, review state, and validation state should be isolated.

Acceptable designs:
- namespaced sections inside `.tmp-execute-plan-state.json`, or
- separate temp state files for execution, review, and validation

Persisted metadata should include at helper start:
- skill version
- frozen reviewer set
- review cap
- validation cap
- current phase
- current attempt counter
- triage decisions so far

These values must be persisted once and then reused instead of being recomputed mid-run.

## File Layout

Keep:
- `claude/my-plugin/plugins/my/skills/execute-plan/SKILL.md`

Add:
- `claude/my-plugin/plugins/my/skills/execute-plan-non-interactive/SKILL.md`
- `claude/my-plugin/plugins/my/skills/review-execution-output/SKILL.md`
- `claude/my-plugin/plugins/my/skills/review-execution-output-non-interactive/SKILL.md`
- `claude/my-plugin/plugins/my/skills/validate-execution-plan/SKILL.md`
- `claude/my-plugin/plugins/my/skills/validate-execution-plan-non-interactive/SKILL.md`

## Migration Plan

1. Shrink `my:execute-plan` to interactive orchestration only.
2. Move non-interactive execution behavior into `my:execute-plan-non-interactive`.
3. Extract current Phase 5 review policy into the review helper pair.
4. Extract current Phase 6 validation policy into the validation helper pair.
5. Replace embedded Phase 5 and Phase 6 sections in the execution skills with helper-skill invocation contracts.
6. Keep shared handoff protocol in one place, but move concern-specific prompt rules into the relevant helper skills.

## Testing Implications

The split should enable separate testing of:
- interactive execution orchestration
- non-interactive execution orchestration
- interactive review-loop behavior
- non-interactive review-loop behavior
- interactive validation-loop behavior
- non-interactive validation-loop behavior

Review and validation helper tests should verify frozen reviewer/validator behavior, retry caps, triage persistence, duplicate suppression, deterministic stop behavior, and prompt construction stability.

## Success Criteria

The split is successful when:
- no skill mixes interactive and non-interactive behavior
- review policy is isolated from execution orchestration
- validation policy is isolated from execution orchestration
- helpers run in the same agent as the orchestrator and spawn narrow reviewer/validator sub-agents underneath them
- state needed across iterations is explicitly persisted instead of reconstructed from long-running conversational context
- editing review logic cannot accidentally change execution orchestration semantics
- editing validation logic cannot accidentally change execution orchestration semantics
