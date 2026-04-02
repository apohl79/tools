---
description: Use when a single parallel reviewer-team run is needed — launches the frozen Claude + Codex + Gemini reviewer set, collects all outputs, triages findings, and returns a structured review report.
---

# Run Reviewer Team

This skill executes one reviewer-team run: it freezes the three-reviewer set, builds reviewer prompts, dispatches all three reviewers in parallel as sub-agents, collects every output, triages findings, and returns a self-contained review report.

It does NOT decide whether to fix, retry, or escalate. That logic belongs to the caller.

## Required Inputs

- `plan_context` — plan path or relevant plan excerpts that define the expected implementation
- `execution_outputs` — description or summary of what was built or changed during execution
- `changed_files` — list of files created or modified
- `language` — detected primary language of the changed files
- `recipe_list` — recipe skills relevant to the changed code (used to build reviewer prompts)
- `prior_review_context` — prior triage history for this review loop; must include already-fixed, rejected, and deferred findings so reviewers do not re-raise resolved items; pass empty object `{}` on the first run

If any required input is missing, stop immediately and return `status: blocked` with the missing field in `notes`.

## Reviewer Set

The frozen reviewer set for every invocation is exactly three reviewers:

1. **Claude** — launched as a focused sub-agent via the Agent tool
2. **Codex** — launched via `mcp__codex__codex`
3. **Gemini** — launched via `mcp__gemini-cli__ask-gemini`

All three must be launched in the same parallel batch. Do not launch them sequentially.

A run is not complete until all three reviewer outputs have been collected. Do not produce a triage report from a partial batch.

If a tool is unavailable, return `status: blocked` with the tool name and a concrete availability error in `notes`. Do not substitute a different reviewer or reduce the set below three.

## Reviewer Prompt Contract

Build one prompt per reviewer. Each prompt must include:

- the review scope: changed files, plan context, execution summary
- language and recipe context
- prior review context: already-fixed findings, rejected findings, deferred findings
- the reporting contract below

**Reporting contract to include in every reviewer prompt:**

> Report only findings within the current review scope. For each finding include: file path, line reference if applicable, a concrete description, and your reasoning. Classify every finding as one of:
> - `FIX_REQUIRED` — real, in-scope, must be fixed
> - `VERIFIED_FIX` — a prior FIX_REQUIRED issue that is now correctly fixed
> - `REJECTED` — invalid, out of scope, or based on incorrect assumptions
> - `DEFERRED` — real but intentionally left unresolved (must state reason)
>
> Do not re-raise findings already marked fixed, rejected, or deferred in prior review context unless you have new evidence that invalidates the prior decision. Do not make code changes directly.

## Execution

1. Validate all required inputs are present.
2. Build one reviewer prompt per reviewer using the contract above.
3. Launch all three reviewers in a single parallel batch:
   - Claude sub-agent via Agent tool (subagent_type: general-purpose)
   - Codex via `mcp__codex__codex`
   - Gemini via `mcp__gemini-cli__ask-gemini`
4. Wait for all three outputs before proceeding.
5. Triage every finding from every reviewer into exactly one bucket:
   - `FIX_REQUIRED`
   - `VERIFIED_FIX`
   - `REJECTED`
   - `DEFERRED`
   - Deduplicate across reviewers: if multiple reviewers raise the same issue, merge into one finding and note that N reviewers agreed.
6. Produce the review report (see Completion Contract).

## Completion Contract

Return one structured report with these fields:

- `status` — `complete` | `blocked`
- `reviewer_set` — list of the three reviewers used
- `attempt_note` — free-text note about this run (e.g. first attempt, retry N)
- `findings` — list of triaged findings; each entry:
  - `id` — short unique identifier (e.g. `F1`, `F2`)
  - `category` — one of `FIX_REQUIRED` | `VERIFIED_FIX` | `REJECTED` | `DEFERRED`
  - `file` — affected file path
  - `description` — concrete description of the finding
  - `reasoning` — reviewer reasoning
  - `reviewers` — which reviewer(s) raised this finding
  - `deferred_reason` — populated only for `DEFERRED`
- `triage_summary` — counts per category: `fix_required`, `verified_fix`, `rejected`, `deferred`
- `notes` — any blocker detail, tool errors, or observations

### `status: complete`
All three reviewers ran and produced output. Triage is complete. `findings` and `triage_summary` are populated.

### `status: blocked`
A required tool was unavailable or a required input was missing. `findings` and `triage_summary` may be empty or partial. `notes` must contain the exact blocker.
