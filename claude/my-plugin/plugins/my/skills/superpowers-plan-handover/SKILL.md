---
description: Use when a Superpowers plan should be handed over into the my-plugin execution flow.
argument-hint: [superpowers-plan-path]
---

# superpowers-plan-handover

You convert a Superpowers plan into a `.my/plans/` execution-ready plan and then offer execution options that include both the standard Superpowers handoff and the my-plugin executors.

You do NOT implement product code. You only migrate the plan, preserve its content, add the required execution flags, and hand off to the correct executor.

The initial plan input is: $ARGUMENTS

## Required outcome

In every successful path:

1. Resolve the source Superpowers plan file.
2. Ask about the JIRA ticket using the same approach as `my:plan`.
3. Convert it into a plan file under `.my/plans/`.
4. Preserve the existing plan title, goal, architecture, tech stack, and plan body as much as possible.
5. Ensure the migrated plan contains these execution headers near the top of the file:
   - `**Status:** READY`
   - `**no-worktree:** [ ]`
   - `**no-pr:** [ ]`
   - `**draft-pr:** [ ]`
   - `**merge:** [ ]`
   - `**merge-admin:** [ ]`
6. Offer these execution options after the migrated plan is written:
   - Superpowers: Subagent-Driven
   - Superpowers: Inline Execution
   - My Executor: Interactive mode
   - My Executor: Non-Interactive mode

## JIRA ticket

JIRA handling is mandatory.

Before execution flags or execution choices, you MUST ask the user about JIRA using the same approach as `my:plan`.

Use `AskUserQuestion` with:
- Question: "Do you have a JIRA ticket for this work?"
- Options:
  - `Yes, I have a ticket`
  - `Create one after planning`
  - `No ticket needed`

Rules:
- Never trust or reuse the source Superpowers plan's JIRA field without asking the user.
- If the user has a ticket, ask for the exact ticket value and write `**JIRA:** <ticket>`.
- If the user chose `Create one after planning`, write `**JIRA:** TO-BE-CREATED`.
- If the user chose `No ticket needed`, write `**JIRA:** none`.
- After updating the migrated plan, re-read it and verify the header contains the exact chosen JIRA value.
- Do not continue until JIRA has been written and verified.
- Do not ask execution-flag questions or execution-choice questions before JIRA verification passes.

The migrated plan header must contain exactly one of these outcomes before the next phase starts:
- `**JIRA:** <ticket>`
- `**JIRA:** TO-BE-CREATED`
- `**JIRA:** none`

If verification fails, fix the migrated file before continuing.

JIRA handling always happens after source-plan resolution and before execution-flag handling.

Skipping the JIRA step is not allowed.

If the JIRA step has not completed, the handover is not complete.

Do not proceed with stale, inferred, copied, or ambiguous JIRA state.

That behavior is required.
## Plan migration rules

### Source plan resolution

- If `$ARGUMENTS` contains a plan path, use it.
- If no path is provided, search `docs/superpowers/plans/` for candidate plan files.
- If exactly one plausible plan exists, use it.
- If multiple plausible plans exist, ask the user to choose with `AskUserQuestion`.
- Stop with a deterministic error if the chosen source file does not exist or cannot be read.

### Target path

- The migrated file MUST live in `.my/plans/`.
- Reuse the source filename stem when possible.
- Prefer a path of the form `.my/plans/plan-<normalized-source-name>.md`.
- If that exact target path already exists and is not the intended file for this run, create a unique suffixed filename instead of overwriting silently.

### Migration format

Transform the Superpowers plan into a my-plan-compatible header while keeping the existing Superpowers structure and content below the header.

The migrated file MUST begin with this header shape:

```md
# <Original Title>

**Goal:** <goal line from source plan>
**Type:** Feature
**JIRA:** none
**Tech Stack:** <tech stack line from source plan, or "unknown">
**Code Standards:** n/a
**Status:** READY
**no-worktree:** [ ]
**no-pr:** [ ]
**draft-pr:** [ ]
**merge:** [ ]
**merge-admin:** [ ]

---
```

Migration requirements:

- Preserve the original title.
- Preserve the original `**Goal:**` if present. If missing, derive it from the Superpowers goal section.
- Preserve the original `**Tech Stack:**` if present. If missing, infer a concise value from the plan text when possible, otherwise use `unknown`.
- Set `**Type:** Feature` unless the source plan clearly states a different type in its header. If it does, preserve that type.
- Set `**JIRA:** none` unless the source plan already contains a clear ticket value.
- Set `**Code Standards:** n/a` unless the source plan already contains that field.
- Replace any existing status line with `**Status:** READY`.
- Add any missing execution-flag headers.
- Remove the Superpowers execution banner block that begins with `> **For agentic workers:** REQUIRED SUB-SKILL:` so the migrated plan no longer instructs execution through Superpowers skills.
- Keep the remaining plan body intact after the header separator.

When removing the execution banner:
- remove the full quoted block, not just the first line
- remove any immediately attached execution-only continuation text that belongs to that banner
- do not remove normal plan content such as goal, architecture, tech stack, tasks, files, or verification steps
- ensure the migrated document does not mention `superpowers:subagent-driven-development` or `superpowers:executing-plans` in the carried-over plan body

If those strings still exist after migration, rewrite the affected carried-over section so execution guidance points to the migrated `.my/plans/...` flow instead of Superpowers execution skills.

The migrated plan must not contain stale Superpowers execution instructions.

If execution-related cleanup changes the carried-over body, re-read the migrated file and verify the remaining plan content is still coherent.

### Validation after migration

After writing the migrated file, read it back and verify:

- it is under `.my/plans/`
- it contains all required execution flags
- it contains `**Status:** READY`
- the original plan body is still present

If validation fails, fix the migrated file before continuing.

After applying the user-selected execution flags, read the migrated file again and verify that the selected flags now use `[x]` and all unselected flags remain `[ ]`.

If that verification fails, fix the migrated file before presenting execution choices.

## Execution flags

After the migrated plan is validated, you MUST ask about execution flags using the same approach as `my:plan`.

Use **two** `AskUserQuestion` calls:

### Question 1

- Question: "Which execution flags should be enabled?"
- multiSelect: true
- Options:
  - `no-worktree` — Skip creating a git worktree and run directly in the current working directory
  - `no-pr` — Skip creating a pull request after implementation
  - `draft-pr` — Create a draft PR instead of a ready-for-review PR
  - `None of these` — No execution flags needed

Rules:
- Treat `None of these` deterministically: ignore it whenever any real flag is also selected, and only treat it as meaningful when it is the sole selection.
- For each selected real flag, update the migrated plan header by replacing `**<flag>:** [ ]` with `**<flag>:** [x]`.
- Leave unselected flags as `[ ]`.

### Question 2

- Skip this question if `no-pr` or `draft-pr` were enabled.
- Question: "Do you want to enable auto-merge?"
- multiSelect: false
- Options:
  - `merge` — Merge after the PR has been finalized
  - `merge-admin` — Merge with `--admin` after the PR has been finalized
  - `None of these` — No auto-merge needed

Rules:
- If `merge` is selected, update `**merge:** [ ]` to `**merge:** [x]`.
- If `merge-admin` is selected, update `**merge-admin:** [ ]` to `**merge-admin:** [x]`.
- Leave all unselected flags as `[ ]`.

## Execution options

Only after the execution flags have been applied to the migrated `.my/plans/...` file, present these four options with `AskUserQuestion`:

1. **Superpowers: Subagent-Driven**
   - Use `superpowers:subagent-driven-development`
   - Pass the migrated `.my/plans/...` file path as the execution target when possible
2. **Superpowers: Inline Execution**
   - Use `superpowers:executing-plans`
   - Pass the migrated `.my/plans/...` file path as the execution target when possible
3. **My Executor: Interactive mode**
   - Run `my:execute-plan` with the converted plan document
4. **My Executor: Non-Interactive mode**
   - Run `my:execute-plan-non-interactive` with the converted plan document

## Executor handoff rules

### My Executor: Interactive mode

- Invoke `my:execute-plan`.
- Pass the migrated plan path as the explicit plan argument.
- Do not ask the user to rerun anything manually when the skill can invoke the executor directly.

### My Executor: Non-Interactive mode

- Invoke `my:execute-plan-non-interactive`.
- Pass the migrated plan path as the explicit plan argument.
- Non-interactive mode is handled in this same handoff flow.

### Superpowers options

- If the user chooses a Superpowers execution path, continue with the selected Superpowers skill using the migrated `.my/plans/...` document as the source of truth for execution.
- Do not leave execution pointed at the old `docs/superpowers/plans/...` file.

## Constraints

- Do NOT write implementation code.
- Do NOT leave the migrated plan outside `.my/plans/`.
- Do NOT omit the execution flags required by `my:plan`.
- Do NOT keep the migrated plan in WIP state.
- Do NOT offer execution choices until the migrated plan has been validated.
- Do NOT offer execution choices until the user has answered the execution-flag questions and the migrated plan has been updated and re-verified.
- Do NOT skip the execution-flag questions.

## Final response pattern

When migration succeeds, report:

- source plan path
- migrated plan path
- verification result
- selected execution path or the fact that execution choice is needed
