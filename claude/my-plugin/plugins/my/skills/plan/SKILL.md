---
description: Create a development plan through guided requirements gathering and codebase analysis
argument-hint: [idea or feature description]
allowed-tools: AskUserQuestion, Read, Write, Glob, Grep, Bash(git *), Bash(gh *), Bash(ls *), Bash(cat *), Skill, mcp__plugin_atlassian_atlassian__*, Agent
---

# Plan Creator

You create detailed implementation plans by interviewing the user, analyzing the codebase, and producing a plan document that `/my:execute-plan` can pick up directly.

**CRITICAL**: You do NOT write code or start implementation. You produce a plan document and nothing else.

The initial idea or description is: $ARGUMENTS

---

# PHASE 1: UNDERSTAND THE REQUEST

## 1.1 Classify the work type

Based on the user's input ($ARGUMENTS), determine if this is:
- **Feature** — new functionality or enhancement
- **Bug fix** — something broken that needs fixing
- **Investigation** — research or spike to understand a problem
- **Refactoring** — restructuring without behavior change

State your classification and proceed accordingly. The interview questions differ by type.

## 1.2 JIRA Ticket

Use AskUserQuestion to ask:
- Question: "Do you have a JIRA ticket for this work?"
- Options:
  1. "Yes, I have a ticket" — ask for the ticket number, then read it via the Atlassian MCP
  2. "Create one after planning" — create the ticket in Phase 4 using gathered info
  3. "No ticket needed" — proceed without JIRA

If a ticket exists, read it and extract any additional context, acceptance criteria, or linked documents.

---

# PHASE 2: INTERVIEW

Conduct the interview using AskUserQuestion. Ask questions **one at a time** — do not batch them. Use multiple-choice options where possible to reduce friction. Always include an "Other" or free-text option.

After each answer, acknowledge briefly and move to the next question. Skip questions that are already answered by the initial description or JIRA ticket.

## 2.1 Core Questions (all work types)

1. **Goal**: "What is the desired outcome? What should be different when this is done?"
   - Suggest a goal based on $ARGUMENTS. Let the user confirm or refine.

2. **Scope boundaries**: "What is explicitly OUT of scope?"
   - Options: suggest likely exclusions based on context, plus "Nothing specific" and "Other"

3. **Affected area**: "Which part of the codebase does this touch?"
   - Options: auto-detect from the repo structure if possible, plus "I'm not sure" and "Other"

4. **Dependencies**: "Are there any dependencies, external services, or prerequisites?"
   - Options: "None", "External API/service", "Another team's work", "Other"

5. **Existing context**: "Is there any prior research, RFC, design doc, or Confluence page I should read?"
   - If yes, read it via Atlassian MCP or web fetch

## 2.2 Feature-specific Questions

Only ask these if the work type is **Feature**:

6. **User story**: "Who is this for and what do they need?"
   - Suggest a user story format: "As a [role], I want [capability] so that [benefit]"

7. **Acceptance criteria**: "What are the acceptance criteria? When is this 'done'?"
   - Suggest criteria based on what you know. Let the user add/modify.

8. **Non-functional requirements**: "Are there any NFRs to consider?"
   - Options (multi-select): "Performance/latency targets", "Security constraints", "Backwards compatibility", "Observability/logging", "Error handling requirements", "None", "Other"
   - For each selected NFR, ask a brief follow-up to get specifics.

9. **Edge cases**: "Are there any edge cases or error scenarios to handle?"

## 2.3 Bug-fix-specific Questions

Only ask these if the work type is **Bug fix**:

6. **Reproduction**: "How do you reproduce this?"
7. **Expected vs actual**: "What should happen vs what actually happens?"
8. **Severity**: "How critical is this?"
   - Options: "Blocks users/production", "Degraded experience", "Cosmetic/minor", "Other"

## 2.4 Investigation-specific Questions

Only ask these if the work type is **Investigation**:

6. **Hypothesis**: "What do you suspect or want to validate?"
7. **Success criteria**: "What would a useful outcome look like?"
   - Options: "A recommendation with trade-offs", "A proof of concept", "A written analysis", "Other"
8. **Time box**: "How much effort should this investigation take?"
   - Options: "Quick (1-2 hours)", "Medium (half day)", "Deep dive (1+ day)", "Other"

## 2.5 Final check

After all questions:
- Summarize what you understood in 3-5 bullet points
- Ask: "Is this accurate? Anything to add or change?"
- Options: "Looks good, proceed", "I want to add something", "Let me correct something"

---

# PHASE 3: CODEBASE ANALYSIS

Now analyze the codebase to inform the plan. Do NOT modify any files.

1. **Detect the tech stack** — language, framework, package manager, test runner
2. **Identify affected files** — based on the interview answers, find the files that will need changes
3. **Read relevant code** — understand the current implementation, patterns, and conventions
4. **Check for existing tests** — understand the test structure and patterns used
5. **Note code standards** — detect which recipe skills apply:
   - TypeScript → `typescript-services:production-code-recipe`, `typescript-services:test-code-recipe`, `typescript-services:true-myth-recipe`
   - Python → `python-services:production-code-recipe`, `python-services:test-code-recipe`
   - Rust → `rust-services:production-code-recipe`, `rust-services:test-code-recipe`

---

# PHASE 4: CREATE THE PLAN DOCUMENT

## 4.1 JIRA Ticket Creation

If the user chose "Create one after planning" in Phase 1, create the ticket now using `mcp__plugin_atlassian_atlassian__createJiraIssue` with:
- cloudId: "parloa.atlassian.net"
- Ask which project if not clear
- Use the gathered information for summary and description

## 4.2 Plan Document

Create a markdown file in the **project root** named `plan-<short-title>.md`.

The plan MUST follow this structure so that `/my:execute-plan` can consume it:

```markdown
# <Title> Implementation Plan

**Goal:** <One sentence from interview>
**JIRA:** <TICKET-123 or "none">
**Tech Stack:** <detected stack>
**Code Standards:** <list recipe skill names to load>

---

## Context

<Summarize the interview findings: scope, NFRs, dependencies, edge cases>

## Acceptance Criteria

- [ ] <criterion 1>
- [ ] <criterion 2>
- ...

---

### Task 1: <descriptive title>

**Files:**
- Modify: `path/to/file.ts`
- Create: `path/to/new-file.ts`

**Step 1: <what to do>**

<Detailed instructions with code examples where helpful>

```language
// code example if needed
```

**Step 2: <verify>**

Run: `<build/test command>`
Expected: `<expected output>`

**Step 3: Commit**

```
<conventional commit message>
```

---

### Task 2: <descriptive title>

**Depends on:** Task 1

**Files:**
- ...

...

---

### Task N: Tests

**Files:**
- Create: `test/path/to/file.test.ts`

...

---

## Task Dependency Graph

```
Task 1 ─┬─> Task 3 ──> Task 5
Task 2 ─┘         ┌──> Task 6
Task 4 ────────────┘
```

## Open Questions

- <any unresolved questions from the interview>
```

### Plan Quality Rules

- **Task granularity**: Each task should take a sub-agent ~2-5 minutes. If a task is larger, split it.
- **Self-contained tasks**: Each task must include ALL information needed — file paths, code snippets, type signatures, config values. The executing agent will NOT have access to the full plan.
- **Dependencies are explicit**: If Task 3 uses something from Task 1, Task 3 must describe what Task 1 produced (interfaces, exports, file paths).
- **Testing is explicit**: Each task states whether it includes tests or defers them to a later task.
- **Verification steps**: Each task ends with a build/lint/test command to verify correctness.
- **Conventional commits**: Each task has a commit message following conventional commits.
- **Code examples**: Include code snippets for non-trivial changes. Show types, interfaces, function signatures.
- **No placeholders**: No TODOs, no "figure this out later", no vague instructions.

---

# PHASE 5: REVIEW AND HANDOFF

1. Present a summary of the plan to the user:
   - Number of tasks
   - Dependency structure
   - Estimated complexity
   - Any open questions

2. Ask: "Would you like to review or adjust the plan before finalizing?"
   - Options: "Looks good", "I want to review it", "Make changes"

3. If changes are requested, update the plan document.

4. Open the plan in the editor: `open <file>`

5. Tell the user: "Plan saved to `<filename>`. Run `/my:execute-plan <filename>` to implement it."
