---
description: Use when the user wants a new implementation plan created from an idea, feature request, bug report, investigation, refactor, deployment change, or research task.
argument-hint: [idea or feature description]
---

# plan

You create detailed implementation plans by interviewing the user, analyzing the codebase, and producing a plan document that `/my:execute-plan` can pick up directly.

**CRITICAL**: You do NOT write code or start implementation. You produce a plan document and nothing else.

The initial idea or description is: $ARGUMENTS

---

**Before starting Phase 1**, create a progress tasklist using the TaskCreate tool — one task per phase:
- "Understand the Request"
- "Interview"
- "Codebase Analysis"
- "Create Plan Document"
- "Automated Plan Review"
- "Review and Handoff"

---

# PHASE 1: UNDERSTAND THE REQUEST

**Mark the "Understand the Request" task `in_progress` before starting. Mark it `completed` when done.**

## 1.1 Classify the work type

Based on the user's input ($ARGUMENTS), determine if this is:
- **Feature** — new functionality or enhancement
- **Bug fix** — something broken that needs fixing
- **Investigation** — research or spike to understand a problem
- **Refactoring** — restructuring without behavior change
- **Deployment / Infra change** — infrastructure, config, CI/CD, Terraform, Helm, Kubernetes, or deployment changes with no application code changes
- **Research** — open-ended exploration, analysis, or technical discovery producing a written output (no code changes)

State your classification and proceed accordingly. The interview questions differ by type.

## 1.2 Initial Code Scan

Before starting the interview, do a short codebase scan to get context. Do NOT modify any files.

The scan should be brief and targeted:
- detect the tech stack and likely affected area
- read the most relevant existing files for this request
- identify whether there are existing tests near that area
- assess whether this looks like a **simple/localized change** or a **larger/unclear change**
- note any obvious architecture constraints, NFRs, or security-sensitive areas

Use this scan to make the interview adaptive:
- **Simple/localized change** — keep the interview brief and ask only for missing information that materially affects the plan
- **Larger/unclear/risky change** — run the fuller interview for that work type

State your scan findings briefly before continuing.

## 1.3 JIRA Ticket

Use AskUserQuestion to ask:
- Question: "Do you have a JIRA ticket for this work?"
- Options:
  1. "Yes, I have a ticket" — ask for the ticket number, then read it via the Atlassian MCP
  2. "Create one after planning" — create the ticket in Phase 4 using gathered info
  3. "No ticket needed" — proceed without JIRA

If a ticket exists, read it and extract any additional context, acceptance criteria, or linked documents. If Atlassian MCP access or any external retrieval fails, ask the user to paste the relevant ticket/context or proceed with the information already available.

---

# PHASE 2: INTERVIEW

**Mark the "Interview" task `in_progress` before starting. Mark it `completed` when done.**

Conduct the interview using AskUserQuestion. Ask questions **one at a time** — do not batch them. Use multiple-choice options where possible to reduce friction. Always include an "Other" or free-text option.

After each answer, acknowledge briefly and move to the next question. Skip questions that are already answered by the initial description, the initial code scan, or the JIRA ticket.

Keep the interview proportional to the work:
- For a simple/localized change, ask only the minimum set of questions needed to clarify requirements, constraints, NFRs, security concerns, and unknowns.
- For a larger or riskier change, ask the fuller set of relevant questions below.
- Do NOT ask questions whose answers are already clear from the request, ticket, or code scan.
- If the change is obviously small and low-risk, it is acceptable to skip entire question groups once you have enough context to write a good plan.

## 2.1 Core Questions (all work types)

1. **Goal**: "What is the desired outcome? What should be different when this is done?"
   - Suggest a goal based on $ARGUMENTS. Let the user confirm or refine.

2. **Scope boundaries**: "What is explicitly OUT of scope?"
   - Options: suggest likely exclusions based on context, plus "Nothing specific" and "Other"

## 2.2 Feature-specific Questions

Only ask these if the work type is **Feature**:

3. **Non-functional requirements**: Ask this using **two** AskUserQuestion calls so each stays within the 4-option limit.
   - Question 1 options (multi-select): "Performance/latency targets", "Security constraints", "Error handling requirements", "Other / free-text"
   - Question 2 options (multi-select): "Backwards compatibility", "Observability/logging", "None of these", "Other / free-text"
   - If the user selects "Other / free-text" in either question, capture the custom NFRs in free text before moving on.
   - Combine the selections across both questions, ignoring any "None of these" choice when other options were selected in Question 2.
   - Treat selecting only "None of these" in Question 2, with no selections from Question 1 other than optional free-text that adds no NFRs, as an explicit answer that there are no cataloged NFRs beyond any captured free-text.
   - Keep "Error handling requirements" as an explicit selectable NFR. Do NOT defer it to a hidden follow-up trigger.
   - For each selected NFR, including error handling when selected, ask a brief follow-up to get specifics.

4. **Edge cases**: "Are there any edge cases or error scenarios to handle?"

## 2.3 Bug-fix-specific Questions

Only ask these if the work type is **Bug fix**:

3. **Reproduction**: "How do you reproduce this?"
4. **Expected vs actual**: "What should happen vs what actually happens?"
5. **Severity**: "How critical is this?"
   - Options: "Blocks users/production", "Degraded experience", "Cosmetic/minor", "Other"

## 2.4 Investigation-specific Questions

Only ask these if the work type is **Investigation**:

3. **Hypothesis**: "What do you suspect or want to validate?"
4. **Success criteria**: "What would a useful outcome look like?"
   - Options: "A recommendation with trade-offs", "A proof of concept", "A written analysis", "Other"
5. **Time box**: "How much effort should this investigation take?"
   - Options: "Quick (1-2 hours)", "Medium (half day)", "Deep dive (1+ day)", "Other"

## 2.5 Deployment / Infra-specific Questions

Only ask these if the work type is **Deployment / Infra change**:

3. **Target environment(s)**: Ask this using AskUserQuestion with at most 4 options.
   - Question: "Which environments does this affect?"
   - Options: "Dev only", "Staging", "Production", "Other"
   - If the user answers "Other", allow free-text such as "All environments".
4. **Rollback plan**: Ask this using AskUserQuestion with at most 4 options.
   - Question: "What's the rollback strategy if something goes wrong?"
   - Options: "Revert the config/commit", "Feature flag / toggle", "Manual rollback steps", "Other"
   - If the user answers "Other", allow free-text such as "No rollback needed".
5. **Downtime**: Ask this using AskUserQuestion with at most 4 options.
   - Question: "Does this change require downtime or cause service disruption?"
   - Options: "No downtime", "Brief restart", "Rolling update", "Other"
   - If the user answers "Other", allow free-text such as "Maintenance window required".

## 2.6 Research-specific Questions

Only ask these if the work type is **Research**:

3. **Research question**: "What specific question should this research answer?"
4. **Deliverable**: Ask this using AskUserQuestion with at most 4 options.
   - Question: "What is the expected output?"
   - Options: "Written analysis / doc", "ADR (Architecture Decision Record)", "Proof of concept", "Other"
   - If the user answers "Other", allow free-text such as "Recommendation with trade-offs".
5. **Time box**: "How much effort should this research take?"
   - Options: "Quick (1-2 hours)", "Medium (half day)", "Deep dive (1+ day)", "Other"
6. **Success criteria**: "How will you know the research is done?"

## 2.7 Final check

After all questions:
- Summarize what you understood in 3-5 bullet points
- Ask: "Is this accurate? Anything to add or change?"
- Options: "Looks good, proceed", "I want to add something", "Let me correct something"

---

# PHASE 3: CODEBASE ANALYSIS

**Mark the "Codebase Analysis" task `in_progress` before starting. Mark it `completed` when done.**

Expand the initial code scan as needed to inform the plan. Do NOT modify any files.

1. **Confirm the tech stack** — language, framework, package manager, test runner
2. **Identify affected areas** — based on the interview answers, find the files, modules, services, or workflows likely to change
3. **Read relevant code** — understand the current implementation, architecture boundaries, and conventions
4. **Check for existing tests** — understand the current validation patterns and test coverage in the affected area
5. **Note code standards** — detect which recipe skills apply:
   - TypeScript → `typescript-services:production-code-recipe`, `typescript-services:test-code-recipe`, and `typescript-services:true-myth-recipe` only when the codebase already uses true-myth or the planned change will introduce it intentionally
   - Python → `python-services:production-code-recipe`, `python-services:test-code-recipe`
   - Rust → `rust-services:production-code-recipe`, `rust-services:test-code-recipe`
6. **Capture planning inputs** — architecture constraints, integration points, NFRs, security considerations, migration or rollout implications, and remaining unknowns

---

# PHASE 4: CREATE THE PLAN DOCUMENT

**Mark the "Create Plan Document" task `in_progress` before starting. Mark it `completed` when the plan file is written.**

## 4.1 JIRA Ticket Creation

If the user chose "Create one after planning" in Phase 1, create the ticket now using `mcp__plugin_atlassian_atlassian__createJiraIssue` with:
- cloudId: "parloa.atlassian.net"
- Ask which project if not clear
- Use the gathered information for summary and description

## 4.2 Plan Document

Create the directory `.my/plans/` in the project root if it doesn't exist, then create a markdown file with a deterministic non-colliding name. First try `.my/plans/plan-<short-title>.md`. If that file already exists, reuse it only when the user explicitly chose that existing plan path or filename for this planning run; otherwise create a unique suffixed filename such as `.my/plans/plan-<short-title>-2.md` instead of overwriting silently. When reusing an existing plan file, rewrite the full plan document body for the new planning run before continuing to review or acceptance. Do not keep stale task content, context, acceptance criteria, open questions, or prior summaries in place. Reset `**Status:** WIP` and normalize all execution flags (`**no-worktree:**`, `**no-pr:**`, `**draft-pr:**`) back to `[ ]` as part of that full rewrite so stale state cannot leak into the new plan.

The plan MUST follow this structure so that `/my:execute-plan` can consume it:

```markdown
# <Title> Implementation Plan

**Goal:** <One sentence from interview>
**Type:** <Feature | Bug fix | Investigation | Refactoring | Deployment / Infra change | Research>
**JIRA:** <TICKET-123 or "none">
**Tech Stack:** <detected stack>
**Code Standards:** <list recipe skill names to load, or "n/a" for Deployment/Research>
**Status:** WIP
**no-worktree:** [ ]
**no-pr:** [ ]
**draft-pr:** [ ]
**merge:** [ ]
**merge-admin:** [ ]

---

## Context

<Summarize the problem, scope, affected area, dependencies, and relevant current-state findings>

## Requirements

### Functional Requirements
- [ ] <requirement 1>
- [ ] <requirement 2>

### Out of Scope
- <explicitly excluded item>

## Acceptance Criteria

- [ ] <criterion 1>
- [ ] <criterion 2>

## Architecture / Approach

<Describe the intended solution at a strategy level. Focus on components, boundaries, integrations, data flow, and rollout approach. Do NOT prescribe detailed code-level implementation unless already required by the request.>

## Affected Areas

- `path/or/module` — <why it is relevant>
- `path/or/module` — <why it is relevant>

## Non-Functional Requirements

- **Performance:** <requirements or "none identified">
- **Reliability / Error Handling:** <requirements or "none identified">
- **Observability:** <requirements or "none identified">
- **Compatibility / Migration:** <requirements or "none identified">

## Security Considerations

- <auth/authz, secrets, input validation, data sensitivity, tenant isolation, auditability, or "no special concerns identified">

## Testing / Validation Strategy

- <unit/integration/e2e/manual validation approach>
- <key regression areas>

## Implementation Phases

### Phase 1: <descriptive title>
- **Goal:** <what this phase achieves>
- **Scope:** <what it covers>
- **Dependencies:** <none | prior phases/external dependency>
- **Notes:** <important constraints, sequencing, or rollout notes>

### Phase 2: <descriptive title>
- **Goal:** <what this phase achieves>
- **Scope:** <what it covers>
- **Dependencies:** <Phase 1>
- **Notes:** <important constraints, sequencing, or rollout notes>

## Open Questions

- <any unresolved questions from the interview or analysis>
```

### Plan Quality Rules

- **Strategic, not task-level**: The plan should describe requirements, architecture, affected areas, NFRs, security concerns, validation strategy, and phased implementation. Do NOT break the work down into sub-agent-sized tasks.
- **Keep code decisions high level**: Leave detailed code-level decisions for later unless they are already dictated by the request, current architecture, or an explicit constraint.
- **Phases over micro-tasks**: Use a small number of meaningful implementation phases or workstreams. These should be enough for `/my:execute-plan` to decompose later.
- **Dependencies are explicit**: Phase/workstream dependencies, sequencing constraints, and rollout implications must be clear.
- **Testing is strategic**: Describe the validation strategy and regression areas, not per-task commands.
- **Security and NFRs are explicit**: Always document relevant NFRs and security considerations, even when the answer is that none were identified.
- **Affected areas are concrete**: Name likely files, modules, services, or workflows when they can be identified from the analysis.
- **No placeholders**: No TODOs, no "figure this out later", no vague instructions.

---

# PHASE 4.5: AUTOMATED PLAN REVIEW LOOP

**Mark the "Automated Plan Review" task `in_progress` before starting. Mark it `completed` when the review passes (or after surfacing unresolved issues to the user).**

After writing the plan document, run an automated review loop to catch gaps before presenting it to the user. This loop runs a maximum of **3 iterations**. Track `review_attempt` starting at 1.

1. **Dispatch a plan-document-reviewer sub-agent** using the Agent tool with a general-purpose agent and the following prompt (substitute `[PLAN_FILE_PATH]` with the absolute path to the plan file):

   ```
   You are a plan document reviewer. Verify this implementation plan is complete and ready for execution.

   **Plan to review:** [PLAN_FILE_PATH]

   Do NOT modify any files. You are a reviewer, not an implementer.

   ## What to Check

   | Category | What to Look For |
   |----------|-----------------|
   | Completeness | TODOs, placeholders, "TBD", missing phases/sections, incomplete requirements |
   | Consistency | Internal contradictions, conflicting requirements, architecture, or affected areas |
   | Clarity | Instructions ambiguous enough to cause the executor to choose the wrong approach |
   | Granularity | Phases/workstreams that are too vague, too broad, or missing meaningful sequencing |
   | Dependencies | Missing dependency declarations between phases/workstreams or missing rollout constraints |
   | Verification | Missing validation strategy or missing key regression areas |
   | Scope | Plan covers more than what was requested (over-engineering / YAGNI) |

   ## Calibration

   Only flag issues that would cause real problems during implementation. A missing dependency,
   a contradiction, or an ambiguous instruction that could be interpreted two different ways —
   those are issues. Minor wording preferences and "less detail than other sections" are not.

   Approve unless there are serious gaps that would lead to a flawed or incomplete implementation.

   ## Output Format

   ## Plan Review

   **Status:** Approved | Issues Found

   **Issues (if any):**
   - [Task N / Section]: [specific issue] — [why it matters for execution]

   **Recommendations (advisory, do not block approval):**
   - [suggestions]
   ```

2. **Evaluate the review result:**

   - If **Status is "Approved"**: proceed to Phase 5.

   - Automated review approval only makes the plan eligible for final acceptance. The plan MUST remain `WIP` until Phase 5 ends with the user explicitly accepting the final version that will be saved.
   - If unresolved review issues remain after the automated loop, the plan still MUST remain `WIP` until the user explicitly acknowledges those remaining issues and explicitly accepts the final version in Phase 5.

   - If **Status is "Issues Found"** and `review_attempt < 3`:
     a. Fix each reported issue directly in the plan document.
     b. Increment `review_attempt` and re-dispatch the reviewer (return to step 1).

   - If substantive Phase 5 edits are made later to phase structure, dependencies, acceptance criteria, architecture, validation strategy, security considerations, or other execution-relevant plan content, re-run this automated review loop before the plan can become `READY` again. Purely clerical edits such as wording polish or execution-flag updates do not require re-running Phase 4.5.

   - If **Status is "Issues Found"** and `review_attempt >= 3`:
     a. Do NOT attempt further automated fixes.
     b. Summarize the remaining issues for the user in Phase 5 and ask them to decide.

---

# PHASE 5: REVIEW AND HANDOFF

**Mark the "Review and Handoff" task `in_progress` before starting. Mark it `completed` after handing off to the user.**

1. Present a summary of the plan to the user:
   - Number of implementation phases / workstreams
   - Dependency structure
   - Estimated complexity
   - Key architecture decisions
   - Key NFRs and security considerations
   - Any open questions
   - Any issues from Phase 4.5 that could not be auto-resolved (if `review_attempt >= 3`)

2. Ask: "Would you like to review or adjust the plan before finalizing?"
   - Options: "Looks good", "I want to review it", "Make changes"

3. If the user chooses "I want to review it", walk through their feedback, answer questions, and keep the plan `WIP` until they explicitly accept the final version.
   - After answering, ask again whether they want to accept the current version, make changes, or keep reviewing.
   - If they still need review, stay in Phase 5 without changing status.
   - If they want changes, follow step 4.

4. If the user chooses "Make changes", update the plan document, summarize the changes you made, and treat substantive edits as a return to the automated-review gate before final acceptance.
   - After making changes, ask again whether they want additional changes, need more review, or accept the current version.
   - If the edits were substantive to phase structure, dependencies, acceptance criteria, architecture, validation strategy, security considerations, or other execution-relevant content, re-run Phase 4.5 before continuing toward `READY`.
   - After any required re-run of Phase 4.5, present the updated plan summary again in Phase 5 and continue from the current review/handoff flow on that updated version.
   - If the user requests more changes or more review, keep iterating in Phase 5.
   - Do NOT imply the plan is `READY` until the user explicitly accepts the final version.

5. Ask about execution flags using **two** AskUserQuestion calls:

   **Question 1** (multiSelect: true):
   - Question: "Which execution flags should be enabled?"
   - Options:
     - label: "no-worktree", description: "Skip creating a git worktree (run directly in the current working directory)"
     - label: "no-pr", description: "Skip creating a pull request after implementation"
     - label: "draft-pr", description: "Create a draft PR instead of a ready-for-review PR"
     - label: "None of these", description: "No execution flags needed"

   -- Skip the second qeustion if "no-pr" or "draft-pr" are enabled. --

   **Question 2** (multiSelect: false):
   - Question: "Do you want to enable auto-merge?"
   - Options:
     - label: "merge", description: "Merge after the PR has been finalized"
     - label: "merge-admin", description: "Merge with --admin after the PR has been finalized"
     - label: "None of these", description: "No auto-merge needed"

   - Treat `None of these` deterministically: ignore it whenever any real flag is also selected, and only treat it as meaningful when it is the sole selection.
   - For each selected real flag, update the plan header by replacing `**<flag>:** [ ]` with `**<flag>:** [x]`.
   - Leave unselected flags as `[ ]`.

6. If unresolved automated-review issues remain and the user is moving toward `READY`, ask a separate explicit acknowledgment question before the final explicit acceptance step.
   - Summarize the remaining issues concretely.
   - Ask: "These review issues are still open. Do you explicitly accept them for this version of the plan?"
   - Options: "Accept remaining issues", "Make changes", "I need to review it"
   - If the user does not explicitly choose "Accept remaining issues", keep the plan `WIP` and do not present it as ready for execution.

7. After the flag selections have been applied to the plan header, present the final version summary that will actually be saved and ask for explicit acceptance of that exact final version.
   - This confirmation MUST happen after any requested edits, after any required unresolved-review acknowledgment, and after execution flags are reflected in the document.
   - Options: "Looks good", "Make changes", "I need to review it"
   - If the user does not explicitly accept this final flagged version, keep the plan `WIP`.

8. Only after the user explicitly accepts the final version of the plan in Phase 5 (for example by choosing "Looks good" in step 7), update the plan document header by replacing `**Status:** WIP` with `**Status:** READY`.

9. Final handoff message must be status-aware:
   - If the plan was promoted to `READY`, tell the user: "Plan saved to `.my/plans/<filename>`. Run `/my:execute-plan .my/plans/<filename>` to implement it. Add execution flags such as `--non-interactive` when that execution mode is intended."
   - If the plan remains `WIP` because the user has not explicitly accepted the final version yet or because unresolved automated review issues still need user resolution, tell the user the plan was saved at `.my/plans/<filename>`, summarize why it is still `WIP`, and ask them to review or resolve those issues before running `/my:execute-plan`.
