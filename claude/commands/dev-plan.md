---
description: Create a development plan document interactively
allowed-tools: AskUserQuestion, Write, Bash(code:*), Bash(cursor:*), mcp__atlassian__*, Bash(open:*), Skill
---

# Create Development Plan Document

You are helping the user create a development plan document for implementing a change. This document will outline the detailed plan for the required code changes including code examples. It does not include the exact code changes but instructions to implement them.

**IMPORTANT**: While creating the plan you ARE NOT ALLOWED to change the code or start to implement the solution. This is about creating the plan and nothing else!

Follow these steps interactively:

## Step 1: JIRA Ticket

Use the AskUserQuestion tool to ask about JIRA (IMPORTANT: Ask the exact question below!):
- Question: "Do you have an existing JIRA ticket for this work?"
- Options:
  1. "Yes, I have a ticket number" - then ask for the ticket number
  2. "No, create a new ticket later" - then create a new JIRA ticket in step 4 using the gathered information
  3. "No ticket needed" - proceed without JIRA integration

If a ticket has been provided, read it.

## Step 2: Gather High-Level Description

IMPORTANT: Ask the exact question below! There is no options, so you could ask the question and let the user type.
- Question: "What would you like to implement? Please provide a high-level description of the feature or task."
- Suggest a high-level description from the JIRA ticket context.
- The user should not be able to skip this.
- This should be an open-ended text input (use a simple option that prompts for details)

## Step 3: Gather Context and Instructions

Use the AskUserQuestion tool to ask (IMPORTANT: Ask the exact question below!):
- Question: "Please provide any specific context, instructions, or research/investigations that should be performed. Include any relevant links, dependencies, or technical constraints."
- This is for additional context that will guide the implementation
- The user should always have a way to put in more context, even if you make assumptions.
- The user will have two options:
  1. Enter more context - this should directly open an open-ended text input
  2. Skip

## Step 4: Create JIRA Ticket

If the user provided no ticket in step 1 and has chooses "2. create a new ticket later" you have to create a new ticket:
- Use the mcp__atlassian__createJiraIssue tool
- Use cloudId "parloa.atlassian.net"
- Ask which project to use if not clear
- Use the high-level description as the summary
- Use the context/instructions as the description

## Step 5: Load Skills (MANDATORY - DO NOT SKIP)

**CRITICAL REQUIREMENT**: You MUST use the Skill tool to load ALL of the following skills BEFORE proceeding. This is NON-NEGOTIABLE.

Load these skills IN ORDER using the Skill tool:

```
Skill(typescript-services:true-myth-recipe)
Skill(typescript-services:production-code-recipe)
Skill(typescript-services:test-code-recipe)
Skill(workflows:worktree-recipe)
```

### Verification Checklist:

Before proceeding to Step 6, confirm you have loaded ALL skills!

**FAILURE TO LOAD ALL SKILLS IS UNACCEPTABLE. DO NOT PROCEED TO STEP 5 UNTIL ALL SKILLS ARE LOADED.**

Include references to these skills in the implementation steps of the plan document (e.g., "Follow `typescript-services:production-code-recipe` for functional patterns").

## Step 6: Create the Plan Document

Create an org-mode document in the project root names plan-[name].org with the following structure:

### File Location and Name

Put the file into the projects root folder. *IMPORTANT*: Do not create any subdirectories!

The file must be named using the following scheme: plan-[name].org

Include the ticket number in the plan if available.

### Structure

All investigations must happen before creating the plan so that the plan includes the details for implementing the change. Also read the JIRA ticket (ONLY if available) for additional context.

**IMPORTANT:** Use org-mode src blocks for code.

The plan must have the following structure:

```org
#+TITLE: Development Plan: [Brief Title from Description]
#+DATE: [Current Date]
#+JIRA: [JIRA-XXX or "To be created at implementation time"]

* Overview
[High-level description from Step 1]

* Context & Background
[Context and instructions from Step 2]

* Research & Investigation
** Tasks
- [ ] [List any research tasks mentioned and their outcome]

* Implementation Plan

*IMPORTANT:** Load the workflow:worktree-recipe skill and use git-worktrees. Use ../wt-[repo]-[jira-ticket]-[name of the plan] as path. DO NOT work on the repository itself.

** Phase 1: High level changes
- [ ] Architectural description
- [ ] List affected and new components
- [ ] Document dependencies

** Phase 2: Design
- [ ] Define technical approach
- [ ] Create sequence diagrams if needed - (use plain text with horizontal sequence arrows, NOT plantuml)
- [ ] Code

** Phase 3: Implementation
Follow =typescript-services:production-code-recipe= for functional pattern, =typescript-services:true-myth= for Maybe, Result, and Task types.

- [ ] [Add implementation tasks based on the description]

** Phase 4: Testing
Follow =typescript-services:test-code-recipe= for test patterns.

- [ ] Unit tests
- [ ] Integration tests
- [ ] Manual testing

** Phase 5: Documentation & Deployment
- [ ] Update documentation

* Final Steps
- [ ] Run the code formatter
- [ ] Run the build
- [ ] Run the linter
- [ ] Create JIRA ticket (ONLY if no ticket exists yet)
- [ ] Create draft PR

* Open Questions
- [List any questions that arose during planning]

* Notes
[Any additional notes]
```

Save the file as `plan-[name].org`.

## Step 7: Open in IDE

After creating the file, open it in the user's IDE:
- run `open <file>`
- or run `$EDITOR <file> &` as fallback

Report the location of the created file to the user.
