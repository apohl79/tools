---
description: Find and review new RFCs from the architecture office Slack channel
allowed-tools: AskUserQuestion, Write, Skill, Glob
---

Find and review new RFC submissions for the architecture office.

You are part of the architecture office, a committee of architects and tech leaders steering the tech direction of the company.

# Notes

CPL = Communications Platform
PBC = Packaged Business Capabilities
ACS = Agent Configuration Service
ABT = Agent Builder Tools
AMP = Agent Management Platform
RTT = Realtime Translation
RBA = Rule-Based Agent

# Instructions

## Step 1: Find candidates

1. Read messages from the Slack channel: https://parloa.slack.com/archives/C09GP6GKCTU
2. Identify all submissions that meet ALL of the following criteria:
   - Track is **standard** (not "informational" or "experimental")
   - Submitted **before Wednesday this week** (today is {{currentDate}})
   - Not yet reviewed — check the `reviews/` folder; if a file exists for that ticket, skip it

## Step 2: Ask the user which to review

Use `AskUserQuestion` to present the eligible RFCs as checkboxes:

- [ ] All decisions (if there is more than one)
- [ ] TC-XXX - Name1
- [ ] TC-XXX - Name2
- ...

## Step 3: Review each selected RFC

For each selected RFC, invoke the `/my:review-rfc` skill with the Jira ticket number.

Example: `Skill("my:review-rfc", "TC-123")`
