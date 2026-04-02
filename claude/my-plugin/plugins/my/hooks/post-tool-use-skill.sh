#!/usr/bin/env bash
# PostToolUse hook: fires after each Skill tool call.
# When superpowers:writing-plans was invoked, injects a mandatory reminder
# that my:superpowers-plan-handover MUST be run after the plan is written.

set -euo pipefail

SKILL=$(cat | jq -r '.tool_input.skill // empty' 2>/dev/null)

if [[ "$SKILL" != *"writing-plans"* ]]; then
  exit 0
fi

cat <<'EOF'
{
  "hookSpecificOutput": {
    "hookEventName": "PostToolUse",
    "additionalContext": "MANDATORY NEXT STEP: superpowers:writing-plans was just invoked. Once the plan has been written to docs/superpowers/plans/, you MUST immediately invoke my:superpowers-plan-handover with that exact plan path. This step is REQUIRED and MUST NOT be skipped, deferred, bypassed, or replaced with the default Superpowers execution-choice handoff. Do NOT present execution choices or continue with the default Superpowers flow — hand over to my:superpowers-plan-handover instead."
  }
}
EOF

exit 0
