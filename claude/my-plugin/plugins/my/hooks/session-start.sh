#!/usr/bin/env bash

cat <<'EOF'
{
  "hookSpecificOutput": {
    "hookEventName": "SessionStart",
    "additionalContext": "MANDATORY WORKFLOW RULE: After any successful superpowers:writing-plans run that writes a plan under docs/superpowers/plans/, you MUST immediately invoke my:superpowers-plan-handover with that exact plan path. This step is REQUIRED and MUST NOT be skipped, deferred, bypassed, or replaced with the default Superpowers execution-choice handoff. Once the Superpowers plan file exists, the migrated .my/plans/ file becomes the ONLY source of truth for all execution choices and execution handoff. You MUST NOT continue with the default Superpowers execution-choice flow after plan creation."
  }
}
EOF

exit 0
