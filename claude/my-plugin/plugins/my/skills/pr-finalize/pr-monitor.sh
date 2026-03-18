#!/usr/bin/env bash
set -euo pipefail

# Arguments:
#   --owner <owner>        GitHub repo owner
#   --repo <repo>          GitHub repo name
#   --pr <number>          PR number
#   --head-sha <sha>       Initial HEAD SHA
#   --push-time <iso>      Initial push timestamp
#   --workdir <path>       Working directory (repo root or worktree)
#   --summary-file <path>  Path to write the fix summary
#   --log-file <path>      Path to write formatted Claude output log

POLL_INTERVAL=15
MAX_CONSECUTIVE_CLEAN=3   # require N consecutive clean polls before declaring done
MIN_BUGBOT_WAIT_SECS=360  # wait at least 6 min after last push before counting clean polls
                          # (CI takes ~2min, Bugbot analysis takes ~2-3min after CI)
MAX_INFRA_RETRIES=3       # max times to re-run a failing infra check before giving up on it
MAX_FIX_SESSIONS=20       # max fix sessions before giving up (prevents infinite loops)

OWNER=""
REPO=""
PR_NUMBER=""
HEAD_SHA=""
PUSH_TIME=""
WORKDIR=""
SUMMARY_FILE=""
LOG_FILE=""
LAST_PUSH_EPOCH=0  # epoch seconds of the last fix push; 0 = no fix pushed yet

# Temp file used to pass infra-failure JSON from collect_issues → main loop
INFRA_TMP=""

# Check name substrings that identify infrastructure-only failures.
# These are NOT code problems — they need a CI re-run or org-level config fix,
# NOT a Claude fix session. Wiz-cli 401 = credential timeout (flaky).
# SonarCloud cancelled = Automatic Analysis config (org-level, can't fix from code).
INFRA_CHECK_PATTERNS=(
    "Wiz-cli Scan"
    "Wiz Scan"
    "WizScan"
    "wiz-scan"
    "trivy"
    "Trivy"
)

# Returns 0 if check name matches an infra pattern, 1 otherwise
is_infra_check() {
    local name="$1"
    for pattern in "${INFRA_CHECK_PATTERNS[@]}"; do
        if [[ "$name" == *"$pattern"* ]]; then
            return 0
        fi
    done
    return 1
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --owner) OWNER="$2"; shift 2 ;;
            --repo) REPO="$2"; shift 2 ;;
            --pr) PR_NUMBER="$2"; shift 2 ;;
            --head-sha) HEAD_SHA="$2"; shift 2 ;;
            --push-time) PUSH_TIME="$2"; shift 2 ;;
            --workdir) WORKDIR="$2"; shift 2 ;;
            --summary-file) SUMMARY_FILE="$2"; shift 2 ;;
            --log-file) LOG_FILE="$2"; shift 2 ;;
            *) echo "Unknown argument: $1" >&2; exit 1 ;;
        esac
    done

    local missing=()
    [[ -z "$OWNER" ]] && missing+=("--owner")
    [[ -z "$REPO" ]] && missing+=("--repo")
    [[ -z "$PR_NUMBER" ]] && missing+=("--pr")
    [[ -z "$HEAD_SHA" ]] && missing+=("--head-sha")
    [[ -z "$PUSH_TIME" ]] && missing+=("--push-time")
    [[ -z "$WORKDIR" ]] && missing+=("--workdir")
    [[ -z "$SUMMARY_FILE" ]] && missing+=("--summary-file")
    [[ -z "$LOG_FILE" ]] && missing+=("--log-file")

    if [[ ${#missing[@]} -gt 0 ]]; then
        echo "Missing required arguments: ${missing[*]}" >&2
        exit 1
    fi

    if ! [[ "$PR_NUMBER" =~ ^[0-9]+$ ]]; then
        echo "PR number must be numeric, got: $PR_NUMBER" >&2
        exit 1
    fi
}

# ─── Check-run queries ────────────────────────────────────────────────────────

# All failed check-runs for HEAD_SHA (failure OR cancelled), classified.
# Outputs JSON: {code: [...], infra: [...]}
# Uses --paginate to ensure ALL check-runs are returned (not just first 30).
# If the API call fails, returns "ERROR" so callers can treat it as PENDING.
get_classified_failures() {
    local all_failed
    # NOTE: avoid jq capture() — it silently drops array elements when the regex
    # doesn't match (returns null mid-pipeline). Use test() + ltrimstr() instead.
    if ! all_failed=$(gh api "repos/${OWNER}/${REPO}/commits/${HEAD_SHA}/check-runs" \
        --paginate \
        --jq '[.check_runs[] | select(
            .status == "completed"
            and .conclusion != "success"
            and .conclusion != "neutral"
            and .conclusion != "skipped"
        ) | {
            name: .name,
            conclusion: .conclusion,
            details_url: (.details_url // ""),
            run_id: ((.details_url // "") | if test("actions/runs/[0-9]+") then (split("actions/runs/")[1] | split("/")[0]) else "" end)
        }]' 2>/dev/null); then
        echo "ERROR"
        return 1
    fi
    # Deduplicate: for each check name, keep only the entry with the "worst" conclusion.
    # After a re-run, GitHub returns BOTH the old (failure) and new (success) check-runs.
    # We want the latest result, so filter: if a check has ANY success, exclude its failures.
    local success_names
    success_names=$(gh api "repos/${OWNER}/${REPO}/commits/${HEAD_SHA}/check-runs" \
        --paginate \
        --jq '[.check_runs[] | select(.status == "completed" and .conclusion == "success") | .name]' 2>/dev/null || echo "[]")
    all_failed=$(echo "$all_failed" | jq --argjson successes "$success_names" \
        '[.[] | select(.name as $n | $successes | index($n) | not)]')

    # Split into code vs infra failures using bash pattern matching
    local code_failures="[]"
    local infra_failures="[]"

    # Process each check through is_infra_check
    local count
    count=$(echo "$all_failed" | jq 'length')
    for i in $(seq 0 $((count - 1))); do
        local entry name
        entry=$(echo "$all_failed" | jq ".[$i]")
        name=$(echo "$entry" | jq -r '.name')
        if is_infra_check "$name"; then
            infra_failures=$(echo "$infra_failures" | jq --argjson e "$entry" '. + [$e]')
        else
            code_failures=$(echo "$code_failures" | jq --argjson e "$entry" '. + [$e]')
        fi
    done

    jq -n --argjson code "$code_failures" --argjson infra "$infra_failures" \
        '{code: $code, infra: $infra}'
}

get_new_bugbot_comments() {
    gh api "repos/${OWNER}/${REPO}/pulls/${PR_NUMBER}/comments" \
        --jq "[.[] | select(.user.login == \"cursor[bot]\" and .created_at > \"${PUSH_TIME}\") | {id: .id, path: .path, body: .body}]"
}

get_unresolved_threads() {
    gh api graphql -f query="
        { repository(owner: \"${OWNER}\", name: \"${REPO}\") {
            pullRequest(number: ${PR_NUMBER}) {
                reviewThreads(first: 100) {
                    nodes {
                        id
                        isResolved
                        comments(first: 1) {
                            nodes { author { login } body }
                        }
                    }
                }
            }
        }}" --jq '[.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved == false) | {id: .id, author: .comments.nodes[0].author.login, body: .comments.nodes[0].body}]'
}

get_pending_checks() {
    gh api "repos/${OWNER}/${REPO}/commits/${HEAD_SHA}/check-runs" \
        --jq '[.check_runs[] | select(.status != "completed") | {name: .name, status: .status}]'
}

# Check if Bugbot has completed its run on HEAD_SHA.
# Returns "complete" / "pending" / "unknown"
get_bugbot_status() {
    local status conclusion
    status=$(gh api "repos/${OWNER}/${REPO}/commits/${HEAD_SHA}/check-runs" \
        --jq '.check_runs[] | select(.app.slug == "cursor" or (.name | test("Bugbot|bugbot|cursor"; "i"))) | .status' 2>/dev/null | head -1)
    conclusion=$(gh api "repos/${OWNER}/${REPO}/commits/${HEAD_SHA}/check-runs" \
        --jq '.check_runs[] | select(.app.slug == "cursor" or (.name | test("Bugbot|bugbot|cursor"; "i"))) | .conclusion' 2>/dev/null | head -1)

    if [[ -z "$status" ]]; then
        echo "unknown"
    elif [[ "$status" == "completed" ]]; then
        echo "complete:${conclusion:-unknown}"
    else
        echo "pending"
    fi
}

# ─── Infra check retry ────────────────────────────────────────────────────────

# Attempt to re-run failed infra checks via `gh run rerun`.
# Takes the infra_failures JSON array as argument.
retry_infra_checks() {
    local infra_failures="$1"
    local count
    count=$(echo "$infra_failures" | jq 'length')
    if [[ "$count" -eq 0 ]]; then return; fi

    echo "[$(date +%H:%M:%S)] Retrying $count infrastructure check(s) (NOT launching fix session — these are not code issues)"

    for i in $(seq 0 $((count - 1))); do
        local name run_id conclusion
        name=$(echo "$infra_failures" | jq -r ".[$i].name")
        run_id=$(echo "$infra_failures" | jq -r ".[$i].run_id // empty")
        conclusion=$(echo "$infra_failures" | jq -r ".[$i].conclusion")

        echo "[$(date +%H:%M:%S)]   → '${name}' (${conclusion}): run_id=${run_id:-unknown}"

        if [[ -n "$run_id" ]]; then
            if gh run rerun "$run_id" --failed 2>/dev/null; then
                echo "[$(date +%H:%M:%S)]   ✓ Re-queued run $run_id"
            else
                echo "[$(date +%H:%M:%S)]   ✗ Could not re-run job $run_id (may need manual retry)"
            fi
        else
            echo "[$(date +%H:%M:%S)]   ✗ No run_id found for '${name}' — cannot auto-retry"
        fi
    done
}

# ─── Issue collection ─────────────────────────────────────────────────────────

# Returns one of:
#   "PENDING"         — checks still running, no actionable issues yet
#   "INFRA_ONLY"      — only infra failures; infra JSON written to $INFRA_TMP
#   ""  (empty)       — all checks clean
#   JSON object       — actionable code issues for Claude to fix
collect_issues() {
    local classified new_comments unresolved_threads pending_checks
    local bugbot_status merge_conflicts

    classified=$(get_classified_failures) || classified="ERROR"
    new_comments=$(get_new_bugbot_comments) || new_comments="[]"
    unresolved_threads=$(get_unresolved_threads) || unresolved_threads="[]"
    pending_checks=$(get_pending_checks) || pending_checks="[]"
    bugbot_status=$(get_bugbot_status) || bugbot_status="unknown"

    # Check for merge conflicts
    local mergeable base_branch
    mergeable=$(gh api "repos/${OWNER}/${REPO}/pulls/${PR_NUMBER}" --jq '.mergeable' 2>/dev/null || echo "unknown")
    if [[ "$mergeable" == "CONFLICTING" ]]; then
        base_branch=$(gh api "repos/${OWNER}/${REPO}/pulls/${PR_NUMBER}" --jq '.base.ref' 2>/dev/null || echo "main")
        merge_conflicts="{\"conflicting\": true, \"base_branch\": \"${base_branch}\"}"
        echo "[$(date +%H:%M:%S)] ⚠ PR has merge conflicts with ${base_branch}"
    else
        merge_conflicts="{\"conflicting\": false}"
    fi

    # If the check-runs API failed, treat as PENDING (don't false-declare clean)
    if [[ "$classified" == "ERROR" ]]; then
        echo "PENDING"
        return
    fi

    local code_failures infra_failures
    code_failures=$(echo "$classified" | jq '.code // []')
    infra_failures=$(echo "$classified" | jq '.infra // []')

    local has_code has_comments has_threads has_pending has_infra has_conflicts bugbot_pending
    has_code=$(echo "$code_failures" | jq 'length > 0')
    has_comments=$(echo "$new_comments" | jq 'length > 0')
    has_threads=$(echo "$unresolved_threads" | jq 'length > 0')
    has_pending=$(echo "$pending_checks" | jq 'length > 0')
    has_infra=$(echo "$infra_failures" | jq 'length > 0')
    has_conflicts=$(echo "$merge_conflicts" | jq '.conflicting')

    # "unknown" means no Bugbot app is installed on this repo — don't block on it
    if [[ "$bugbot_status" == "pending" ]]; then
        bugbot_pending=true
    else
        bugbot_pending=false
    fi

    # Still waiting for checks or Bugbot to complete
    if [[ "$has_pending" == "true" || "$bugbot_pending" == "true" ]]; then
        echo "PENDING"
        return
    fi

    # Save infra failures to temp file for main loop to read
    if [[ "$has_infra" == "true" && -n "$INFRA_TMP" ]]; then
        echo "$infra_failures" > "$INFRA_TMP"
    fi

    # Actionable code issues for Claude to fix
    if [[ "$has_code" == "true" || "$has_comments" == "true" || "$has_threads" == "true" || "$has_conflicts" == "true" ]]; then
        jq -n \
            --argjson failed "$code_failures" \
            --argjson comments "$new_comments" \
            --argjson threads "$unresolved_threads" \
            --argjson conflicts "$merge_conflicts" \
            '{failed_checks: $failed, new_bugbot_comments: $comments, unresolved_threads: $threads, merge_conflicts: $conflicts}'
        return
    fi

    # No code issues — but infra failures may need retry
    if [[ "$has_infra" == "true" ]]; then
        echo "INFRA_ONLY"
        return
    fi

    # Truly clean — empty output
}

# ─── Fix session ──────────────────────────────────────────────────────────────

format_claude_stream() {
    local log_file="$1"
    local fix_num="$2"

    echo "--- Fix Session #${fix_num} ---" >> "$log_file"

    while IFS= read -r line; do
        local type
        type=$(echo "$line" | jq -r '.type // empty' 2>/dev/null) || continue

        case "$type" in
            assistant)
                local text
                text=$(echo "$line" | jq -r '.message.content[]? | select(.type == "text") | .text // empty' 2>/dev/null)
                if [[ -n "$text" ]]; then
                    local truncated
                    truncated=$(echo "$text" | head -5)
                    echo "$truncated" | sed 's/^/  /'
                    echo "$text" >> "$log_file"
                fi
                ;;
            tool_use)
                local tool_name
                tool_name=$(echo "$line" | jq -r '.tool_name // empty' 2>/dev/null)
                echo "  [tool] $tool_name"
                echo "[tool] $tool_name" >> "$log_file"
                ;;
            result)
                local cost_usd duration_ms
                cost_usd=$(echo "$line" | jq -r '.cost_usd // "?"' 2>/dev/null)
                duration_ms=$(echo "$line" | jq -r '.duration_ms // "?"' 2>/dev/null)
                local duration_s
                if [[ "$duration_ms" != "?" ]]; then
                    duration_s=$(echo "$duration_ms" | awk '{printf "%d", $1/1000}')
                else
                    duration_s="?"
                fi
                echo "  [done] Cost: \$${cost_usd} | Duration: ${duration_s}s"
                echo "[done] Cost: \$${cost_usd} | Duration: ${duration_s}s" >> "$log_file"
                ;;
        esac
    done

    echo "" >> "$log_file"
}

build_fix_prompt() {
    local issues="$1"

    local failed_checks new_comments unresolved_threads merge_conflicts
    failed_checks=$(echo "$issues" | jq -r '.failed_checks // []')
    new_comments=$(echo "$issues" | jq -r '.new_bugbot_comments // []')
    unresolved_threads=$(echo "$issues" | jq -r '.unresolved_threads // []')
    merge_conflicts=$(echo "$issues" | jq -r '.merge_conflicts // {"conflicting": false}')

    cat <<PROMPT
/my:pr-finalize --fix

PR: ${OWNER}/${REPO}#${PR_NUMBER}
HEAD SHA: ${HEAD_SHA}
Working directory: ${WORKDIR}

The following issues were detected on this PR. Fix ALL of them, run tests locally, push the fixes, and resolve any review threads you addressed.

Merge conflicts:
${merge_conflicts}

Failed checks (these are code failures, not infrastructure issues):
${failed_checks}

New Bugbot comments (since last push):
${new_comments}

Unresolved review threads:
${unresolved_threads}
PROMPT
}

launch_fix_session() {
    local issues="$1"
    local fix_num="$2"

    local prompt
    prompt=$(build_fix_prompt "$issues")

    echo ""
    echo "=========================================="
    echo " Fix Session #${fix_num}"
    echo "=========================================="

    {
        echo "## Fix #${fix_num}"
        echo ""
        echo "**Time:** $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo ""
        echo "**Issues:**"
        echo '```json'
        echo "$issues" | jq .
        echo '```'
        echo ""
    } >> "$SUMMARY_FILE"

    local exit_code=0
    claude -p "$prompt" \
        --output-format stream-json \
        --dangerously-skip-permissions \
        --verbose 2>/dev/null \
    | format_claude_stream "$LOG_FILE" "$fix_num" \
    || exit_code=$?

    if [[ $exit_code -eq 0 ]]; then
        echo "  [result] Fix session #${fix_num} succeeded"
        echo "**Result:** Success" >> "$SUMMARY_FILE"
    else
        echo "  [result] Fix session #${fix_num} failed (exit code: $exit_code)"
        echo "**Result:** Failed (exit code: $exit_code)" >> "$SUMMARY_FILE"
    fi
    echo "" >> "$SUMMARY_FILE"

    return 0
}

finalize_summary() {
    local total_fixes="$1"
    local status="${2:-All checks passing}"
    {
        echo "---"
        echo ""
        echo "## Result"
        echo ""
        echo "**Status:** ${status}"
        echo "**Total fix sessions:** ${total_fixes}"
        echo "**Completed:** $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    } >> "$SUMMARY_FILE"
}

# ─── Main loop ────────────────────────────────────────────────────────────────

main() {
    parse_args "$@"
    local consecutive_clean=0
    local fix_count=0
    local infra_retry_count=0  # how many times we've tried to re-run infra checks
    local seen_thread_ids=""   # space-separated list of thread IDs already attempted

    # Temp file for infra failure JSON passed from collect_issues
    INFRA_TMP=$(mktemp /tmp/pr-monitor-infra-XXXXX.json)
    trap 'rm -f "$INFRA_TMP"' EXIT

    echo "=== PR Monitor started ==="
    echo "  PR: ${OWNER}/${REPO}#${PR_NUMBER}"
    echo "  Polling every ${POLL_INTERVAL}s"
    echo "  Summary: ${SUMMARY_FILE}"
    echo "  Log: ${LOG_FILE}"
    echo ""

    {
        echo "# PR Finalize Summary"
        echo ""
        echo "PR: ${OWNER}/${REPO}#${PR_NUMBER}"
        echo "Started: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo ""
    } > "$SUMMARY_FILE"

    while true; do
        local issues infra_json
        echo "[]" > "$INFRA_TMP"  # reset each poll
        issues=$(collect_issues) || issues=""
        infra_json=$(cat "$INFRA_TMP" 2>/dev/null) || infra_json="[]"

        if [[ "$issues" == "PENDING" ]]; then
            consecutive_clean=0
            echo "[$(date +%H:%M:%S)] Checks still running (or Bugbot pending), waiting..."

        elif [[ "$issues" == "INFRA_ONLY" ]]; then
            # Only infra failures remain — no code issues, no bugbot threads
            consecutive_clean=0
            local infra_names
            infra_names=$(echo "$infra_json" | jq -r '[.[].name] | join(", ")' 2>/dev/null || echo "unknown")

            if [[ "$infra_retry_count" -lt "$MAX_INFRA_RETRIES" ]]; then
                infra_retry_count=$((infra_retry_count + 1))
                echo "[$(date +%H:%M:%S)] Only infrastructure failures remain: ${infra_names}"
                echo "[$(date +%H:%M:%S)] Attempting re-run (retry $infra_retry_count/$MAX_INFRA_RETRIES)..."
                retry_infra_checks "$infra_json"
                sleep 30  # give re-run time to queue
            else
                # Exhausted retries — exit with a clear report
                echo ""
                echo "╔══════════════════════════════════════════════════════════════╗"
                echo "║  PR FINALIZATION CANNOT COMPLETE — INFRASTRUCTURE FAILURES   ║"
                echo "╚══════════════════════════════════════════════════════════════╝"
                echo ""
                echo "The following checks are failing due to INFRASTRUCTURE issues,"
                echo "NOT code problems. Claude cannot fix these from code changes."
                echo ""
                echo "Failing infrastructure checks (after $infra_retry_count re-run attempts):"
                echo "$infra_json" | jq -r '.[] | "  • \(.name) [\(.conclusion)]"' 2>/dev/null || echo "  • ${infra_names}"
                echo ""
                echo "Known root causes and required manual actions:"
                echo "  Wiz-cli Scan 401    → WIZ_CLIENT_ID / WIZ_CLIENT_SECRET credentials in"
                echo "    GitHub org secrets are expired or invalid. Rotate them."
                echo ""
                echo "✅  All CODE checks pass."
                echo "✅  All Bugbot review threads resolved."
                echo "✅  The PR code itself is ready to merge."
                echo "❌  Only infrastructure configuration blocks finalization."
                echo ""
                finalize_summary "$fix_count" "BLOCKED — infrastructure failures require manual action (see output)"
                exit 2
            fi

        elif [[ -z "$issues" ]]; then
            # Truly clean — no code failures, no infra failures, no bugbot issues
            # Enforce minimum wait after a push before counting clean polls
            local now elapsed
            now=$(date +%s)
            if [[ $LAST_PUSH_EPOCH -gt 0 ]]; then
                elapsed=$((now - LAST_PUSH_EPOCH))
                if [[ $elapsed -lt $MIN_BUGBOT_WAIT_SECS ]]; then
                    local remaining=$((MIN_BUGBOT_WAIT_SECS - elapsed))
                    echo "[$(date +%H:%M:%S)] Checks clean — waiting for Bugbot analysis (${elapsed}s elapsed, ~${remaining}s remaining)..."
                    consecutive_clean=0
                    sleep "$POLL_INTERVAL"
                    continue
                fi
            fi
            consecutive_clean=$((consecutive_clean + 1))
            echo "[$(date +%H:%M:%S)] Clean poll ($consecutive_clean/$MAX_CONSECUTIVE_CLEAN)"
            if [[ $consecutive_clean -ge $MAX_CONSECUTIVE_CLEAN ]]; then
                echo "[$(date +%H:%M:%S)] All checks green. Done."
                finalize_summary "$fix_count"
                exit 0
            fi

        else
            # Actionable code issues — check if all threads are already-seen (no-progress loop guard)
            consecutive_clean=0

            # Extract current thread IDs from issues
            local current_thread_ids
            current_thread_ids=$(echo "$issues" | jq -r '[.unresolved_threads[]?.id // empty] | join(" ")' 2>/dev/null || echo "")
            local has_new_threads=false
            for tid in $current_thread_ids; do
                if [[ " $seen_thread_ids " != *" $tid "* ]]; then
                    has_new_threads=true
                    break
                fi
            done

            # Also check for failed checks or new bugbot comments (those are always actionable)
            local has_code_issues
            has_code_issues=$(echo "$issues" | jq '(.failed_checks | length > 0) or (.new_bugbot_comments | length > 0)' 2>/dev/null || echo "false")

            if [[ "$has_new_threads" == "false" && "$has_code_issues" == "false" && "$has_conflicts" == "false" ]]; then
                # All remaining issues are threads we've already attempted — avoid infinite loop
                echo "[$(date +%H:%M:%S)] All remaining unresolved threads were already attempted. Treating as done."
                echo "[$(date +%H:%M:%S)] Unresolved thread IDs: ${current_thread_ids:-none}"
                finalize_summary "$fix_count" "All checks passing (some review threads may require manual resolution)"
                exit 0
            fi

            # Enforce max fix sessions
            if [[ $fix_count -ge $MAX_FIX_SESSIONS ]]; then
                echo ""
                echo "╔══════════════════════════════════════════════════════════════╗"
                echo "║  PR FINALIZATION STOPPED — MAX FIX SESSIONS REACHED          ║"
                echo "╚══════════════════════════════════════════════════════════════╝"
                echo ""
                echo "Reached maximum of ${MAX_FIX_SESSIONS} fix sessions without converging."
                echo "Remaining issues:"
                echo "$issues" | jq .
                finalize_summary "$fix_count" "BLOCKED — max fix sessions (${MAX_FIX_SESSIONS}) reached without converging"
                exit 3
            fi

            fix_count=$((fix_count + 1))
            echo "[$(date +%H:%M:%S)] Issues found — launching fix session #${fix_count}"
            launch_fix_session "$issues" "$fix_count"

            # Record thread IDs we just attempted so we don't re-trigger on them
            for tid in $current_thread_ids; do
                if [[ " $seen_thread_ids " != *" $tid "* ]]; then
                    seen_thread_ids="$seen_thread_ids $tid"
                fi
            done

            # Update HEAD SHA and push timestamp after fix
            local new_sha
            new_sha=$(gh api "repos/${OWNER}/${REPO}/pulls/${PR_NUMBER}" --jq '.head.sha') || new_sha="$HEAD_SHA"
            if [[ "$new_sha" != "$HEAD_SHA" ]]; then
                HEAD_SHA="$new_sha"
                PUSH_TIME=$(date -u +%Y-%m-%dT%H:%M:%SZ)
                LAST_PUSH_EPOCH=$(date +%s)
                infra_retry_count=0  # reset infra retries after a code push
                # Reset seen threads when code changes — new threads may be different
                seen_thread_ids=""
                echo "[$(date +%H:%M:%S)] Fix pushed. Will wait ${MIN_BUGBOT_WAIT_SECS}s for Bugbot before counting clean polls."
            fi
        fi

        sleep "$POLL_INTERVAL"
    done
}

main "$@"
