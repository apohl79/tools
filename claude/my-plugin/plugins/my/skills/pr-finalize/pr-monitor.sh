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
MAX_CONSECUTIVE_CLEAN=3  # require N consecutive clean polls before declaring done

OWNER=""
REPO=""
PR_NUMBER=""
HEAD_SHA=""
PUSH_TIME=""
WORKDIR=""
SUMMARY_FILE=""
LOG_FILE=""

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

    # Validate required args
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

# Poll functions
get_failed_checks() {
    gh api "repos/${OWNER}/${REPO}/commits/${HEAD_SHA}/check-runs" \
        --jq '[.check_runs[] | select(.status == "completed" and .conclusion != "success" and .conclusion != "neutral" and .conclusion != "skipped") | {name: .name, conclusion: .conclusion, details_url: .details_url}]'
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

# Collect and aggregate all issues
collect_issues() {
    local failed_checks new_comments unresolved_threads pending_checks

    failed_checks=$(get_failed_checks) || failed_checks="[]"
    new_comments=$(get_new_bugbot_comments) || new_comments="[]"
    unresolved_threads=$(get_unresolved_threads) || unresolved_threads="[]"
    pending_checks=$(get_pending_checks) || pending_checks="[]"

    local has_failed has_comments has_threads has_pending
    has_failed=$(echo "$failed_checks" | jq 'length > 0')
    has_comments=$(echo "$new_comments" | jq 'length > 0')
    has_threads=$(echo "$unresolved_threads" | jq 'length > 0')
    has_pending=$(echo "$pending_checks" | jq 'length > 0')

    # If checks are still pending, don't trigger a fix yet but don't count as clean
    if [[ "$has_pending" == "true" && "$has_failed" == "false" && "$has_comments" == "false" && "$has_threads" == "false" ]]; then
        echo "PENDING"
        return
    fi

    if [[ "$has_failed" == "true" || "$has_comments" == "true" || "$has_threads" == "true" ]]; then
        jq -n \
            --argjson failed "$failed_checks" \
            --argjson comments "$new_comments" \
            --argjson threads "$unresolved_threads" \
            '{failed_checks: $failed, new_bugbot_comments: $comments, unresolved_threads: $threads}'
    fi
}

# Format Claude's stream-json output for human-readable terminal display.
# Reads from stdin, writes formatted output to stdout and appends to log file.
#
# Arguments:
#   $1 - log_file: path to append formatted output
#   $2 - fix_num: fix session number for labeling
#
# Stream-json event types:
#   assistant  — Claude's text responses (show as-is, max 5 lines per chunk)
#   tool_use   — tool being called (show tool name)
#   tool_result — tool result (skip for brevity)
#   result     — final result with cost_usd and duration_ms
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

    local failed_checks new_comments unresolved_threads
    failed_checks=$(echo "$issues" | jq -r '.failed_checks // []')
    new_comments=$(echo "$issues" | jq -r '.new_bugbot_comments // []')
    unresolved_threads=$(echo "$issues" | jq -r '.unresolved_threads // []')

    cat <<PROMPT
/my:pr-finalize --fix

PR: ${OWNER}/${REPO}#${PR_NUMBER}
HEAD SHA: ${HEAD_SHA}
Working directory: ${WORKDIR}

The following issues were detected on this PR. Fix ALL of them, run tests locally, push the fixes, and resolve any review threads you addressed.

Failed checks:
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

    # Record fix start in summary
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

    # Launch Claude and pipe through formatter
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

    return 0  # Don't fail the monitor on a fix failure — keep polling
}

# Write final summary
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

# Main loop
main() {
    parse_args "$@"
    local consecutive_clean=0
    local fix_count=0

    echo "=== PR Monitor started ==="
    echo "  PR: ${OWNER}/${REPO}#${PR_NUMBER}"
    echo "  Polling every ${POLL_INTERVAL}s"
    echo "  Summary: ${SUMMARY_FILE}"
    echo "  Log: ${LOG_FILE}"
    echo ""

    # Initialize summary file
    {
        echo "# PR Finalize Summary"
        echo ""
        echo "PR: ${OWNER}/${REPO}#${PR_NUMBER}"
        echo "Started: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo ""
    } > "$SUMMARY_FILE"

    while true; do
        local issues=""
        issues=$(collect_issues)

        if [[ "$issues" == "PENDING" ]]; then
            consecutive_clean=0
            echo "[$(date +%H:%M:%S)] Checks still running, waiting..."
        elif [[ -z "$issues" ]]; then
            consecutive_clean=$((consecutive_clean + 1))
            echo "[$(date +%H:%M:%S)] Clean poll ($consecutive_clean/$MAX_CONSECUTIVE_CLEAN)"
            if [[ $consecutive_clean -ge $MAX_CONSECUTIVE_CLEAN ]]; then
                echo "[$(date +%H:%M:%S)] All checks green. Done."
                finalize_summary "$fix_count"
                exit 0
            fi
        else
            consecutive_clean=0
            fix_count=$((fix_count + 1))
            echo "[$(date +%H:%M:%S)] Issues found — launching fix session #${fix_count}"
            launch_fix_session "$issues" "$fix_count"
            # Update HEAD SHA and push timestamp after fix
            local new_sha
            new_sha=$(gh api "repos/${OWNER}/${REPO}/pulls/${PR_NUMBER}" --jq '.head.sha') || new_sha="$HEAD_SHA"
            if [[ "$new_sha" != "$HEAD_SHA" ]]; then
                HEAD_SHA="$new_sha"
                PUSH_TIME=$(date -u +%Y-%m-%dT%H:%M:%SZ)
            fi
        fi

        sleep "$POLL_INTERVAL"
    done
}

main "$@"
