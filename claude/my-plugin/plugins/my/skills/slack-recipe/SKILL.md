---
name: managing-slack-recipe
description: Manages Slack operations including sending messages, reading channels, searching conversations, personal assistant with AI analysis, and message inbox. Use when working with Slack, sending messages, reading channels, searching for users or messages, checking inbox ("what slack messages do I have?"), or managing Slack communication.
---

# Slack Management

## Quick Reference

**List → Read → Send → Reply → Search → Cache → Assistant → Inbox**

```bash
node ~/.slack/slack.js channels
node ~/.slack/slack.js send C123ABC456 "Message text"
node ~/.slack/slack.js search "query text"
node ~/.slack/slack.js get-user nuno
node ~/.slack/slack.js assistant              # Start monitoring
inbox list                                    # View messages (inbox-recipe)
inbox done <id>                               # Mark done (inbox-recipe)
```

## When to Use

```
mention: Slack | message | channel | DM | workspace → activate
request: "message X" | "check Y" | "find Z" → execute
query: "what slack messages" | "what's on slack" | "what should I check" → use inbox-recipe (inbox list)
```

## Setup

```bash
[ ! -f ~/.slack/slack.js ] && {
  mkdir -p ~/.slack
  cp slack.js package.json config.json.example users-cache.json ~/.slack/
  cd ~/.slack && npm install
  echo "✓ Slack CLI installed to ~/.slack"
}

[ ! -f ~/.slack/config.json ] && {
  cp ~/.slack/config.json.example ~/.slack/config.json
  echo "⚠ Edit ~/.slack/config.json with your token"
}
```

[setup.md](setup.md) → OAuth token generation

**Token scopes:**
```
channels:read, channels:history, chat:write, search:read, users:read, im:read, im:history, mpim:read, groups:read, team:read
```

Note: `team:read` is required for personal assistant mode to generate Slack links.

## Core Operations

| Operation | Command | Output |
|-----------|---------|--------|
| List channels | `node ~/.slack/slack.js channels` | ID + name/type |
| Read history | `node ~/.slack/slack.js read <channel_id>` | Last 20 messages |
| Send message | `node ~/.slack/slack.js send <channel_id> <text>` | Confirmation |
| Reply to thread | `node ~/.slack/slack.js reply <channel_id> <timestamp> <text>` | Confirmation |
| Search | `node ~/.slack/slack.js search <query>` | Matches with context |
| User info | `node ~/.slack/slack.js me` | Your username + ID |
| Get user ID | `node ~/.slack/slack.js get-user <name>` | User ID (cached or searched) |
| Cache user | `node ~/.slack/slack.js cache-user <name> <user_id>` | Confirmation |
| List cache | `node ~/.slack/slack.js list-cache` | All cached users |
| Clear cache | `node ~/.slack/slack.js clear-cache` | Confirmation |
| Start assistant | `node ~/.slack/slack.js assistant` | Continuous monitoring |

## Channel Operations

**List:**
```bash
node ~/.slack/slack.js channels
```
Output:
```
C123ABC456 - engineering
D234BCD567 - DM
G345CDE678 - private-channel
```

**Read history:**
```bash
node ~/.slack/slack.js read C123ABC456
```
Output:
```
[John Doe] Hey team!
[Jane Smith] Working on the PR
[Bot] Build succeeded
```

**Find by name:**
```bash
node ~/.slack/slack.js channels | grep "engineering"
```

## Channel IDs

| Type | Format | Example |
|------|--------|---------|
| Public | C+10chars | C123ABC456 |
| Private | G+10chars | G123ABC456 |
| DM | D+10chars | D123ABC456 |
| User | U+10chars | U06SZL4S5BK |

Find: `node ~/.slack/slack.js channels | grep "name"`

## Messaging

**Channel:**
```bash
node ~/.slack/slack.js send C123ABC456 "Deployment complete"
```

**DM:**
```bash
node ~/.slack/slack.js send U06SZL4S5BK "Hey, quick question"
```

**Thread reply:**
```bash
SLACK_SHOW_TS=1 node ~/.slack/slack.js search "original message"
node ~/.slack/slack.js reply C123ABC456 1234567890.123456 "Reply text"
```

## Search Operations

**Messages:**
```bash
node ~/.slack/slack.js search "deployment"
```
Output:
```
[engineering] john: deployment started
[devops] jane: deployment config updated
```

**Users:**
```bash
node ~/.slack/slack.js search "Nuno"
```
Output:
```
[engineering] mohsen: @U06SZL4S5BK mentioned
[ai-dev] gabriele: Nuno working on feature
```

**In channel:**
```bash
node ~/.slack/slack.js search "in:#engineering error"
```

## User Info

```bash
node ~/.slack/slack.js me
```
Output:
```
User: mohsen.bostan (U04REN2QUKS)
```

## User Cache Operations

**Get user ID (checks cache first, then searches):**
```bash
node ~/.slack/slack.js get-user nuno
```
Output:
```
✓ Found in cache: nuno → U06SZL4S5BK
```

If not cached:
```
Searching for "nuno"...
Found 1 user(s):
  U06SZL4S5BK - Nuno Filipe Vieira Marques
  ✓ Auto-cached as "nuno"
```

**Manually cache a user:**
```bash
node ~/.slack/slack.js cache-user gabriele U07119WRTMZ
```
Output:
```
✓ Cached gabriele → U07119WRTMZ
```

**List all cached users:**
```bash
node ~/.slack/slack.js list-cache
```
Output:
```
Cached users (3):
  nuno → U06SZL4S5BK
  issam → U07HCBZTR37
  gabriele → U07119WRTMZ
```

**Clear the cache:**
```bash
node ~/.slack/slack.js clear-cache
```
Output:
```
✓ Cache cleared
```

**Cache location:**
```
~/.slack/users-cache.json
```

## Personal Assistant

Uses [inbox-recipe](../inbox-recipe/SKILL.md) for message management.

**Start assistant (runs continuously):**
```bash
node ~/.slack/slack.js assistant
```

Output:
```
🤖 Personal Assistant activated for mohsen.bostan
📋 Monitoring messages from 3 user(s)
✅ Ready! Waiting for messages...

📨 New message from Nuno Filipe Vieira Marques: Can you review my PR? It's urgent
🔍 Analyzing with Claude...
📊 Analysis:
URGENCY: URGENT
ACTION_NEEDED: YES
SUMMARY: Nuno is requesting a code review for an urgent PR
PRIORITY: High

💾 Cached message
🔔 Notification sent!
```

**View messages:**
```bash
inbox list
```

See [inbox-recipe](../inbox-recipe/SKILL.md) for full inbox commands (done, archive, clear).

**Workflow:**
```
DM → analyze (Claude) → add to inbox → notify → inbox list → inbox done
```

**Requirements:**

| Component | Requirement | Install |
|-----------|-------------|---------|
| APP_TOKEN | config.json | [setup.md](setup.md) Socket Mode |
| OAuth scope | team:read | OAuth & Permissions |
| terminal-notifier | macOS notifications | `brew install terminal-notifier` |
| Claude CLI | Message analysis | https://claude.com/claude-code |
| inbox CLI | Note management | [inbox-recipe](../inbox-recipe/SKILL.md) |

## Common Patterns

**Find + message user:**
```bash
node ~/.slack/slack.js get-user nuno     # Cached/searches
node ~/.slack/slack.js send U06SZL4S5BK "Message text"
```

**Monitor channel:**
```bash
node ~/.slack/slack.js read C123ABC456
node ~/.slack/slack.js search "in:#channel-name keyword"
```

**Check mentions:**
```bash
node ~/.slack/slack.js search "mohsen"
```

## Errors

```
invalid_auth → check ~/.slack/config.json token
channel_not_found → verify ID format (C|G|D|U+10chars)
not_in_channel → join channel | add bot
missing_scope → add OAuth scope | regenerate token
rate_limited → read Retry-After header → backoff
ENOENT config.json → run setup script
```

## Workflow

```
~/.slack/slack.js missing → install | exists → parse_intent → identify_targets → execute_operation → parse_output → report
```

Operations:
```
send → node ~/.slack/slack.js send <id> <text>
read → node ~/.slack/slack.js read <id>
search → node ~/.slack/slack.js search <query>
```

## Validation

| Phase | Command | Expected |
|-------|---------|----------|
| Install | Setup script | ~/.slack/ created |
| Configure | Edit config.json | Token set |
| Auth | `node ~/.slack/slack.js me` | User ID returned |
| List | `node ~/.slack/slack.js channels` | Channels listed |
| Send | `send C123 "test"` | Message appears |

**Checklist:**
```
~/.slack/slack.js exists?
~/.slack/config.json configured?
me command returns user?
channels command lists channels?
test message sends?
assistant mode: APP_TOKEN set?
assistant mode: message.im event subscribed?
inbox command: returns cached messages?
```

**Validation workflow:**

| Phase | Command | Expected |
|-------|---------|----------|
| Basic | `node ~/.slack/slack.js me` | User ID returned |
| Channels | `node ~/.slack/slack.js channels` | List displayed |
| Cache | `node ~/.slack/slack.js list-cache` | Cached users shown |
| Inbox | `node ~/.slack/slack.js inbox` | Messages or "No messages" |
| Assistant | `node ~/.slack/slack.js assistant` | Connects successfully |

## Example Session

```bash
$ node ~/.slack/slack.js search "Nuno"
[engineering] mohsen.bostan: @U06SZL4S5BK mentioned in discussion

$ node ~/.slack/slack.js send U06SZL4S5BK "Hey! Quick question about the PR"
✓ Message sent

$ node ~/.slack/slack.js read C08SU0H7T9U
[Nuno Marques] Sure, what's up?
[Mohsen Bostan] Just sent you a message
```
