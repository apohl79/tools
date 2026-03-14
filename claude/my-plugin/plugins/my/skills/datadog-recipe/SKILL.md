---
name: datadog-recipe
description: Manages Datadog operations via API and dogshell CLI - monitors, metrics, events, dashboards, SLOs, downtimes, and tags. Use when querying Datadog, checking monitor status, posting metrics, managing monitors, or investigating alerts.
allowed-tools: Bash, Read, Write, Edit, Grep, Glob
---

# Datadog Operations Skill

## Authentication

Credentials are cached in `/tmp/.dd_cache` for 1 hour to avoid repeated 1Password prompts. Use this helper before any Datadog operation:

```bash
_dd_auth() {
  local cache="/tmp/.dd_cache"
  if [[ -f "$cache" ]] && [[ $(( $(date +%s) - $(stat -f%m "$cache") )) -lt 3600 ]]; then
    source "$cache"
  else
    DD_API_KEY=$(op item get "datadog api key" --account parloa.1password.eu --vault Employee --fields label=credential)
    DD_APP_KEY=$(op item get "datadog app key" --account parloa.1password.eu --fields label=credential)
    printf 'export DD_API_KEY="%s"\nexport DD_APP_KEY="%s"\n' "$DD_API_KEY" "$DD_APP_KEY" > "$cache"
    chmod 600 "$cache"
  fi
  export DD_API_KEY DD_APP_KEY
}
_dd_auth
```

Run `_dd_auth` at the start of each bash command that needs Datadog credentials. The cache auto-expires after 1 hour. To force refresh: `rm /tmp/.dd_cache`.

API host: `https://api.datadoghq.eu`

## Methods

### 1. dogshell CLI (`dog`)

Use for simple operations. Requires `.dogrc` at `~/.dogrc` (already configured).

```bash
# List all monitors
dog monitor show_all

# Show specific monitor
dog monitor show <MONITOR_ID>

# Mute a monitor
dog monitor mute <MONITOR_ID>

# Unmute a monitor
dog monitor unmute <MONITOR_ID>

# Post an event
dog event post "Title" "Message" --tags "env:prod"

# Query metrics
dog metric query <FROM_EPOCH> <TO_EPOCH> "avg:system.cpu.user{env:prod}"

# Search
dog search query "hosts:myhost"

# Manage downtimes
dog downtime post <SCOPE> --start <EPOCH> --end <EPOCH> --message "Reason"

# Host operations
dog host mute <HOSTNAME> --message "Reason"
dog host unmute <HOSTNAME>
```

### 2. Direct API (curl)

Use for operations dogshell doesn't support (searching monitors, querying with complex filters, getting monitor group states).

```bash
_dd_auth  # loads DD_API_KEY and DD_APP_KEY from cache or 1Password

# Search monitors by status
curl -s -X GET "https://api.datadoghq.eu/api/v1/monitor/search?query=status%3AAlert" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY"

# Get monitor with group states
curl -s -X GET "https://api.datadoghq.eu/api/v1/monitor/<ID>?group_states=all" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY"

# Query metrics (timeseries)
curl -s -X GET "https://api.datadoghq.eu/api/v1/query?from=<EPOCH>&to=<EPOCH>&query=<URL_ENCODED_QUERY>" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY"

# List SLOs
curl -s -X GET "https://api.datadoghq.eu/api/v1/slo" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY"

# Get events
curl -s -X GET "https://api.datadoghq.eu/api/v1/events?start=<EPOCH>&end=<EPOCH>" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY"
```

## Common Patterns

### Check alerting monitors with summary

```bash
_dd_auth

curl -s -X GET "https://api.datadoghq.eu/api/v1/monitor/search?query=status%3AAlert&per_page=50" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY" \
  | python3 -c "
import json,sys
data = json.load(sys.stdin)
for m in data.get('monitors', []):
    name = m.get('name','')
    tags = m.get('tags',[])
    env = [t for t in tags if t.startswith('env:')]
    svc = [t for t in tags if t.startswith('service:')]
    team = [t for t in tags if t.startswith('team:')]
    tier = [t for t in tags if t.startswith('tier:')]
    print(f'{name}')
    print(f'  env={env} service={svc} team={team} tier={tier}')
"
```

### Query a metric for the last N hours

```bash
_dd_auth
FROM=$(python3 -c "import time; print(int(time.time()) - 3600 * N)")
TO=$(python3 -c "import time; print(int(time.time()))")

curl -s -X GET "https://api.datadoghq.eu/api/v1/query?from=$FROM&to=$TO&query=<URL_ENCODED_METRIC_QUERY>" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY"
```

### Get monitor group states (which groups are alerting)

```bash
_dd_auth

curl -s -X GET "https://api.datadoghq.eu/api/v1/monitor/<MONITOR_ID>?group_states=all" \
  -H "DD-API-KEY: $DD_API_KEY" \
  -H "DD-APPLICATION-KEY: $DD_APP_KEY" \
  | python3 -c "
import json,sys,datetime
data = json.load(sys.stdin)
for group, info in data.get('state',{}).get('groups',{}).items():
    status = info.get('status')
    ts = info.get('last_triggered_ts')
    lt = datetime.datetime.fromtimestamp(int(ts), tz=datetime.timezone.utc).isoformat() if ts else 'N/A'
    print(f'{group}: {status} (triggered: {lt})')
"
```

## Important Notes

- Always use `https://api.datadoghq.eu` (EU region), not `datadoghq.com`
- For epoch timestamps on macOS use `python3 -c "import time; print(int(time.time()))"` instead of `date +%s` for arithmetic
- URL-encode metric queries when using curl
- Monitor search supports: `status:Alert`, `status:Warn`, `status:OK`, `status:No Data`, `tag:"key:value"`, `type:metric`
- Paginate with `per_page` (max 100) and `page` (0-indexed) for monitor search
- Parse large JSON responses through `python3 -c` or `python3 -m json.tool` for readability
- Never hardcode API keys - always use `_dd_auth` which caches keys in `/tmp/.dd_cache` for 1 hour
- To force credential refresh: `rm /tmp/.dd_cache`
