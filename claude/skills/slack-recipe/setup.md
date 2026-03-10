# Slack Setup

## Installation

**Automatic (Recommended):**

Run from skill directory:
```bash
# Check if CLI exists
[ ! -f ~/.slack/slack.js ] && {
  mkdir -p ~/.slack
  cp slack.js package.json config.json.example ~/.slack/
  cd ~/.slack && npm install
  echo "✓ Slack CLI installed to ~/.slack"
}

# Configure token
[ ! -f ~/.slack/config.json ] && {
  cp ~/.slack/config.json.example ~/.slack/config.json
  echo "⚠ Edit ~/.slack/config.json with your token"
}
```

**Manual:**

```bash
mkdir -p ~/.slack
cp slack.js package.json config.json.example ~/.slack/
cd ~/.slack
npm install
cp config.json.example config.json
# Edit config.json with your token
```

## OAuth Token Generation

**Create Slack App:**
```
1. Visit https://api.slack.com/apps
2. Create New App → From scratch
3. Name app + select workspace
```

**Configure OAuth scopes:**
```
Settings → OAuth & Permissions → User Token Scopes:
- channels:read
- channels:history
- chat:write
- search:read
- users:read
- im:read
- im:history
- mpim:read
- groups:read
- reactions:write
- reactions:read
- team:read (required for personal assistant)
```

**Install to workspace:**
```
OAuth & Permissions → Install to Workspace
→ Authorize
→ Copy "User OAuth Token" (starts with xoxp-)
```

## Socket Mode Setup (For Personal Assistant)

**Only required if using `assistant` command for AI-powered message monitoring**

**Enable Socket Mode:**
```
1. Settings → Socket Mode
2. Toggle "Enable Socket Mode" to ON
3. Click "Generate an App-Level Token"
   - Token Name: "socket-token" (or any name)
   - Scope: connections:write
   - Click "Generate"
4. Copy the token (starts with xapp-)
```

**Subscribe to Events:**
```
1. Settings → Event Subscriptions
2. Toggle "Enable Events" to ON
3. Expand "Subscribe to events on behalf of users"
4. Click "Add Workspace Event"
5. Search for and add: message.im
6. Click "Save Changes"
7. Reinstall the app when prompted
```

**Add APP_TOKEN to config.json:**
```json
{
  "USER_OAUTH_TOKEN": "xoxp-...",
  "APP_TOKEN": "xapp-1-A09NH9CP8NQ-..."
}
```

## Configuration File

Read the user token from 1password: `op --account parloa.1password.eu item get "claude passwords" --fields 'label=slack personal token' --reveal`

**Create config.json in skill directory:**
```json
{
  "USER_OAUTH_TOKEN": "[token from 1password]"
}
```

**Permissions:**
```bash
chmod 600 config.json
```

## Security

**NEVER commit tokens:**
```gitignore
config.json
**/*token*.json
```

**Token rotation:**
```
Settings → OAuth & Permissions → Revoke → Generate new
```

## Verification

```bash
# Test token
curl -H "Authorization: Bearer [token-from-1password]" \
  https://slack.com/api/auth.test

# Expected response
{
  "ok": true,
  "url": "https://workspace.slack.com/",
  "user": "username",
  "user_id": "U123ABC456"
}
```
