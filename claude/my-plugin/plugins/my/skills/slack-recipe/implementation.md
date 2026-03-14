# CLI Implementation Reference

## Architecture

```
slack.js
├─ Import @slack/web-api
├─ Load config.json → USER_OAUTH_TOKEN
├─ Parse argv → command + args
├─ Switch command → execute operation
└─ Error handling → log message
```

## Dependencies

```json
{
  "dependencies": {
    "@slack/web-api": "^7.0.0"
  }
}
```

## Core Implementation

```javascript
#!/usr/bin/env node
import { WebClient } from '@slack/web-api';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const config = JSON.parse(readFileSync(join(__dirname, 'config.json'), 'utf8'));
const client = new WebClient(config.USER_OAUTH_TOKEN);

const [,, command, ...args] = process.argv;
```

## Command Handlers

**List channels:**
```javascript
case 'channels':
  const channels = await client.conversations.list({
    types: 'public_channel,private_channel,im,mpim'
  });
  channels.channels.forEach(c =>
    console.log(`${c.id} - ${c.name || c.user || 'DM'}`)
  );
  break;
```

**Read channel history:**
```javascript
case 'read':
  const [readChannel] = args;
  const history = await client.conversations.history({
    channel: readChannel,
    limit: 20
  });
  for (const msg of history.messages.reverse()) {
    const user = msg.user ?
      (await client.users.info({ user: msg.user })).user.real_name :
      'Bot';
    console.log(`[${user}] ${msg.text || '(attachment)'}`);
  }
  break;
```

**Send message:**
```javascript
case 'send':
  const [channel, ...msgParts] = args;
  await client.chat.postMessage({
    channel,
    text: msgParts.join(' ')
  });
  console.log('✓ Message sent');
  break;
```

**Search messages:**
```javascript
case 'search':
  const query = args.join(' ');
  const results = await client.search.messages({
    query,
    count: 10
  });
  results.messages.matches.forEach(m => {
    console.log(`\n[${m.channel.name}] ${m.username}: ${m.text}`);
  });
  break;
```

**Get user info:**
```javascript
case 'me':
  const auth = await client.auth.test();
  console.log(`User: ${auth.user} (${auth.user_id})`);
  break;
```

## Error Handling

```javascript
try {
  // Command execution
} catch (e) {
  console.error('Error:', e.message);
}
```

## Slack Web API Methods

| Operation | Method | Parameters |
|-----------|--------|------------|
| List channels | `conversations.list` | `types` |
| Read history | `conversations.history` | `channel, limit` |
| Send message | `chat.postMessage` | `channel, text` |
| Search | `search.messages` | `query, count` |
| Auth test | `auth.test` | none |
| User info | `users.info` | `user` |
