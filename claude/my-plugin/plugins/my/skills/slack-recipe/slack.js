#!/usr/bin/env node
import { WebClient } from '@slack/web-api';
import { SocketModeClient } from '@slack/socket-mode';
import { readFileSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { spawn } from 'child_process';

const __dirname = dirname(fileURLToPath(import.meta.url));

const CONFIG_PATH = join(__dirname, 'config.json');
const USERS_CACHE_PATH = join(__dirname, 'users-cache.json');
const INBOX_CLI = join(process.env.HOME, '.inbox', 'inbox');
const DEFAULT_MESSAGE_LIMIT = 20;
const DEFAULT_SEARCH_COUNT = 10;
const SHOW_TIMESTAMP_ENV = 'SLACK_SHOW_TS';
const TIMESTAMP_ENABLED_VALUE = '1';

const CommandType = Object.freeze({
  SEND: 'send',
  CHANNELS: 'channels',
  READ: 'read',
  SEARCH: 'search',
  ME: 'me',
  REACT: 'react',
  DM: 'dm',
  REPLY: 'reply',
  CACHE_USER: 'cache-user',
  GET_USER: 'get-user',
  LIST_CACHE: 'list-cache',
  CLEAR_CACHE: 'clear-cache',
  ASSISTANT: 'assistant'
});

const ConversationType = Object.freeze({
  PUBLIC_CHANNEL: 'public_channel',
  PRIVATE_CHANNEL: 'private_channel',
  IM: 'im',
  MPIM: 'mpim'
});

const ErrorMessage = Object.freeze({
  DM_SCOPE_MISSING: 'Note: Make sure your token has the "im:read" scope to open DM channels.',
  DM_USER_REQUIRED: 'Usage: node slack.js dm <user_id>'
});

const createConfig = (path) =>
  JSON.parse(readFileSync(path, 'utf8'));

const createSlackClient = (token) =>
  new WebClient(token);

const formatChannel = (channel) =>
  `${channel.id} - ${channel.name || channel.user || 'DM'}`;

const formatMessage = (user, text) =>
  `[${user}] ${text || '(attachment)'}`;

const formatSearchResult = (match, showTimestamp) =>
  showTimestamp
    ? `\n[${match.channel.name || match.channel.id}] ${match.username}: ${match.text}\n  Channel: ${match.channel.id}, Timestamp: ${match.ts}`
    : `\n[${match.channel.name || match.channel.id}] ${match.username}: ${match.text}`;

const formatUserInfo = (auth) =>
  `User: ${auth.user} (${auth.user_id})`;

const isTimestampEnabled = () =>
  process.env[SHOW_TIMESTAMP_ENV] === TIMESTAMP_ENABLED_VALUE;

// User cache functions
const loadUsersCache = () => {
  try {
    return JSON.parse(readFileSync(USERS_CACHE_PATH, 'utf8'));
  } catch (e) {
    return {};
  }
};

const saveUsersCache = (cache) =>
  writeFileSync(USERS_CACHE_PATH, JSON.stringify(cache, null, 2));

const getUserFromCache = (name) => {
  const cache = loadUsersCache();
  return cache[name.toLowerCase()];
};

const addUserToCache = (name, userId) => {
  const cache = loadUsersCache();
  cache[name.toLowerCase()] = userId;
  saveUsersCache(cache);
};

// Inbox integration helper
const generateSlackLink = (workspaceUrl, channelId, timestamp) => {
  // Convert timestamp format: 1234567890.123456 -> p1234567890123456
  const ts = timestamp.replace('.', '');
  return `${workspaceUrl}/archives/${channelId}/p${ts}`;
};

const addToInbox = async (message, urgency, priority, actionNeeded, link, metadata) => {
  return new Promise((resolve, reject) => {
    // Build JSON for programmatic add
    const noteData = JSON.stringify({
      message,
      urgency,
      priority,
      actionNeeded,
      link,
      metadata
    });

    // Call inbox CLI to add note
    const child = spawn(INBOX_CLI, ['add-json', noteData], {
      stdio: ['ignore', 'pipe', 'pipe']
    });

    let stdout = '';
    let stderr = '';

    child.stdout.on('data', (data) => stdout += data.toString());
    child.stderr.on('data', (data) => stderr += data.toString());

    child.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Inbox add failed: ${stderr}`));
      } else {
        resolve(stdout.trim());
      }
    });

    child.on('error', reject);

    setTimeout(() => {
      child.kill();
      reject(new Error('Inbox timeout'));
    }, 5000);
  });
};

const getUserName = async (client, userId) =>
  userId
    ? (await client.users.info({ user: userId })).user.real_name
    : 'Bot';

const sendMessage = async (client, args) => {
  const [channel, ...messageParts] = args;
  return client.chat.postMessage({
    channel,
    text: messageParts.join(' ')
  }).then(() => '✓ Message sent');
};

const listChannels = async (client) => {
  const conversationTypes = [
    ConversationType.PUBLIC_CHANNEL,
    ConversationType.PRIVATE_CHANNEL,
    ConversationType.IM,
    ConversationType.MPIM
  ].join(',');

  return client.conversations.list({
    types: conversationTypes
  }).then(response =>
    response.channels.map(formatChannel).join('\n')
  );
};

const readMessages = async (client, args) => {
  const [channel] = args;
  const history = await client.conversations.history({
    channel,
    limit: DEFAULT_MESSAGE_LIMIT
  });

  const messages = await Promise.all(
    history.messages.reverse().map(async (msg) => {
      const userName = await getUserName(client, msg.user);
      return formatMessage(userName, msg.text);
    })
  );

  return messages.join('\n');
};

const searchMessages = async (client, args) => {
  const query = args.join(' ');
  const results = await client.search.messages({
    query,
    count: DEFAULT_SEARCH_COUNT
  });

  const showTimestamp = isTimestampEnabled();
  return results.messages.matches
    .map(match => formatSearchResult(match, showTimestamp))
    .join('');
};

const getUserInfo = async (client) =>
  client.auth.test()
    .then(formatUserInfo);

const addReaction = async (client, args) => {
  const [channel, timestamp, emoji] = args;
  return client.reactions.add({
    channel,
    timestamp,
    name: emoji
  }).then(() => '✓ Reaction added');
};

const openDirectMessage = async (client, args) => {
  const [userId] = args;

  return !userId
    ? Promise.reject(new Error(ErrorMessage.DM_USER_REQUIRED))
    : client.conversations.open({ users: userId })
        .then(dmResult => dmResult.channel.id)
        .then(async (channelId) => {
          const history = await client.conversations.history({
            channel: channelId,
            limit: DEFAULT_MESSAGE_LIMIT
          });

          const messages = await Promise.all(
            history.messages.reverse().map(async (msg) => {
              const userName = await getUserName(client, msg.user);
              return formatMessage(userName, msg.text);
            })
          );

          return `DM Channel: ${channelId}\n\n=== Latest messages ===\n\n${messages.join('\n')}`;
        })
        .catch(error => {
          const errorMsg = error.data?.error || error.message;
          return Promise.reject(new Error(`DM Error: ${errorMsg}\n\n${ErrorMessage.DM_SCOPE_MISSING}`));
        });
};

const replyToThread = async (client, args) => {
  const [channel, timestamp, ...messageParts] = args;

  if (!channel || !timestamp || messageParts.length === 0) {
    return Promise.reject(new Error('Usage: node slack.js reply <channel> <timestamp> <message>'));
  }

  return client.chat.postMessage({
    channel,
    thread_ts: timestamp,
    text: messageParts.join(' ')
  }).then(() => '✓ Reply sent');
};

const cacheUser = async (client, args) => {
  const [name, userId] = args;

  if (!name || !userId) {
    return Promise.reject(new Error('Usage: node slack.js cache-user <name> <user_id>'));
  }

  addUserToCache(name, userId);
  return `✓ Cached ${name} → ${userId}`;
};

const getUser = async (client, args) => {
  const [searchName] = args;

  if (!searchName) {
    return Promise.reject(new Error('Usage: node slack.js get-user <name>'));
  }

  // Check cache first
  const cachedUserId = getUserFromCache(searchName);
  if (cachedUserId) {
    return `✓ Found in cache: ${searchName} → ${cachedUserId}`;
  }

  // Search for user
  const searchResults = await client.search.messages({
    query: searchName,
    count: 5
  });

  // Extract user IDs from mentions
  const mentions = new Set();
  const mentionPattern = /<@([A-Z0-9]+)\|[^>]+>/g;

  searchResults.messages.matches.forEach(m => {
    let match;
    while ((match = mentionPattern.exec(m.text)) !== null) {
      mentions.add(match[1]);
    }
  });

  if (mentions.size === 0) {
    return 'No users found';
  }

  const results = [];
  let autoCachedUserId;

  for (const uid of mentions) {
    const userInfo = await client.users.info({ user: uid });
    const name = userInfo.user.real_name || userInfo.user.name;
    results.push(`  ${uid} - ${name}`);

    // Auto-cache if name matches
    if (name.toLowerCase().includes(searchName.toLowerCase())) {
      addUserToCache(searchName, uid);
      results.push(`  ✓ Auto-cached as "${searchName}"`);
      autoCachedUserId = uid;
    }
  }

  return `Found ${mentions.size} user(s):\n${results.join('\n')}`;
};

const listCache = async () => {
  const cache = loadUsersCache();
  const entries = Object.entries(cache);

  if (entries.length === 0) {
    return 'Cache is empty';
  }

  const list = entries.map(([name, id]) => `  ${name} → ${id}`).join('\n');
  return `Cached users (${entries.length}):\n${list}`;
};

const clearCache = async () => {
  saveUsersCache({});
  return '✓ Cache cleared';
};

const analyzeMessageWithClaude = async (senderName, messageText, conversationContext) => {
  return new Promise((resolve, reject) => {
    const analysisPrompt = `You are Mohsen's personal assistant analyzing an incoming Slack message.

Sender: ${senderName}
Message: ${messageText}

${conversationContext ? `Recent conversation:\n${conversationContext}` : ''}

Analyze this message and provide:
1. Urgency level (LOW/MEDIUM/HIGH/URGENT)
2. Whether immediate action/reply is needed (YES/NO)
3. Brief 1-sentence summary of what it's about
4. Suggested priority for Mohsen

Format your response EXACTLY as:
URGENCY: [level]
ACTION_NEEDED: [YES/NO]
SUMMARY: [one sentence]
PRIORITY: [High/Medium/Low]`;

    const child = spawn('claude', [
      '-p',
      '--dangerously-skip-permissions',
      '--system-prompt',
      'You are a helpful assistant that analyzes Slack messages. Always follow the exact format requested.',
      analysisPrompt
    ], {
      cwd: process.env.HOME,
      env: process.env,
      shell: false,
      stdio: ['ignore', 'pipe', 'pipe']
    });

    let stdout = '';
    let stderr = '';

    child.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    child.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    child.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Claude analysis failed: ${stderr}`));
      } else {
        resolve(stdout.trim());
      }
    });

    child.on('error', (err) => {
      reject(err);
    });

    setTimeout(() => {
      child.kill();
      reject(new Error('Claude analysis timeout'));
    }, 20000);
  });
};

const sendMacOSNotification = (title, message, subtitle = '') => {
  const args = ['-title', title, '-message', message];

  if (subtitle) {
    args.push('-subtitle', subtitle);
  }

  spawn('terminal-notifier', args, { stdio: 'ignore' });
};

const runPersonalAssistant = async (client, config) => {
  if (!config.APP_TOKEN) {
    return Promise.reject(new Error('APP_TOKEN is required for assistant mode. Please add it to config.json'));
  }

  const socketClient = new SocketModeClient({
    appToken: config.APP_TOKEN,
    logLevel: 'error'
  });

  const authInfo = await client.auth.test();
  const myUserId = authInfo.user_id;

  // Get workspace URL
  const teamInfo = await client.team.info();
  const workspaceUrl = `https://${teamInfo.team.domain}.slack.com`;

  console.log(`🤖 Personal Assistant activated for ${authInfo.user}`);
  console.log(`📋 Monitoring all incoming DMs`);
  console.log(`✅ Ready! Waiting for messages...\n`);

  socketClient.on('message', async ({ event, ack }) => {
    try {
      await ack();

      // Only process DM messages
      if (event.type !== 'message' || event.subtype || event.channel_type !== 'im') return;

      // Ignore self messages
      if (event.user === myUserId) return;

      const userInfo = await client.users.info({ user: event.user });
      const userName = userInfo.user.real_name || userInfo.user.name;
      const firstName = userName.split(' ')[0];

      console.log(`\n📨 New message from ${userName}: ${event.text}`);
      console.log(`🔍 Analyzing with Claude...`);

      // Fetch conversation context
      let conversationContext = '';
      try {
        const history = await client.conversations.history({
          channel: event.channel,
          limit: 6
        });

        if (history.messages && history.messages.length > 1) {
          const recentMessages = history.messages.reverse().slice(0, -1);
          for (const msg of recentMessages) {
            if (msg.user) {
              const msgUser = await client.users.info({ user: msg.user });
              const msgUserName = msgUser.user.real_name || msgUser.user.name;
              conversationContext += `${msgUserName}: ${msg.text}\n`;
            }
          }
        }
      } catch (err) {
        console.log(`⚠️  Could not fetch context: ${err.message}`);
      }

      // Analyze with Claude
      try {
        const analysis = await analyzeMessageWithClaude(userName, event.text, conversationContext);
        console.log(`📊 Analysis:\n${analysis}\n`);

        // Parse analysis
        const urgencyMatch = analysis.match(/URGENCY:\s*(\w+)/i);
        const actionMatch = analysis.match(/ACTION_NEEDED:\s*(\w+)/i);
        const summaryMatch = analysis.match(/SUMMARY:\s*(.+)/i);
        const priorityMatch = analysis.match(/PRIORITY:\s*(\w+)/i);

        const urgency = urgencyMatch ? urgencyMatch[1] : 'UNKNOWN';
        const actionNeeded = actionMatch ? actionMatch[1] : 'UNKNOWN';
        const summary = summaryMatch ? summaryMatch[1].trim() : event.text.substring(0, 100);
        const priority = priorityMatch ? priorityMatch[1] : 'Medium';

        // Generate Slack link
        const slackLink = generateSlackLink(workspaceUrl, event.channel, event.ts);

        // Add to inbox
        try {
          await addToInbox(
            summary,
            urgency,
            priority,
            actionNeeded === 'YES',
            slackLink,
            {
              source: 'slack',
              sender: firstName,
              channel: event.channel,
              timestamp: event.ts
            }
          );
          console.log(`💾 Added to inbox`);
        } catch (inboxError) {
          console.log(`⚠️  Could not add to inbox: ${inboxError.message}`);
        }

        // Send notification
        const notificationTitle = `${firstName}: ${urgency} Priority`;
        const notificationMessage = `${summary}\n${actionNeeded === 'YES' ? '⚠️ Action needed!' : '📬 FYI'}`;

        sendMacOSNotification(notificationTitle, notificationMessage);
        console.log(`🔔 Notification sent!`);

      } catch (analysisError) {
        console.error(`❌ Analysis error: ${analysisError.message}`);

        // Add to inbox with fallback data
        const slackLink = generateSlackLink(workspaceUrl, event.channel, event.ts);
        try {
          await addToInbox(
            event.text.substring(0, 100),
            'MEDIUM',
            'Medium',
            false,
            slackLink,
            {
              source: 'slack',
              sender: firstName,
              channel: event.channel,
              timestamp: event.ts
            }
          );
        } catch (inboxError) {
          console.log(`⚠️  Could not add to inbox: ${inboxError.message}`);
        }

        // Send fallback notification
        sendMacOSNotification(
          `${firstName} sent a message`,
          event.text.substring(0, 100)
        );
      }

    } catch (error) {
      console.error(`Error processing message: ${error.message}`);
    }
  });

  await socketClient.start();

  // Keep running
  return new Promise(() => {});
};

const getHelpText = () => `Usage:
  node slack.js channels                          - List all channels
  node slack.js send <channel> <msg>              - Send message
  node slack.js reply <channel> <timestamp> <msg> - Reply to thread
  node slack.js read <channel>                    - Read recent messages
  node slack.js search <query>                    - Search messages (use SLACK_SHOW_TS=1 for timestamps)
  node slack.js react <channel> <timestamp> <emoji> - Add reaction to message
  node slack.js dm <user_id>                      - Read DM with user
  node slack.js me                                - Show your info

  User Cache Commands:
  node slack.js get-user <name>                   - Get user ID (searches and caches if not found)
  node slack.js cache-user <name> <user_id>       - Manually add user to cache
  node slack.js list-cache                        - List all cached users
  node slack.js clear-cache                       - Clear user cache

  Personal Assistant:
  node slack.js assistant                         - Start personal assistant (analyzes messages & sends notifications)

  Note: Use 'inbox list' and 'inbox clear-done' commands from inbox-recipe for viewing/managing messages`;

const commandHandlers = Object.freeze({
  [CommandType.SEND]: sendMessage,
  [CommandType.CHANNELS]: listChannels,
  [CommandType.READ]: readMessages,
  [CommandType.SEARCH]: searchMessages,
  [CommandType.ME]: getUserInfo,
  [CommandType.REACT]: addReaction,
  [CommandType.DM]: openDirectMessage,
  [CommandType.REPLY]: replyToThread,
  [CommandType.CACHE_USER]: cacheUser,
  [CommandType.GET_USER]: getUser,
  [CommandType.LIST_CACHE]: listCache,
  [CommandType.CLEAR_CACHE]: clearCache
});

const executeCommand = async (client, command, args) => {
  const handler = commandHandlers[command];
  return handler
    ? handler(client, args)
    : Promise.resolve(getHelpText());
};

const handleError = (error) => {
  console.error(error.message || 'Error: Unknown error occurred');
  process.exit(1);
};

const handleSuccess = (output) => {
  console.log(output);
  process.exit(0);
};

const main = () => {
  const [,, command, ...args] = process.argv;

  return Promise.resolve()
    .then(() => createConfig(CONFIG_PATH))
    .then(config => {
      const client = createSlackClient(config.USER_OAUTH_TOKEN);

      // Special handling for assistant command
      if (command === CommandType.ASSISTANT) {
        return runPersonalAssistant(client, config);
      }

      // Regular commands
      return executeCommand(client, command, args);
    })
    .then(handleSuccess)
    .catch(handleError);
};

main();
