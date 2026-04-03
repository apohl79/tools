# REPO STRUCTURE

- setup script/config, dotfiles and tools in main repo dir
- `.doom.d/` - Doom Emacs based setup.
- `claude/` - Claude code tools and plugin/skills

## EMACS

### USER ENVIRONMENT
- **Emacs Config Directory**: `~/.config/emacs`
- **Doom Config Directory**: `/.config/doom` (symlink to `.doom.d` in this repo)

### TESTING

When running Emacs batch tests, NEVER use `~/tools/emacs` or any wrapper that may connect to a running Emacs daemon. ALWAYS call `/opt/homebrew/bin/emacs` directly.

### RUNTIME

Always running in **terminal mode** (`emacsclient -t`). No GUI frames ever. GUI-only solutions (child frames, posframe, mini-frame) will never work.

### DEBUGGING

Before making any speculative fix to Emacs config, add diagnostic `message` calls first so the problem is understood from logs, not guessed. Never do trial-and-error without logging.
