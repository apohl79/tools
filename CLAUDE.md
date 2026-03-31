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
