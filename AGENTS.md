# REPO STRUCTURE

- setup script/config, dotfiles and tools in main repo dir
- `.doom.d/` - Doom Emacs based setup.
- `Codex/` - Codex tools and plugin/skills

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

### LIVE RELOAD INTO RUNNING DAEMON

After editing any file under `.doom.d/`, ask the user: **"Reload into the running daemon?"**. If yes, load the affected files via `emacsclient -e '(load-file "...")'`. The user runs a long-lived Emacs daemon and a restart loses all session context (open buffers, Codex/vterm sessions, magit state).

Reload command pattern:

```bash
emacsclient -e '(progn (load-file "/Users/andreas.pohl/tools/.doom.d/+functions.el") (load-file "/Users/andreas.pohl/tools/.doom.d/projects.el") (load-file "/Users/andreas.pohl/tools/.doom.d/config.el") "reloaded")'
```

Only load the files that actually changed. Order matters: `+functions.el` and `projects.el` define functions used by `config.el`, so load them first.

Caveats:
- `(advice-add ...)` calls in reloaded code add another copy of the advice. If the change is to the advice body, remove the old advice first via `advice-remove` (or write a small `advice-mapc` cleanup script in `/tmp/Codex/`).
- `(add-hook ...)` is idempotent for named functions but adds duplicates for lambdas. Same caveat — remove old lambdas first.
- `setq` updates take effect immediately. `defvar` does NOT re-evaluate the default value if the var is already bound (use `setq` for one-shot value updates).
- Hook bodies guarded by `(unless projects--hooks-installed-p ...)` skip on reload — that's by design; do not naively flip the guard.

After reload, verify with a probe like `emacsclient -e '(fboundp (quote my-new-function))'` or by reading `*Messages*` for any errors.
