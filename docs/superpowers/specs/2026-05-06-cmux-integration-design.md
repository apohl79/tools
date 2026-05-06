# Cmux integration for Doom projects

## Goal

Add cmux support for the Doom Emacs projects workflow without changing the current wezterm behavior. The existing `projects.el` implementation remains the wezterm backend. A new `projects-cmux.el` backend provides cmux-specific workspace, pane, and browser integration.

Cmux mode is optimized for in-terminal browser support while keeping the current Emacs-centric editing model: Claude, vterm, Magit, and project buffers remain Emacs buffers; only browser views are moved into cmux browser panes.

## Non-goals

- Do not refactor or split the current `projects.el` implementation during the first cmux implementation.
- Do not sync cmux-initiated workspace changes back into Emacs.
- Do not replace Emacs buffers with native cmux terminal panes for Claude, vterm, tests, or shells.
- Do not start with one Emacs daemon per project.

## Backend selection

`config.el` chooses exactly one backend at startup. Selection is driven by an explicit environment variable set by the `tools/emacs` wrapper when it starts the named daemon:

```elisp
(defvar my/projects-mode
  (pcase (getenv "MY_PROJECTS_MODE")
    ("cmux" 'cmux)
    ("wezterm" 'wezterm)
    (_ 'wezterm)))

(pcase my/projects-mode
  ('cmux    (load! "projects-cmux"))
  ('wezterm (load! "projects")))
```

Do not use `cmux ping` as the primary detector. A reachable cmux socket only proves cmux is running somewhere; it does not prove the current Emacs daemon was started for cmux. The wrapper owns the decision and exports `MY_PROJECTS_MODE=cmux` or `MY_PROJECTS_MODE=wezterm` before daemon startup.

Only one backend loads. `projects-cmux.el` must be self-contained and must not require or load `projects.el`. It may copy the pure project-model portions of `projects.el` for now. A later shared-core extraction can be considered after cmux mode stabilizes.

## Emacs daemon selection

The existing `tools/emacs` wrapper must be updated so wezterm and cmux use different named daemons.

Current behavior uses the default daemon:

```sh
emacs --daemon
emacsclient -c -nw
```

New behavior:

- wezterm sessions use a stable daemon name such as `wezterm`.
- cmux sessions use a stable daemon name such as `cmux`.
- `emacsclient` always passes `-s <daemon-name>`.
- `emacs --daemon=<daemon-name>` starts the matching daemon when needed.
- `-kill` kills the selected daemon, not every daemon.

The wrapper should infer the daemon from the environment:

- If running inside cmux, use daemon `cmux` and export `MY_PROJECTS_MODE=cmux` before starting or connecting to that daemon.
- Else if running inside wezterm, use daemon `wezterm` and export `MY_PROJECTS_MODE=wezterm` before starting or connecting to that daemon.
- Else use the default daemon name `default`, with `MY_PROJECTS_MODE=wezterm` unless explicitly overridden.

This separates cmux and wezterm runtime state while avoiding one daemon per project. It prevents cmux experiments from disrupting the existing long-lived wezterm daemon. Because daemon mode selection is fixed at daemon startup, changing `MY_PROJECTS_MODE` later only affects newly started daemons, not an already-running one.

## Cmux project model

`projects-cmux.el` provides the same public command surface expected by the rest of the Doom config:

- `projects-create`
- `projects-delete`
- `projects-rename`
- `projects-switch`
- `projects-set-layout`
- `projects-clone-from-git`
- model accessors such as `projects-current`, `projects-dir`, `projects-names`, `projects-buffers`

The first version may copy these model definitions from `projects.el` rather than extracting a shared core.

Emacs is the source of truth. Project creation, deletion, rename, and selection happen through Emacs commands, and `projects-cmux.el` mirrors those actions into cmux with CLI calls. Workspaces manually created in cmux are ignored by Emacs.

## Cmux workspace mapping

Each Emacs project has a corresponding cmux workspace. The workspace's cwd is the project directory and the workspace name is the project name.

### `projects-create`

1. Add the project to the Emacs project table.
2. Call:

```sh
cmux new-workspace --name <project> --cwd <dir> --command '<emacsclient command>'
```

The command starts an emacsclient terminal frame in that workspace, bound to the project.

### `projects-delete`

1. Call:

```sh
cmux close-workspace --workspace <project>
```

2. Remove the project from the Emacs project table and clean up project buffers using the same policy as the current backend.

Closing the cmux workspace closes its panes and terminal frames. Buffers remain daemon-owned until the project deletion code kills or unregisters them.

### `projects-rename`

1. Rename the project in the Emacs table.
2. Call:

```sh
cmux rename-workspace --workspace <old> <new>
```

3. Rewrite any live frame parameter whose `projects-project` value is the old name.

## Frame to project binding

Cmux panes host terminal Emacs frames via `emacsclient -t`. Each frame is tagged with a frame parameter when created:

```sh
emacsclient -s cmux -t -F '((projects-project . "myproject"))'
```

`projects-cmux.el` installs an `after-make-frame-functions` hook. When a new frame has a `projects-project` parameter, the hook switches that frame to the named project.

Manual `emacsclient -t` invocations without the frame parameter are allowed. They create untagged frames. The user can tag them later by running `projects-switch` in that frame.

## Frame-local project switching

In cmux mode, `projects-switch` is frame-local. It changes the selected Emacs frame's `projects-project` parameter and updates Emacs's current-project bookkeeping, but it does not select or change the cmux workspace.

A separate command, `projects-cmux-select-workspace`, selects a cmux workspace:

```sh
cmux select-workspace --workspace <project>
```

This distinction allows a user to temporarily switch a frame inside workspace `foo` to project `bar`. That is allowed and intentionally does not mutate cmux workspace identity.

If workspace `foo` is later deleted, all panes and frames in that workspace close, including a frame that was temporarily switched to `bar`. Buffers survive according to daemon behavior, but that frame view is gone.

## Layout behavior

Cmux mode keeps the invariant: a workspace initially opens panes for the workspace's project.

`projects-set-layout` no longer means "show several different projects." In cmux mode it means "create a pane layout inside the current workspace, with each new pane running an Emacs frame initially bound to the workspace project."

For example, `projects-set-layout 2x2`:

1. Determine the cmux workspace project.
2. Close existing secondary panes managed by the layout command.
3. Split the current cmux workspace into four panes.
4. In each new pane, send an `emacsclient -s cmux -t -F '((projects-project . "<workspace-project>"))'` command.

Users may later switch any pane's frame to another project manually with `projects-switch`.

Because `cmux new-pane`/`new-split` do not accept a command, the initial implementation uses the two-step flow:

1. Create split/pane with `cmux new-split` or `cmux new-pane`.
2. Use `cmux send --surface <ref> "emacsclient ...\n"` to execute the emacsclient command in the pane's shell.

## Browser integration

In cmux mode, `browse-url-browser-function` is set to a cmux browser opener:

```elisp
(defun projects-cmux--browse-url (url &rest _)
  (projects-cmux--call "new-pane" "--type" "browser" "--url" url))
```

This affects flows such as markdown preview, which already call `browse-url` after starting a local server. The first version always creates a new browser pane. Reuse-by-workspace can be added later if pane churn becomes annoying.

## Error handling

All cmux CLI calls go through a helper that captures output in `*projects-cmux*` and returns the exit code:

```elisp
(defun projects-cmux--call (&rest args)
  (let ((buf (get-buffer-create "*projects-cmux*")))
    (with-current-buffer buf (goto-char (point-max)))
    (let ((exit (apply #'call-process "cmux" nil (list buf t) nil args)))
      (unless (zerop exit)
        (message "[projects-cmux] cmux %s → exit %d (see *projects-cmux*)"
                 (string-join args " ") exit))
      exit)))
```

Failure policy is non-fatal: log the cmux error and keep Emacs usable. The Emacs project model remains authoritative even if cmux fails to mirror an operation.

Add a manual `projects-cmux-resync` command to recreate missing cmux workspaces from the Emacs project table after cmux restarts. The first implementation includes this command because cmux failures are otherwise only logged.

## Startup behavior

No automatic workspace creation happens at daemon startup. Emacs loading config must not unexpectedly launch cmux UI or create panes.

Startup sequence:

1. The `tools/emacs` wrapper infers the target terminal environment and starts/connects to the matching named daemon with `MY_PROJECTS_MODE` exported.
2. The daemon loads `config.el`.
3. `my/projects-mode` reads `MY_PROJECTS_MODE` and selects `cmux` or `wezterm`.
4. Only the selected backend loads.
5. Desktop/session restoration may restore project tables and buffers.
6. The first cmux-launched `emacsclient -s cmux -t -F ...` frame binds itself to its project through the frame hook.

## Testing

### Wrapper tests

- Run `tools/emacs` from a wezterm environment and verify it uses the wezterm daemon name.
- Run `tools/emacs` from a cmux environment and verify it uses the cmux daemon name.
- Verify `tools/emacs -kill` kills only the selected named daemon.
- Verify default/non-wezterm/non-cmux behavior is documented and works.

### Backend selection tests

- With `MY_PROJECTS_MODE=wezterm`, verify `projects.el` loads and current behavior remains unchanged, even if cmux is running.
- With `MY_PROJECTS_MODE=cmux`, verify `projects-cmux.el` loads and `projects.el` does not load.

### Cmux backend tests

- `projects-create` creates a cmux workspace and launches an emacsclient frame bound to the project.
- `projects-switch` changes only the selected frame's project parameter and does not call `cmux select-workspace`.
- `projects-cmux-select-workspace` selects the cmux workspace without mutating the selected frame's project parameter.
- `projects-set-layout 2x2` creates four panes, each initially bound to the workspace project.
- Manual frame-local switching inside one pane does not affect sibling panes.
- `browse-url` opens a cmux browser pane.
- If `cmux` commands fail, the error is logged to `*projects-cmux*` and Emacs remains usable.

## Implementation order

1. Update `tools/emacs` to support named daemons for cmux vs wezterm.
2. Add backend selection in `config.el`, preserving wezterm as the fallback.
3. Create self-contained `projects-cmux.el` by copying the minimal project model from `projects.el` and replacing UI operations with cmux CLI calls.
4. Add frame-parameter initialization for cmux emacsclient frames.
5. Add cmux workspace/project operations.
6. Add cmux layout pane orchestration.
7. Add cmux browser integration.
8. Verify both modes: existing wezterm path and new cmux path.
