# Project Info Screen Shortcuts Design

## Summary

Add two single-key shortcuts to the project info screen in `.doom.d/projects.el`:

- `d` opens the current project's directory using the existing `C-x d RET` flow.
- `g` opens Magit for the current project using the existing `C-c p v` flow.

The change is limited to the project info screen and its on-screen help text.

## Current Context

The project info buffer is implemented in `projects-info-mode` and already exposes single-key shortcuts for Claude Code actions:

- `n` → new Claude Code session
- `c` → continue Claude Code session
- `r` → resume Claude Code session

The info buffer also sets `default-directory` to the current project root before rendering the buffer, so project-scoped actions can rely on that context.

## Recommended Approach

Add `d` and `g` bindings directly in `projects-info-mode-map` and update the info buffer contents to display the new shortcuts next to the existing ones.

This follows the current design of the screen:

- shortcuts are local to `projects-info-mode`
- actions execute from the active project's directory
- the buffer remains self-documenting because the visible help text matches the keymap

## Alternatives Considered

### 1. Direct key bindings in `projects-info-mode` (recommended)

Bind `d` and `g` in the mode map and show them in the buffer text.

**Pros**
- Matches the existing `n/c/r` implementation.
- Keeps the behavior scoped to the project info screen.
- Keeps the displayed help text and actual bindings in one feature area.

**Cons**
- Requires a small update in both the keymap and the rendered help text.

### 2. Simulate global key sequences from the info buffer

Map `d` and `g` to commands that replay `C-x d RET` and `C-c p v` exactly.

**Pros**
- Closely mirrors the requested user-facing shortcuts.

**Cons**
- More brittle because it depends on external bindings remaining unchanged.
- Harder to reason about than calling the intended commands in project context.

### 3. Add new standalone wrapper commands elsewhere

Introduce new helper commands and bind `d` and `g` to those wrappers.

**Pros**
- Can centralize logic if future reuse appears.

**Cons**
- Adds extra abstraction for a small, local behavior change.
- Unnecessary unless the actions need custom logic beyond invoking existing commands.

## Design Details

### Key bindings

Extend `projects-info-mode-map` with:

- `d` for opening Dired at the current project's root directory
- `g` for opening Magit for the current project

### Project context

The implementation must rely on the existing `default-directory` assignment in the project info buffer so both shortcuts operate on the active project.

### Screen text

Update the project info buffer display so the new shortcuts are visible together with the existing Claude Code shortcuts. The screen should continue to act as an empty-project landing page with discoverable actions.

## Error Handling

No new error-handling paths are required beyond the existing command behavior. If Dired or Magit cannot run, their normal Emacs errors are sufficient.

## Testing

Manual verification is sufficient for this change:

1. Open a project info buffer.
2. Press `d` and verify Dired opens at that project's root.
3. Return to the project info buffer.
4. Press `g` and verify Magit opens for that project.
5. Confirm the buffer text lists both shortcuts.
6. Confirm existing `n`, `c`, and `r` bindings still work.

## Scope

In scope:
- `projects-info-mode` keymap changes
- project info buffer help text updates

Out of scope:
- changing project switching behavior
- changing global key bindings
- broader refactoring of `projects.el`
