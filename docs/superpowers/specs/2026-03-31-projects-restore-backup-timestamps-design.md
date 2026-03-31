# Projects restore backup timestamps design

## Goal

Restore backup chooser labels in `projects-restore` so each available session file shows both its local modification time and its relative age.

## Current state

`projects--backup-files` in `.doom.d/projects.el` returns chooser labels that only identify the current session file and numbered backups.

`projects-restore` uses those labels as the completion UI when the user selects a backup with a prefix argument.

The current implementation does not expose file modification time in the chooser, so backup age is not visible when restoring.

## Design

Keep backup discovery and chooser label construction in `projects--backup-files`.

1. Enumerate the current session file and numbered backup files exactly as today.
2. For each existing file, read its file modification time from file attributes.
3. Format the chooser label to include:
   - the current or backup identifier
   - an absolute local timestamp
   - a relative age string
4. Preserve the existing return shape `(label . path)` so `projects-restore` can continue to use the same selection flow.
5. Preserve the current ordering semantics so entries still appear newest first in the chooser.

## Recommended formatting

Use labels shaped like:

- `current  (session.el) — 2026-03-31 14:25 — 5m ago`
- `backup 1 (session.el.1) — 2026-03-31 13:55 — 35m ago`

The exact relative wording should remain concise and human-readable.

## Why this approach

This keeps file metadata lookup and display formatting close to the backup enumeration logic that already owns the chooser labels. `projects-restore` remains focused on selection and restore behavior instead of taking on presentation-specific metadata formatting.

This is the smallest change that restores the missing information without changing backup rotation, restore semantics, or selection mapping.

## Testing

Add or restore automated coverage for backup chooser labels to verify:

1. Labels still map to the correct session file paths.
2. Labels include the current or backup identity.
3. Labels include an absolute local timestamp.
4. Labels include a relative age string.
5. Ordering remains unchanged.

## Non-goals

- Changing backup rotation behavior
- Changing backup retention count
- Changing restore file parsing or restore execution
- Adding new restore UI outside the existing chooser
- Changing the save file location
