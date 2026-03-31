# Projects Restore Backup Timestamps Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore backup timestamp information in the `projects-restore` chooser so each entry shows absolute local modification time and relative age.

**Architecture:** Keep backup enumeration and chooser label formatting in `.doom.d/projects.el` near `projects--backup-files`, so `projects-restore` continues to consume `(label . path)` pairs without behavioral changes. Add focused ERT coverage in a dedicated test file that stubs file metadata and verifies label content, ordering, and path mapping.

**Tech Stack:** Emacs Lisp, ERT, `cl-lib`, Doom Emacs project session restore code

---

## File structure

- Modify: `.doom.d/projects.el`
  - Add a small helper to format backup timestamps from file mtimes.
  - Update `projects--backup-files` to include absolute local time and relative age in chooser labels.
- Create: `.doom.d/test-projects-backup-files.el`
  - Add ERT coverage for chooser labels and returned path mapping.

### Task 1: Add failing tests for backup chooser labels

**Files:**
- Create: `.doom.d/test-projects-backup-files.el`
- Modify: none
- Test: `.doom.d/test-projects-backup-files.el`

- [ ] **Step 1: Write the failing test**

```elisp
;;; test-projects-backup-files.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defmacro load! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@(cdr body)))
(defmacro use-package! (&rest _) nil)
(defmacro map! (&rest _) nil)
(defmacro add-hook! (&rest _) nil)
(defmacro undefine-key! (&rest _) nil)
(defmacro set-popup-rule! (&rest _) nil)
(defmacro defadvice! (&rest _) nil)

(load-file "/Users/andreas.pohl/tools/.doom.d/projects.el")

(ert-deftest projects-backup-files-includes-timestamps-in-labels ()
  (let ((projects--save-file "/tmp/session.el")
        (projects--backup-count 2))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path)
                 (member path '("/tmp/session.el"
                                "/tmp/session.el.1"
                                "/tmp/session.el.2"))))
              ((symbol-function 'file-attributes)
               (lambda (path)
                 (list nil nil nil nil nil nil
                       (pcase path
                         ("/tmp/session.el" (date-to-time "2026-03-31 14:25:00"))
                         ("/tmp/session.el.1" (date-to-time "2026-03-31 13:55:00"))
                         ("/tmp/session.el.2" (date-to-time "2026-03-31 12:25:00"))))))
              ((symbol-function 'current-time)
               (lambda () (date-to-time "2026-03-31 14:30:00"))))
      (let ((choices (projects--backup-files)))
        (should (equal (mapcar #'cdr choices)
                       '("/tmp/session.el"
                         "/tmp/session.el.1"
                         "/tmp/session.el.2")))
        (should (string-match-p
                 "current  (session\\.el)"
                 (caar choices)))
        (should (string-match-p
                 "2026-03-31 14:25"
                 (caar choices)))
        (should (string-match-p
                 "5m ago"
                 (caar choices)))
        (should (string-match-p
                 "backup 1 (session\\.el\\.1)"
                 (car (nth 1 choices)))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs -Q --batch -l .doom.d/test-projects-backup-files.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because `projects--backup-files` labels do not yet include timestamp text.

- [ ] **Step 3: Commit**

```bash
git add .doom.d/test-projects-backup-files.el
git commit -m "test: cover projects restore backup labels"
```

### Task 2: Implement timestamp label formatting in backup enumeration

**Files:**
- Modify: `.doom.d/projects.el:686-694`
- Test: `.doom.d/test-projects-backup-files.el`

- [ ] **Step 1: Write the minimal implementation**

Add these helpers above `projects--backup-files` in `.doom.d/projects.el`:

```elisp
(defun projects--format-relative-age (mtime)
  "Return a short relative age string for MTIME."
  (let* ((seconds (max 0 (floor (float-time (time-subtract (current-time) mtime))))))
    (cond
     ((< seconds 60) (format "%ss ago" seconds))
     ((< seconds 3600) (format "%sm ago" (/ seconds 60)))
     ((< seconds 86400) (format "%sh ago" (/ seconds 3600)))
     (t (format "%sd ago" (/ seconds 86400))))))

(defun projects--backup-label (base-label path)
  "Return chooser label for BASE-LABEL and PATH with timestamp details."
  (let* ((attrs (file-attributes path))
         (mtime (file-attribute-modification-time attrs))
         (absolute (format-time-string "%Y-%m-%d %H:%M" mtime))
         (relative (projects--format-relative-age mtime)))
    (format "%s — %s — %s" base-label absolute relative)))
```

Then replace `projects--backup-files` with:

```elisp
(defun projects--backup-files ()
  "Return existing backup files as an alist of (label . path), newest first."
  (let (result)
    (when (file-exists-p projects--save-file)
      (push (cons (projects--backup-label "current  (session.el)" projects--save-file)
                  projects--save-file)
            result))
    (cl-loop for n from 1 to projects--backup-count
             for path = (format "%s.%d" projects--save-file n)
             when (file-exists-p path)
             do (push (cons (projects--backup-label
                             (format "backup %d (session.el.%d)" n n)
                             path)
                            path)
                      result))
    (nreverse result)))
```

- [ ] **Step 2: Run test to verify it passes**

Run: `emacs -Q --batch -l .doom.d/test-projects-backup-files.el -f ert-run-tests-batch-and-exit`
Expected: PASS with 1 test, 0 failures.

- [ ] **Step 3: Commit**

```bash
git add .doom.d/projects.el .doom.d/test-projects-backup-files.el
git commit -m "feat: show backup timestamps in restore chooser"
```

### Task 3: Verify edge behavior and regression safety

**Files:**
- Modify: `.doom.d/test-projects-backup-files.el`
- Test: `.doom.d/test-projects-backup-files.el`

- [ ] **Step 1: Write the failing test for missing files and ordering**

Append this test to `.doom.d/test-projects-backup-files.el`:

```elisp
(ert-deftest projects-backup-files-skips-missing-backups-and-keeps-order ()
  (let ((projects--save-file "/tmp/session.el")
        (projects--backup-count 3))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path)
                 (member path '("/tmp/session.el"
                                "/tmp/session.el.2"))))
              ((symbol-function 'file-attributes)
               (lambda (_path)
                 (list nil nil nil nil nil nil
                       (date-to-time "2026-03-31 14:25:00"))))
              ((symbol-function 'current-time)
               (lambda () (date-to-time "2026-03-31 14:30:00"))))
      (let ((choices (projects--backup-files)))
        (should (equal (mapcar #'cdr choices)
                       '("/tmp/session.el"
                         "/tmp/session.el.2")))
        (should (= (length choices) 2))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs -Q --batch -l .doom.d/test-projects-backup-files.el -f ert-run-tests-batch-and-exit`
Expected: FAIL if implementation mishandles gaps or ordering.

- [ ] **Step 3: Adjust implementation only if needed**

If the new test fails, keep `projects--backup-files` as the single source of ordering and ensure it only appends entries for existing files while preserving current-first then ascending backup number order.

Expected final function:

```elisp
(defun projects--backup-files ()
  "Return existing backup files as an alist of (label . path), newest first."
  (let (result)
    (when (file-exists-p projects--save-file)
      (push (cons (projects--backup-label "current  (session.el)" projects--save-file)
                  projects--save-file)
            result))
    (cl-loop for n from 1 to projects--backup-count
             for path = (format "%s.%d" projects--save-file n)
             when (file-exists-p path)
             do (push (cons (projects--backup-label
                             (format "backup %d (session.el.%d)" n n)
                             path)
                            path)
                      result))
    (nreverse result)))
```

- [ ] **Step 4: Run test suite to verify it passes**

Run: `emacs -Q --batch -l .doom.d/test-projects-backup-files.el -f ert-run-tests-batch-and-exit`
Expected: PASS with 2 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects.el .doom.d/test-projects-backup-files.el
git commit -m "test: cover projects backup chooser ordering"
```

### Task 4: Final verification

**Files:**
- Modify: none
- Test: `.doom.d/test-projects-backup-files.el`

- [ ] **Step 1: Run focused verification**

Run: `emacs -Q --batch -l .doom.d/test-projects-backup-files.el -f ert-run-tests-batch-and-exit`
Expected: PASS with all backup chooser tests green and no unexpected warnings.

- [ ] **Step 2: Run existing related config test**

Run: `emacs -Q --batch -l .doom.d/test-vterm-selection.el -f ert-run-tests-batch-and-exit`
Expected: PASS so the new work does not break existing checked-in batch tests.

- [ ] **Step 3: Inspect git diff**

Run: `git diff -- .doom.d/projects.el .doom.d/test-projects-backup-files.el`
Expected: Diff only shows backup label helpers, updated chooser labels, and focused ERT coverage.

- [ ] **Step 4: Commit**

```bash
git add .doom.d/projects.el .doom.d/test-projects-backup-files.el
git commit -m "chore: verify projects restore timestamp changes"
```
