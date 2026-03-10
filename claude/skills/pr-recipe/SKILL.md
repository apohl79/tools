---
name: pr-recipe
description: Ensures PRs follow conventional commit rules before creation. Use when creating PRs, squashing commits for PRs, or when a conventional commit check fails.
---

# Conventional PR Skill

Ensures all commits in a PR follow conventional commit format before pushing.

## Rules

### Commit Message Format

Every commit message MUST follow:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

### Required Elements

| Element | Rule | Examples |
|---------|------|----------|
| **type** | One of: `feat`, `fix`, `perf`, `refactor`, `docs`, `test`, `ci`, `chore` | `fix`, `feat` |
| **scope** | Ticket number or `CCP-0` for untracked work. Always required. | `CCP-123`, `CCP-0` |
| **description** | Lowercase, imperative, no period at end | `add retry logic for auth failures` |

### PR Title

The PR title MUST also follow conventional commit format — CI checks "Enforce Conventional Commits" and "Enforce Ticket Number in PR Title" run against it.

```
fix(CCP-123): description here
feat(CCP-456): description here
```

### Multiple Commits

When a PR has multiple commits, **squash them into one** before pushing. The single commit message must follow the format above. This prevents conventional commit check failures on intermediate commits.

### Changelog Impact

The `git-cliff` tool generates changelogs from commit types:

| Type | Changelog Section | Bumps |
|------|-------------------|-------|
| `feat` | Features | minor version |
| `fix` | Bug Fixes | patch version |
| `perf` | Performance | patch version |
| `refactor` | Refactoring | patch version |
| `docs` | Documentation | patch version |
| `test` | Testing | patch version |
| `ci` | CI/CD | patch version |
| `chore` | Miscellaneous | patch version |
| Breaking change (`!`) | — | major version |

Commits with `chore(release)` are auto-skipped.

## Validation

Before pushing a PR branch, validate all commits:

```bash
# Check that all commits since base branch follow conventional format
git log origin/main..HEAD --format='%s' | while read -r msg; do
  if ! echo "$msg" | grep -qE '^(feat|fix|perf|refactor|docs?|test|ci|chore)(\([^)]+\))?: .+'; then
    echo "FAIL: $msg"
  fi
done
```

## When Fixing a Failed Check

1. Check which commits are non-compliant: `git log origin/main..HEAD --oneline`
2. Squash all commits into one: `git reset --soft origin/main && git commit -m "type(scope): description"`
3. Force push: `git push --force-with-lease`
