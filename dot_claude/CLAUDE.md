# Style
Never use em dashes (—). Prefer commas, colons, semicolons, or regular dashes (-) instead.

# Git
## Branching
Prefer working on a branch rather than committing directly to main. Exception: personal/dotfiles repos where committing to main is acceptable.

## Committing
Use the `/commit` skill for both splitting uncommitted changes across commits and rewriting existing commit history.

## Commits
Never include "Co-Authored-By" lines in commits.
Never include "Generated with Claude Code" in PR descriptions.

## Pull request body template
When creating or editing a PR, look for a `PULL_REQUEST_TEMPLATE.md` in `.github/` or the repo root and use it as the body structure. Fill in each section with real content. Reproduce all checklist items, checking off satisfied ones (`[x]`) and leaving the rest unchecked (`[ ]`). Never remove unchecked items or add sections not in the template.
