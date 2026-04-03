---
name: tangle-moi
description: Tangle chezmoi config.org then dry-run and optionally apply via chezmoi
user-invocable: true
allowed-tools: Bash, AskUserQuestion
---

# Tangle Moi

Mirrors the `M-x tangle-moi` Emacs command in the chezmoi dotfiles repo.

## Process

### Step 1: Ensure tangle targets exist

Run `~/.claude/skills/tangle-moi/ensure-dirs.sh`.

### Step 2: Tangle

Run `~/.claude/skills/tangle-moi/tangle.sh` and report how many files were tangled.

### Step 3: Dry-run diff

Run `chezmoi apply -vn 2>&1` and capture the output.

If the output is empty, report "No changes to apply." and stop.

Otherwise, display the full diff output to the user.

### Step 4: Confirm

Use AskUserQuestion to ask: "Apply these changes with `chezmoi apply -v`?"

Options: "Yes, apply" / "No, abort"

### Step 5: Apply (if confirmed)

Run `chezmoi apply -v 2>&1` and report the output.
