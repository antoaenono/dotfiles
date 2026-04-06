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

Run `~/.claude/skills/tangle-moi/tangle.sh` and capture the list of tangled files and the count.

### Step 3: Dry-run diff

Run `chezmoi apply -vn 2>&1` and capture the output.

If the output is empty, report "No changes to apply." and stop.

### Step 4: Analyze the diff

Write the structured analysis to a temp file: `$(mktemp /tmp/tangle-moi-analysis-XXXXXX.json)`

Produce a structured analysis with these fields:

- **Files changed**: count and names
- **Lines**: +added / -removed
- **Summary**: one sentence describing what changed
- **Warnings**: list any concerns (or "none")
- **Confidence**: HIGH / MEDIUM / LOW
- **Proceed**: yes / ask

Confidence and Proceed rules:

- Confidence is HIGH when: all modified files were in the tangle output, no deletions, no permission changes, diff is small (under ~40 lines total)
- Confidence is MEDIUM when: diff is larger but files all match tangle output, or diff is small but one unexpected file appears
- Confidence is LOW when: files appear in the diff that were not in the tangle output, or deletions are present, or permission changes, or large unexpected diff

Proceed is **yes** (auto-apply, no prompt) when confidence is HIGH.
Proceed is **ask** when confidence is MEDIUM or LOW.

Write the analysis as JSON to the temp file, e.g.:

```json
{
  "files": [".config/doom/config.el"],
  "lines": { "added": 1, "removed": 1 },
  "summary": "Added #+DATE and #+PUBLISH keywords to org-roam capture template.",
  "warnings": [],
  "confidence": "HIGH",
  "reason": "All changes match tangled files, small scope.",
  "proceed": "yes"
}
```

Then display a compact human-readable version of the analysis:

```
Files:      1 (.config/doom/config.el)
Lines:      +1 / -1
Summary:    Added #+DATE and #+PUBLISH keywords to org-roam capture template.
Warnings:   none
Confidence: HIGH — all changes match tangled files, small scope
Proceed:    auto-applying
```

Then run `~/.claude/skills/tangle-moi/validate-analysis.sh <tmpfile>` to validate the JSON and read the proceed value. If the script exits non-zero, treat it as Proceed = ask and show the error as a warning.

### Step 5: Confirm (only when Proceed = ask)

Show the full diff output, then use AskUserQuestion to ask: "Apply these changes with `chezmoi apply -v`?"

Options: "Yes, apply" / "No, abort"

If Proceed = yes, skip this step entirely.

### Step 6: Apply

Run `chezmoi apply -v 2>&1` and report the output.
