---
name: commit
description: Split uncommitted changes across commits, or reorder/squash/amend existing commits
user-invocable: true
allowed-tools: Bash, AskUserQuestion, Read, Write
---

# Commit

Single entry point for all commit work.

## Triage

Run `git status --porcelain` and `git log --oneline main..HEAD` (try `master` if `main` fails).

Use AskUserQuestion to ask what the user wants to do:
- **Split** - uncommitted changes need to become multiple commits
- **Reorder/Squash/Amend** - already-committed history needs reshaping

Route to the appropriate section below.

---

## Path A: Splitting uncommitted changes

Changes are often logically coupled and need adjustments per commit to make sense in isolation.

### Phase 1: Setup

1. `~/.claude/skills/commit/snapshot.sh` - captures changed files, copies to temp dir, verifies checksums, prints temp dir path
2. Record HEAD SHA: `git rev-parse HEAD`
3. Write `<tmpdir>/plan.md` to disk with this schema:
   ```yaml
   head: <SHA>
   commits:
     - message: <commit message>
       tracked: [<modified tracked files>]
       untracked: [<new files>]
       notes: <adjustments needed for this commit to make sense in isolation>
   ```
4. Present the full plan to the user via AskUserQuestion with plan contents as preview. Do not proceed until confirmed. If user requests changes, update `plan.md` on disk and re-confirm.
5. `~/.claude/skills/commit/stash.sh <tmpdir> <tracked file> [<tracked file> ...]` - stashes tracked files, verifies stash created, then removes untracked originals
6. `git status` - confirm working tree is clean before proceeding

### Phase 2: Per-commit loop

For each commit in the plan:
1. Copy the relevant files from the temp dir into the working tree
2. Edit each file to include only the changes for this commit; verify no changes intended for later commits were lost (temp dir is the source of truth)
3. `git add <specific files>` - never use wildcards
4. `git diff --cached` - verify exactly what will be committed
5. Commit

### Phase 3: Cleanup

1. Compare `git rev-parse HEAD` to the recorded SHA - confirm expected commits were made
2. `git diff HEAD` - verify nothing unexpected remains
3. If clean: `git stash drop`, then delete the temp directory
4. If something went wrong and HEAD has not moved: `~/.claude/skills/commit/restore.sh` to recover, then fix and start over
5. If something went wrong after some commits were made: do NOT restore - ask the user how to proceed. Temp dir and stash are both available for recovery.

---

## Path B: Rewriting commit history

### Step 1: Determine range

If the user specified N commits, use that. Otherwise run `git log --oneline main..HEAD` (try `master`). Count commits. If 0, report and stop.

### Step 2: Show current order

Run `git log --oneline HEAD~N..HEAD` and display oldest-first, numbered 1 through N.

### Step 3: Ask what to do

Use AskUserQuestion:
- **Reorder** - change commit order
- **Edit** - stop at a commit to amend it
- **Squash/Fixup** - combine commits

### Step 4: Collect instructions

- **Reorder**: ask for new order by number (e.g. "3, 1, 2")
- **Edit**: ask which commit number(s) to stop at
- **Squash/Fixup**: ask which to squash into which (e.g. "squash 3 into 2")

### Step 5: Build and execute the todo script

```sh
GIT_SEQUENCE_EDITOR='cat' git rebase -i HEAD~N 2>&1
```

Parse the `pick <hash> <message>` lines. Build the new todo, write to `.git/rebase-todo.sh`:

```sh
#!/bin/sh
cat > "$1" <<'EOF'
pick abc1234 first commit
fixup def5678 second commit
EOF
```

Execute:
```sh
chmod +x .git/rebase-todo.sh
GIT_SEQUENCE_EDITOR=".git/rebase-todo.sh" git rebase -i HEAD~N
rm .git/rebase-todo.sh
```

If conflicts: report to user, do NOT resolve automatically. Tell them to run `git rebase --abort` or resolve and `git rebase --continue`.

### Step 6: Verify

Show new commit order with `git log --oneline HEAD~N..HEAD` oldest-first.

---

## Rules

- Never pipe stdin to interactive git commands (`git add -p`, `git add -i`)
- Never use wildcards with `git add`
- Never force-push without explicit user permission
- Always confirm with the user before stashing, deleting files, or any hard-to-reverse operation
- If a rebase is already in progress (`.git/rebase-merge` exists), report it and stop
