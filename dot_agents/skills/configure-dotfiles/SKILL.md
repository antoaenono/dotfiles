---
name: configure-dotfiles
description: Safely maintain personal dotfiles in the chezmoi-managed, Org-babel-tangled source repository. Use from any conversation or working directory to inspect, add, change, tangle, preview, or apply machine configuration.
---

# Configure Dotfiles

## Model

Run `chezmoi source-path` to locate the source repository. Use that directory as
the working directory for the entire task.

`config.org` is the source of truth. Files such as `dot_config/**`, `Brewfile`, and
`.chezmoiscripts/**` are tangled outputs. Keep prose and code together in the Org
document. Never edit a tangled output directly.

## Workflow

1. Inspect `git status --short`, the relevant Org section, and its current tangle
   target. Preserve unrelated user changes.
2. Edit `config.org`, including nearby prose and the exact `:tangle` target. Add a
   shebang when the target must be executable.
3. Run `~/.agents/skills/configure-dotfiles/scripts/tangle.sh`. It uses a running
   Emacs server when available and otherwise launches batch Emacs. Do not
   reproduce the underlying Emacs invocation manually.
4. Run `git diff --check`, `git status --short`, and `git diff`. Confirm every
   generated change follows from the requested Org edits.
5. Preview deployment with `chezmoi apply --dry-run --verbose --force`. On normal
   command-line workflows, use `--force` only with `--dry-run`; it reveals target
   drift without overwriting it.
6. If implementation was requested and the preview contains only intended
   changes, apply the narrowest affected target or directory with `chezmoi apply
   --verbose <target>`. The interactive `M-x tangle-moi` helper may use `--force`
   for its real apply only after showing the full forced dry-run and receiving
   explicit confirmation. This avoids a second prompt that requires a TTY.
7. Verify installed targets and report source changes, applied targets, and
   anything intentionally left unapplied.

`M-x tangle-moi` is the interactive interface to the same tangle core installed
at `~/.config/doom/lisp/tangle-moi.el`.

## Stop conditions

Stop if the tangle script reports unsaved changes in an Emacs buffer visiting
`config.org`; preserve that buffer and ask the user to resolve it. Ask before
applying when the preview would overwrite independently modified home files,
delete targets, change permissions unexpectedly, execute package or setup
scripts, or include changes outside the request. Do not apply when the user asked
only for review or diagnosis.

## Conventions

- Use chezmoi naming conventions for destination paths (`dot_`, `private_`,
  `symlink_`, and `.tmpl`).
- Keep platform-specific scripts under `.chezmoiscripts/<os>/` and guard them
  appropriately.
- Keep secrets and host-specific credentials out of the repository.
- Prefer idempotent scripts with `set -e` and explicit paths.
- Update nearby prose when behavior or setup instructions change.

## Validation checklist

- The Org source parses and tangles successfully.
- The generated diff contains no unrelated changes.
- Templates and scripts remain executable where intended.
- The chezmoi dry-run reports only expected target changes.
