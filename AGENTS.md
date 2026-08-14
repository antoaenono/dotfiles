# Dotfiles Repository Guidance

## Source of Truth

- `config.org` is a literate configuration file and the authoritative source for configuration and documentation.
- Do not edit tangled files directly. Instead, configuration code should be tangled from an Org code block.
- Keep generated paths and their `:tangle` targets aligned.
- Prose outside Org source blocks is human-authored and must not be edited by agents.

## Change workflow

1. Write configuration code only in Org source blocks in `config.org`. Ask the user to make any needed prose or heading changes.
2. Follow the globally installed `$configure-dotfiles` skill to tangle, review, and apply the change.
3. Review the generated diff, including file modes and templates.
4. Run the relevant validation or dry-run before applying with `chezmoi apply`.

## Safety

- Preserve existing user changes and inspect unexpected diffs before overwriting anything.
- Never place secrets, tokens, private keys, or machine-specific credentials in this repository.
- Prefer small, reversible changes and document non-obvious platform assumptions.

## Agent skill

For dotfiles changes, follow `dot_agents/skills/configure-dotfiles/SKILL.md`.
