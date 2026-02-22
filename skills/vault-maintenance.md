# Hyalo Vault Maintenance

## When to Update the Vault

Update vault documentation whenever:

| Code change | Vault file to update |
|-------------|----------------------|
| New `init/` module added | `vault/2.init/` — add new file, update index |
| `init/` module changed | `vault/2.init/N.module-name.md` |
| New `lisp/` file added | `vault/3.lisp/` — add new file, update index |
| `lisp/` function or var changed | `vault/3.lisp/N.module-name.md` |
| New Swift module added | `vault/4.swift/` — add new file, update index |
| Swift API changed | `vault/4.swift/N.module-name.md` |
| New feature added | `vault/1.features/` — add or update relevant page |
| Release prepared | `vault/Changelog.md` — move Unreleased → new version |

## Changelog Maintenance

1. All new changes go under `## Unreleased` in `vault/Changelog.md`
2. Update the `:changelog-versions` component's JSON when releasing:
   - Move `Unreleased` entries to a new `## vX.Y.Z - YYYY-MM-DD` section
   - Add the version to the JSON array with `title`, `date`, `description`, `to`
3. Keep `CHANGELOG.md` (repo root) in sync — it is the machine-readable source

## Sync with `CHANGELOG.md`

`vault/Changelog.md` is the human-readable presentation layer. The repo-root `CHANGELOG.md` is the canonical source. They should contain the same information. When updating the repo changelog, mirror the changes to the vault page.

## Adding a New Init Module

1. Create `vault/2.init/N.module-name.md` (use next available number)
2. Update `vault/2.init/index.md` load order list
3. Update `vault/1.features/3.init-system.md` load order list if shown

## Adding a New Lisp Module

1. Create `vault/3.lisp/N.module-name.md`
2. Update `vault/3.lisp/index.md` module table
3. Update `vault/1.features/4.lisp-side.md` if the module is significant

## Adding a New Swift Module

1. Create `vault/4.swift/N.module-name.md`
2. Update `vault/4.swift/index.md` architecture diagram and module reference
3. Update `vault/1.features/5.swift-side.md` layout diagram if needed

## Testing the Vault Locally

```bash
cd ~/Syntropment/lithos
npm run dev -- --vault ~/Syntropment/hyalo/vault
```

Visit `http://localhost:3000`.
