# CRITICAL: Reproducible Build Requirements

## DO NOT EDIT `~/Syntropment/hyalo-feedstock-unified/emacs/` DIRECTLY

The `emacs/` directory is a git submodule. Any manual edits will be LOST when:
- `pixi run unpatch` is executed (runs `git checkout .`)
- `pixi run ios_patch` is executed (starts with `git checkout .`)
- `pixi run distclean` is executed

## MANDATORY WORKFLOW for Feedstock Changes

### 1. Patches
All changes to emacs source MUST be done via patches in `~/Syntropment/hyalo-feedstock-unified/patches/`
- Create/edit patch files in the `patches/` directory
- Add patch to `pixi.toml` ios_patch task sequence
- Run `pixi run ios_patch` to apply

### 2. Lisp Files
All lisp changes MUST be done via patches or copied during resource copy
- Do not edit `emacs/lisp/` directly
- Use patches that modify lisp files
- Or copy modified lisp files to iOS app resources AFTER build

### 3. Build Commands
Use ONLY pixi tasks for building:
```bash
pixi run ios_patch         # Apply all patches
pixi run ios_sim_build     # Build temacs
pixi run ios_sim_build_libemacs  # Create libemacs.a
```

## Consequences of Violation
Manual edits to `emacs/` directory will be lost and break reproducibility.

## Evidence Requirement
All builds must be reproducible from clean state using pixi tasks only.

## Current Status
- Task 4 achieved breakthrough with Emacs booting on iOS
- However, manual edits were made to emacs/ directory
- These changes need to be captured as patches for reproducibility
- Next: Create proper patches from the working state and document the exact pixi workflow
