# Feedstock Option C: Separate Build Directories - Decisions

## Decision: Implement Separate Build Directories

**Date**: 2026-02-23
**Rationale**: Allow macOS and iOS Simulator builds to coexist without conflict

### Architecture

```
hyalo-feedstock/
├── emacs/                    # Pristine git submodule (never touched)
├── emacs-build-macos/        # macOS build (rsynced + patched)
├── emacs-build-ios-sim/      # iOS Simulator build (rsynced + patched)
└── pixi.toml                 # Tasks use respective build dirs
```

### Task Naming Convention

- `mac_*` tasks for macOS builds
- `ios_sim_*` tasks for iOS Simulator builds
- Legacy tasks without prefix delegate to macOS (backward compatibility)

### Key Implementation Details

1. **Prep Tasks**: `mac_prep` and `ios_sim_prep` use rsync to copy from emacs/ to build directories
2. **Patch Tasks**: Apply patches to respective build directories (no `git checkout .` needed)
3. **Legacy Aliases**: Original task names (patch, build, install, etc.) delegate to macOS tasks
4. **Clean Tasks**: Remove and recreate build directories rather than git checkout

### Benefits

- Original emacs/ remains pristine
- Both platforms can build simultaneously
- No git conflicts or patch reversals
- Clean separation of concerns

### Files Modified

- `pixi.toml` - Complete restructure of task definitions

### Evidence

- Diff saved to: `.sisyphus/evidence/feedstock-option-c.diff`
- Test results: `.sisyphus/evidence/feedstock-separation-test.txt`
