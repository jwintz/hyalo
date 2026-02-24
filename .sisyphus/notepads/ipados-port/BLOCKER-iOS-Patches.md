# iOS Patches Blocker - Critical Issue

**Status**: BLOCKING iOS native build  
**Priority**: P1 (Second only to macOS build working)  
**Date**: 2026-02-24

## Summary

The iOS patches in the feedstock have compatibility issues with the current Emacs source tree. Patches applied successfully but compilation fails due to missing type definitions and incomplete patch coverage.

## Errors Encountered

### 1. Missing Type Definitions in iosterm.h
```
error: unknown type name 'Emacs_Cursor'; did you mean 'Emacs_Color'?
error: unknown type name 'Window'
error: unknown type name 'CGRect'
```

**Location**: `src/iosterm.h:580, 587, 590, 614`

**Root Cause**: The iOS window system patches define iOS-specific types but don't provide the full set of types needed by the Emacs display system. The patches were written for an older Emacs version and haven't been updated.

### 2. Malformed Patch
**File**: `patches/ios-font-driver.patch`
- Malformed at line 409
- Prevents full font driver patch from applying

## What's Working

- ✅ All patches APPLY successfully (no rejection errors)
- ✅ Configuration completes
- ✅ Build starts and compiles many files
- ❌ Fails during object compilation due to missing types

## Affected Files

The following source files fail to compile:
- `src/dispnew.c` - Needs display info types
- `src/frame.c` - Needs frame output types  
- `src/xdisp.c` - Needs cursor and window types
- `src/window.c` - Needs window system types
- `src/menu.c` - Needs menu types
- `src/coding.c` - Indirectly affected

## Required Actions

1. **Update iOS patches** to match current Emacs source:
   - Add missing type definitions to `iosterm.h`
   - Fix `ios-font-driver.patch` formatting
   - Update patches to use current Emacs APIs

2. **Alternative**: Use older Emacs version that patches were written for

3. **Workaround**: Continue using mock data/stub approach for UI testing

## Test Commands

```bash
# Clean build directory
cd ~/Syntropment/hyalo-feedstock-unified
pixi run clean-builds

# Apply patches
pixi run ios_sim_prep
pixi run ios_patch

# Install iOS sources
pixi run ios_install_src

# Configure
pixi run ios_sim_configure

# Build (will fail with type errors)
pixi run ios_sim_build
```

## Evidence

See `.sisyphus/evidence/` for detailed build logs showing:
- Patch application success
- Configuration success
- Compilation failure with type errors

## Recommendation

**Short term**: Document this as a known limitation and continue with stub-based testing.

**Medium term**: Someone with Emacs C development experience needs to update the patches for current Emacs source compatibility.

**Long term**: Upstream the iOS patches to Emacs project for official support.
