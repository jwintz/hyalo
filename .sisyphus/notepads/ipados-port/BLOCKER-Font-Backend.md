# BLOCKER: No Font Backend Available (iOS Simulator Crash)

## Problem

The iOS Simulator app crashes immediately after Emacs initializes with the fatal error:
```
error ("No font backend available")
```

## Root Cause

In `emacs/src/frame.c`, the function `font_update_drivers()` returns `nil` when no font backend is registered:

```c
new_value = font_update_drivers (f, NILP (new_value) ? Qt : new_value);
if (NILP (new_value))
{
    if (NILP (old_value))
        error ("No font backend available");  // <-- CRASH HERE
    ...
}
```

## Investigation

1. **Font Driver Registration**: Font drivers are registered via `register_font_driver()`:
   - macOS: `nsfns.m` calls `register_font_driver (&nsfont_driver, f)` 
   - Linux: `pgtkfns.c` registers `ftcrfont_driver` and `ftcrhbfont_driver`
   - Android: Has `androidfns.c` with font registration
   
2. **iOS Issue**: 
   - `iosfns.o` exists in build artifacts
   - No `iosfns.c` or `iosfns.m` source file found
   - iOS term stubs exist (`ios-termstubs.o`)
   - No iOS-specific font driver registration code found

3. **Makefile Configuration**:
   ```
   IOS_OBJC_OBJ=iosterm.o iosfns.o macfont.o
   ```
   iOS reuses `macfont.o` from macOS but lacks proper initialization.

## Impact

- **Build**: SUCCEEDS (no compilation errors)
- **Install**: SUCCEEDS (app installs in simulator)
- **Launch**: SUCCEEDS (app launches and starts Emacs)
- **Emacs Init**: SUCCEEDS (loadup.el completes)
- **Window System**: FAILS (crashes at frame creation)

## Solution Required

The feedstock build (`~/Syntropment/hyalo-feedstock-unified`) needs:

1. **iOS Font Driver Implementation**:
   - Create `iosfont.c` or modify existing iOS files
   - Register a font driver using `register_font_driver()`
   - Can reuse macOS font code but needs proper initialization

2. **Build Configuration**:
   - Ensure `iosfns.c`/`iosfns.m` properly calls font registration
   - Link against iOS CoreText framework
   - Configure font backend in iOS window system initialization

## Files Affected

- Feedstock: `emacs/src/iosfns.c` (needs creation/modification)
- Feedstock: `emacs/src/Makefile` (needs iOS font rules)
- Feedstock: `emacs/configure.ac` (needs iOS font detection)

## References

- `frame.c:5707` - `gui_set_font_backend()` where crash occurs
- `nsfns.m:1314-1316` - macOS font driver registration example
- `pgtkfns.c` - Linux font driver registration example
- `font.c:130` - Font driver system documentation

## Status

**Fix implemented -- awaiting confirmation.**

### Fixes Applied (2026-02-24)

Three root causes identified and fixed:

1. **Missing `syms_of_macfont()` call in `syms_of_iosterm()`**: On macOS, `syms_of_nsterm()` calls `syms_of_macfont()` (nsterm.m:11535) which sets `macfont_driver.type = Qmac_ct` and registers the font driver globally. On iOS, `syms_of_iosterm()` had no such call. Fixed by adding `syms_of_macfont();` to `syms_of_iosterm()` in `ios/iosterm.m`.

2. **`syms_of_macfont` declaration hidden behind `#ifdef HAVE_NS` in `font.h`**: The declaration was only visible under `HAVE_NS` (macOS), not `HAVE_IOS`. Fixed by adding a parallel `#ifdef HAVE_IOS` block in `font.h`.

3. **No patches were applied to the Emacs source tree**: The existing `libemacs.a` was built from an unpatched source tree. All 30 patches from `patches/` (including `ios-emacs-entry.patch`, `ios-font-driver.patch`, `ios-dispnew.patch`) were never applied. Fixed by using the pixi build workflow (`ios_sim_prep` -> `ios_patch` -> `ios_install_src` -> `ios_sim_configure` -> `ios_sim_build` -> `ios_sim_build_libemacs`).

Additional fixes during rebuild:
- `iosdispatch.h`: Missing `#endif` closings; nested `*/` in block comment
- `macfont.m`: Unterminated `#ifdef HAVE_IOS` in `macfont_draw`; iOS-specific drawing path added
- `macfont.m`: `CTFontManagerCompareFontFamilyNames` unavailable on iOS; replaced with `CFStringCompare`
- `macfont.m`: `macfont_get_glyph_for_cid` call not guarded by `#ifndef HAVE_IOS`
- `iosterm.m`: `ios_set_main_emacs_view` strong symbol conflicting with Swift @_cdecl; changed to extern declaration
- `install-ios-src.sh`: Missing `iosdispatch.h` in file list
- `project.yml`: Updated to reference `emacs-build-ios-sim/src/libemacs.a` instead of `emacs/src/libemacs.a`

### Verification

Screenshot shows Emacs rendering text on iPad Simulator via the macfont/CoreText driver. No crash.

## Evidence

- Crash log shows: `No font backend available`
- Screenshot shows: iPad home screen (app not running)
- Simulator logs: Emacs initializes successfully up to frame creation
- Build logs: No compilation errors
