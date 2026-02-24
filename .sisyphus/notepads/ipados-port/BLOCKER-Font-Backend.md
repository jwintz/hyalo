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

**BLOCKED** - Requires feedstock modifications outside hyalo-unified scope.

## Evidence

- Crash log shows: `No font backend available`
- Screenshot shows: iPad home screen (app not running)
- Simulator logs: Emacs initializes successfully up to frame creation
- Build logs: No compilation errors
