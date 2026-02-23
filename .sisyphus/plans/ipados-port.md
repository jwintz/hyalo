# Hyalo iPadOS Port - Sisyphus Work Plan

## TL;DR

> **Quick Summary**: Complete the iPadOS port of Hyalo by building libemacs.a for simulator, linking it into the Xcode project, implementing the Lisp-to-Swift channel bridge, adapting init files for iOS, and polishing the SwiftUI shell. Phases 1-4.5 are done; this plan covers Phases 5-9.
> 
> **Deliverables**:
> - libemacs.a built and linked for iOS Simulator (and optionally device)
> - Emacs booting inside the SwiftUI shell with text rendering in UIView
> - Bidirectional Lisp/Swift channel bridge via single-dispatch DEFUN
> - init-hyalo.el adapted for `(eq window-system 'ios)` with bundled packages
> - iPad multitasking, appearance, mock data, and command palette polish
> 
> **Estimated Effort**: XL (multi-week, cross-repository)
> **Parallel Execution**: YES - 4 waves
> **Critical Path**: T1 (build libemacs.a) -> T3 (link in Xcode) -> T4 (bootstrap Emacs) -> T8 (feedstock DEFUN patch) -> T9 (Swift dispatch router) -> T10 (Lisp bridge) -> T11 (reverse channel) -> T13 (init-hyalo.el adaptation)

---

## Context

### Original Request
Convert the existing PLAN.md (Hyalo iPadOS Port Implementation Plan) into an executable Sisyphus work plan. PLAN.md is the source of truth and must not be edited. Only OPEN phases (5.2 through 9.5) are converted into actionable TODOs.

### Project Background
Hyalo is an Emacs IDE shell. On macOS, Emacs is the host process loading Hyalo.dylib as a dynamic module. For iPadOS, the relationship inverts: a SwiftUI app is the host, embedding HyaloKit as a dynamic framework that statically links libemacs.a. The feedstock at `~/Syntropment/hyalo-feedstock-unified` builds libemacs.a and all dependencies for iOS.

Phases 1-4.5 are DONE:
- Phase 1: Package structure and platform abstraction (HyaloShared/HyaloMac/HyaloiOS split)
- Phase 2: HyaloMac compilation fixes
- Phase 3: Xcode project with XcodeGen, HyaloEmacsStubs for simulator
- Phase 4: SwiftUI shell integration (UIView bridge, keyboard, lifecycle)
- Phase 4.5: Build system cleanup (canImport guards, derivedData consolidation)

### Cross-Repository Coordination Protocol
This plan spans TWO repositories:
- `~/Syntropment/hyalo-unified` (Swift/Lisp, main repo)
- `~/Syntropment/hyalo-feedstock-unified` (C/Emacs build, feedstock)


**WARNING**: `~/Syntropment/hyalo-feedstock` is a DIFFERENT repository. DO NOT TOUCH IT.
Only `~/Syntropment/hyalo-feedstock-unified` is used for this project.

**CRITICAL: NEVER COMMIT BINARY FILES**
- NEVER commit `.o`, `.a`, `.dylib`, `.app`, `.framework`, `.png`, `.jpg`, `.pdf` files
- These are build artifacts and should NEVER be in the repository
- Always run `git rm --cached <file>` and add to `.gitignore` if accidentally staged
- This rule is MANDATORY - violating it blocks all work until fixed

Feedstock changes must be:
1. Applied directly to feedstock working directory
2. Documented as patch descriptions in task evidence
3. Verified by rebuilding libemacs.a after patching
4. NOT auto-committed to feedstock — human reviews and commits feedstock changes

### Metis Review
**Identified Gaps** (addressed):
- Cross-repo state synchronization: solved via explicit build-artifact verification commands in each task
 Visual verification limitations: solved via `xcrun simctl ui` accessibility dump + screenshot evidence
- Device testing gate: Phase 6 marked conditional (requires physical iPad + Apple Developer cert)
- Feedstock patch management: each feedstock task documents exact changes and verification
- pdmp fingerprint mismatch: explicit fallback to bootstrap-from-source documented
- Reverse channel queue bounds: addressed in task 11 acceptance criteria


### Current State (2026-02-23)

**Wave 1 DONE**: T1 (libemacs.a built), T2 (mock data), T5 (device lib check), T14 (appearance)
**Wave 2 DONE**: T3 (libemacs.a linked in simulator), T6 (device build config), T15 (multitasking test)
**Wave 3 BLOCKED**: T4 (Emacs bootstrap) is the GATE task -- everything else depends on it.

#### Task 4 Blocker: Unapplied Feedstock Patches

**Problem**: Emacs terminates with `"standard input is not a tty"` during `init_display_interactive()` in `dispnew.c`.

**Root Cause**: 15 iOS patches in `~/Syntropment/hyalo-feedstock-unified/patches/` were never applied to the emacs source tree. The critical missing patch is `ios-dispnew.patch`, which adds:
```c
#ifdef HAVE_IOS
  if (!inhibit_window_system && ios_init_gui)
    {
      Vinitial_window_system = Qios;
      ios_term_init ();
      return;
    }
#endif
```
Without this block, Emacs falls through all window-system checks (HAVE_X_WINDOWS, HAVE_NS, HAVE_ANDROID, etc.) to the tty check, which fails because iOS has no tty.

**All 15 unapplied patches**:
- `ios-dispnew` -- CRITICAL: init_display sets Qios, calls ios_term_init()
- `ios-terminal` -- terminal-live-p needs output_ios case
- `ios-bidi` -- bidi processing safety during early startup
- `ios-bootstrap-progress` -- progress callbacks in lread.c
- `ios-checkstring` -- nil guard in CHECK_STRING
- `ios-compat` -- nil directory fallback in buffer.c
- `ios-cus-edit-lisp` -- customization patches
- `ios-debug` -- nil guards in fileio.c, data.c, search.c
- `ios-epaths` -- path configuration
- `ios-faces-lisp` -- faces.el patches
- `ios-frame-lisp` -- frame.el patches
- `ios-libemacs` -- Makefile for libemacs.a
- `ios-macroexp` -- macroexp.el patches
- `ios-minibuffer-timerp` -- minibuffer timer
- `ios-try-window` -- display safety during early startup

**Fix implemented -- awaiting confirmation**: All 15 patches applied via `git apply --directory=emacs` to `~/Syntropment/hyalo-feedstock-unified`. Rebuild of libemacs.a required.

**Next step**: Use `pixi run ios_patch` for the canonical patch workflow (it starts with `git checkout .` then applies ALL patches). Then rebuild: `pixi run ios_sim_build && pixi run ios_sim_build_libemacs`.

### Feedstock Build Strategy

The feedstock at `~/Syntropment/hyalo-feedstock-unified` supports BOTH macOS and iOS builds via `pixi.toml` task families. The emacs source tree is a git submodule that can only hold ONE active configuration at a time.

#### Task Families

| Family | Prefix | Purpose |
|--------|--------|---------|
| macOS | `mac_*` | Patch, configure with `--with-ns`, build Emacs.app |
| iOS Shared | `ios_*` | Patch (superset of mac), autogen, install sources, copy resources |
| iOS Simulator | `ios_sim_*` | Configure for arm64-apple-ios17.0-simulator, build temacs, create libemacs.a |
| iOS Device | `ios_device_*` | Configure for arm64-apple-ios17.0, build temacs, create libemacs-device.a |

#### Switching Between Targets

```bash
# Switch to macOS build
pixi run unpatch           # git checkout . in emacs submodule
pixi run mac_patch         # apply macOS patches
pixi run mac_configure     # configure with --with-ns
pixi run mac_build         # build Emacs.app

# Switch to iOS Simulator build
pixi run unpatch           # git checkout . in emacs submodule
pixi run ios_patch         # apply ALL iOS patches (superset of mac)
pixi run ios_sim_configure # configure with --with-ios for simulator SDK
pixi run ios_sim_build     # build temacs for simulator
pixi run ios_sim_build_libemacs  # create libemacs.a
```

**Key constraint**: `pixi run unpatch` (or each patch task's `git checkout .`) reverts ALL patches. You must re-apply patches and reconfigure when switching targets. The build artifacts (temacs, libemacs.a, config.status) are overwritten.

**iOS patches are safe for macOS**: All iOS patches use `#ifdef HAVE_IOS` guards, which is only defined by `./configure --with-ios`. If the same source tree has iOS patches applied but is configured for macOS (`--with-ns`), the iOS code is inert.

---

## Work Objectives

### Core Objective
Make Emacs boot and render inside the iPadOS SwiftUI shell, with bidirectional communication between Emacs Lisp and Swift via the channel bridge.

### Concrete Deliverables
- `libemacs.a` for iOS Simulator (arm64-apple-ios17.0-simulator, platform 7)
- Updated `iOS/project.yml` linking real libemacs.a instead of stubs
- Emacs bootstrapping in simulator (pdmp load, lisp/ resources found)
- `hyalo-ios-dispatch` DEFUN in feedstock `iosfns.m`
- `ChannelBridge.dispatch()` Swift router with handler dictionary
- `hyalo-channels-ios.el` rewritten to use single dispatch
- `hyalo_ios_pop_command` reverse channel (Swift -> Emacs)
- `init-hyalo.el` with iOS conditional blocks
- `init-bootstrap-ios.el` or gated package blocks
- Mock data for simulator visual testing
- iPad multitasking and appearance system verification

### Definition of Done
- [ ] `xcodebuild -scheme HyaloApp -destination 'platform=iOS Simulator'` builds clean
- [ ] `xcrun simctl launch $UDID org.gnu.hyalo` boots without crash
- [ ] Console shows Emacs loading lisp files (no void-function errors)
- [ ] Screenshot shows text rendered in editor area
- [ ] `(hyalo-ios-dispatch "test" "{}")` evaluates to nil without crash
- [ ] Buffer list appears in navigator sidebar via channel bridge
- [ ] Tapping a buffer in navigator switches Emacs buffer (reverse channel)
- [ ] `swift build --target Hyalo` still passes (macOS regression check)

### Must Have
- libemacs.a linked in simulator build (replacing HyaloEmacsStubs)
- Single-dispatch DEFUN (`hyalo-ios-dispatch`) in feedstock
- Swift dispatch router matching all existing macOS channel functions
- iOS-conditional init file loading
- All macOS builds remain unbroken

### Must NOT Have (Guardrails)
- DO NOT modify PLAN.md — it is read-only source of truth
- DO NOT modify macOS-specific code in Sources/HyaloMac/ during iOS work
- DO NOT add new dependencies to Package.swift without explicit approval
- DO NOT change channel names or JSON payload formats (must match existing macOS protocol)
- DO NOT change Emacs Lisp API signatures (must remain compatible with macOS path)
- DO NOT delete HyaloEmacsStubs until real libemacs.a links successfully on simulator
- DO NOT commit to hyalo-feedstock-unified — document patches, human commits
- DO NOT add features beyond what exists in the macOS version
- DO NOT add animations or polish not in existing macOS version
- DO NOT use polling — hooks and advices only (per AGENTS.md)
- DO NOT skip platform verification (otool -l) after building libemacs.a
- DO NOT proceed past Task 4 if Emacs does not boot in simulator

---

## Verification Strategy

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed. No exceptions.
> Acceptance criteria requiring "user manually tests/confirms" are FORBIDDEN.
> Visual verification uses `xcrun simctl ui $UDID dump-state` for accessibility hierarchy + screenshots as evidence.

### Test Decision
- **Infrastructure exists**: NO (no test framework configured)
- **Automated tests**: None — this is a build/integration/bridge project
- **Framework**: N/A
 **Verification method**: Build commands, simctl launch, console log inspection, simctl ui dump-state, screenshots

### QA Policy
Every task MUST include agent-executed QA scenarios.
Evidence saved to `.sisyphus/evidence/task-{N}-{scenario-slug}.{ext}`.

- **Build tasks**: Use Bash — run build command, check exit code, verify output artifacts with `file`, `otool -l`, `lipo -archs`
- **Simulator tasks**: Use Bash — `xcrun simctl install/launch`, capture console logs, take screenshots
 **UI verification**: Use Bash — `xcrun simctl ui $UDID dump-state | jq` for structured element checks + `xcrun simctl io screenshot`
- **Lisp evaluation**: Use Bash — `xcrun simctl launch --console` and check for specific log patterns
- **Feedstock patches**: Use Bash — `grep` for expected code in patched files, rebuild, verify no errors

### Simulator Test Loop Reference
```bash
UDID="65B71F4A-5614-485E-A5EF-EEAD2D35D1E9"
cd iOS
swift run --package-path .. xcodegen generate
xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp \
  -destination "platform=iOS Simulator,id=$UDID" \
  -derivedDataPath /tmp/hyalo-ios-build build
xcrun simctl install $UDID /tmp/hyalo-ios-build/Build/Products/Debug-iphonesimulator/Hyalo.app
xcrun simctl launch $UDID org.gnu.hyalo
sleep 2
xcrun simctl io $UDID screenshot /tmp/hyalo-verify.png
xcrun simctl spawn $UDID log show --predicate 'processImagePath CONTAINS "Hyalo"' --last 30s --style compact
```

---

## Execution Strategy

### Parallel Execution Waves

> Maximize throughput by grouping independent tasks into parallel waves.
> Each wave completes before the next begins.
> Cross-repo tasks explicitly document which repo they operate on.

```
Wave 1 (Foundation — feedstock build + simulator polish):
├── Task 1: Build libemacs.a for Simulator [deep] — FEEDSTOCK REPO
├── Task 2: Mock data for visual testing [quick] — hyalo-unified
├── Task 5: Device library verification [quick] — FEEDSTOCK REPO
└── Task 14: Appearance system [quick] — hyalo-unified

Wave 2 (Integration — link + bootstrap + device config):
├── Task 3: Link real libemacs.a in simulator build (depends: T1) [deep] — hyalo-unified + FEEDSTOCK
├── Task 6: Device build configuration (depends: T5) [unspecified-high] — hyalo-unified
└── Task 15: iPad multitasking testing (depends: T2) [unspecified-high] — hyalo-unified

Wave 3 (Emacs alive — bootstrap + channel bridge):
├── Task 4: Emacs bootstrap in simulator (depends: T3) [deep] — hyalo-unified + FEEDSTOCK
├── Task 7: Device testing (depends: T6, conditional) [unspecified-high] — physical iPad
├── Task 8: Feedstock patch: single dispatch DEFUN (depends: T4) [deep] — FEEDSTOCK REPO
├── Task 16: Command palette testing (depends: T2) [quick] — hyalo-unified
└── Task 17: Terminal integration assessment (depends: T2) [quick] — hyalo-unified

Wave 4 (Channel bridge + init — sequential chain):
├── Task 9: Swift dispatch router (depends: T8) [deep] — hyalo-unified
├── Task 10: Lisp bridge update (depends: T9) [deep] — hyalo-unified
├── Task 11: Reverse channel: Swift-to-Emacs (depends: T10) [deep] — hyalo-unified + FEEDSTOCK
├── Task 12: init-hyalo.el iOS adaptation (depends: T10) [unspecified-high] — hyalo-unified
└── Task 13: init-bootstrap.el iOS path (depends: T12) [unspecified-high] — hyalo-unified

Wave FINAL (After ALL tasks — independent review, 4 parallel):
├── Task F1: Plan compliance audit (oracle)
├── Task F2: Code quality review (unspecified-high)
├── Task F3: Real manual QA (unspecified-high)
└── Task F4: Scope fidelity check (unspecified-high)

Critical Path: T1 → T3 → T4 → T8 → T9 → T10 → T11 → T12 → T13 → F1-F4
Parallel Speedup: ~40% faster than strict sequential
Max Concurrent: 4 (Wave 1)
```

### Dependency Matrix

| Task | Depends On | Blocks | Wave |
|------|-----------|--------|------|
| T1 | — | T3 | 1 |
| T2 | — | T15, T16, T17 | 1 |
| T5 | — | T6 | 1 |
| T14 | — | — | 1 |
| T3 | T1 | T4 | 2 |
| T6 | T5 | T7 | 2 |
| T15 | T2 | — | 2 |
| T4 | T3 | T8 | 3 |
| T7 | T6 | — | 3 (conditional) |
| T8 | T4 | T9 | 3 |
| T16 | T2 | — | 3 |
| T17 | T2 | — | 3 |
| T9 | T8 | T10 | 4 |
| T10 | T9 | T11, T12 | 4 |
| T11 | T10 | — | 4 |
| T12 | T10 | T13 | 4 |
| T13 | T12 | — | 4 |

### Agent Dispatch Summary

| Wave | Count | Tasks |
|------|-------|-------|
| 1 | 4 | T1 `deep`, T2 `quick`, T5 `quick`, T14 `quick` |
| 2 | 3 | T3 `deep`, T6 `unspecified-high`, T15 `unspecified-high` |
| 3 | 5 | T4 `deep`, T7 `unspecified-high` (conditional), T8 `deep`, T16 `quick`, T17 `quick` |
| 4 | 5 | T9 `deep`, T10 `deep`, T11 `deep`, T12 `unspecified-high`, T13 `unspecified-high` |
| FINAL | 4 | F1 `oracle`, F2 `unspecified-high`, F3 `unspecified-high`, F4 `unspecified-high` |

---

## TODOs

- [ ] 1. Build libemacs.a for iOS Simulator (Phase 5.2)

  **What to do**:
  - Navigate to `~/Syntropment/hyalo-feedstock-unified/emacs`
  - Set up the iOS Simulator cross-compilation environment (CC, CFLAGS, LDFLAGS targeting arm64-apple-ios17.0-simulator)
  - Build gnulib: `make -C lib -j$(sysctl -n hw.ncpu)`
  - Copy native tools (make-docfile, make-fingerprint) from `../native-tools/` to `lib-src/`
  - Build Emacs: `make -C src -j$(sysctl -n hw.ncpu) LIBS_TERMCAP="" bootstrap-emacs`
  - Create libemacs.a: archive all .o files from src/ + merge libgnu.a objects
  - If build fails on specific .c/.m files, fix the errors iteratively and document each fix
  - The macfont.m iOS font driver fixes from Phase 5.1 are already applied

  **Must NOT do**:
  - Do NOT modify any files in hyalo-unified repo for this task
  - Do NOT commit to feedstock — only build
  - Do NOT change build flags beyond what PLAN.md specifies

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Complex cross-compilation build with potential iterative error fixing. Requires understanding of C build systems, iOS SDK paths, and Emacs internals.
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None applicable — this is a C/make build task

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 2, 5, 14)
  - **Blocks**: Task 3 (link in Xcode)
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `PLAN.md:212-252` — Complete build commands with environment variables, SDK paths, AR commands for creating libemacs.a

  **API/Type References**:
  - `~/Syntropment/hyalo-feedstock-unified/emacs/src/macfont.m` — iOS font driver (already patched with HAVE_IOS guards)
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosgui.h` — enum ios_event_type moved outside __OBJC__ block
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosterm.h` — includes iosgui.h for event queue structs

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios-sim-deps/lib/` — Pre-built simulator dependencies (gmp, gnutls, jansson, nettle, tasn1, xml2)
  - `~/Syntropment/hyalo-feedstock-unified/native-tools/` — Host-native make-docfile and make-fingerprint

  **WHY Each Reference Matters**:
  - PLAN.md build commands are the exact recipe — follow them verbatim
  - macfont.m was the last compilation blocker (Phase 5.1) — verify it compiles cleanly
  - ios-sim-deps/lib/ must exist with all 6 libraries or linking will fail later

  **Acceptance Criteria**:
  - [ ] `~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a` exists
  - [ ] `file ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a` shows "current ar archive"
  - [ ] `otool -l ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a | grep -A2 LC_BUILD_VERSION` shows platform 7 (iossimulator), minos 17.0
  - [ ] Build completed without unresolved compilation errors

  **QA Scenarios:**

  ```
  Scenario: libemacs.a build succeeds with correct platform
    Tool: Bash
    Preconditions: ~/Syntropment/hyalo-feedstock-unified/emacs exists, ios-sim-deps/lib/ has 6 libraries
    Steps:
      1. Run: ls ~/Syntropment/hyalo-feedstock-unified/ios-sim-deps/lib/*.a | wc -l
         Expected: >= 6 (gmp, gnutls, jansson, nettle, tasn1, xml2)
      2. Run build sequence from PLAN.md Phase 5.2
      3. Run: file ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a
         Expected: contains "current ar archive"
      4. Run: otool -l ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a | head -20
         Expected: contains "platform 7" and "minos 17.0"
    Expected Result: libemacs.a is a valid ar archive with iossimulator platform markers
    Failure Indicators: "ar: no such file", "platform 2" (device instead of sim), compilation errors in build log
    Evidence: .sisyphus/evidence/task-1-libemacs-build.txt

  Scenario: Build failure produces actionable error log
    Tool: Bash
    Preconditions: Build attempted but fails
    Steps:
      1. Capture full build output to .sisyphus/evidence/task-1-build-fail.txt
      2. Grep for first error: grep -m5 'error:' .sisyphus/evidence/task-1-build-fail.txt
    Expected Result: Error messages identify specific file and line
    Evidence: .sisyphus/evidence/task-1-build-fail.txt
  ```

  **Commit**: NO (feedstock build artifact, not in hyalo-unified git)

- [ ] 2. Mock Data for Visual Testing (Phase 9.3)

  **What to do**:
  - Create `HyaloiOSModule.loadMockData()` method in `Sources/HyaloiOS/Core/HyaloiOSModule.swift`
  - Populate NavigatorManager with sample buffers (at least 3: *scratch*, main.swift, README.md)
  - Populate EditorTabViewModel with sample tabs (at least 2 tabs)
  - Populate StatusBarViewModel with sample values (line 42, column 13, utf-8, Swift mode)
  - Call `loadMockData()` from `HyaloRootView.onAppear` when `#if DEBUG` and no real Emacs running
  - Wrap everything in `#if DEBUG` and `#if canImport(UIKit)` guards
  - Verify all three panels render with data: navigator sidebar shows files, editor shows tabs, status bar shows values

  **Must NOT do**:
  - Do NOT modify any HyaloShared view model interfaces — only populate existing published properties
  - Do NOT add mock data to macOS code path
  - Do NOT create new view models or managers

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Straightforward task — creating mock data instances and populating existing view models. No architectural decisions.
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 5, 14)
  - **Blocks**: Tasks 15, 16, 17 (they need mock data to test UI)
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `Sources/HyaloShared/Navigator/NavigatorManager.swift` — BufferInfo model and updateBufferList() method
  - `Sources/HyaloShared/Editor/EditorTabViewModel.swift` — Tab model and published properties
  - `Sources/HyaloShared/StatusBar/StatusBarViewModel.swift` — Status bar published properties

  **API/Type References**:
  - `Sources/HyaloiOS/Core/HyaloiOSModule.swift:61 lines` — Main coordinator class where loadMockData() will live
  - `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift:247 lines` — HyaloRootView.onAppear is the call site

  **WHY Each Reference Matters**:
  - NavigatorManager.updateBufferList() is the exact API to populate the sidebar — use same JSON format as macOS
  - HyaloiOSModule is the singleton coordinator — loadMockData() belongs there
  - HyaloRootView.onAppear is where initialization happens — add conditional mock loading there

  **Acceptance Criteria**:
  - [ ] `swift build --target HyaloKit` compiles without errors (with canImport(UIKit) unavailable, mock code is guarded)
  - [ ] iOS simulator build succeeds
  - [ ] Simulator launch shows non-empty navigator sidebar
  - [ ] Simulator shows tab bar with 2+ tabs
  - [ ] Status bar shows line/column/encoding values

  **QA Scenarios:**

  ```
  Scenario: Mock data renders in all panels
    Tool: Bash
    Preconditions: iOS simulator running with mock data enabled (DEBUG build)
    Steps:
      1. Build and install: xcodebuild + simctl install + simctl launch
      2. Wait 3 seconds for SwiftUI to render
      3. Run: xcrun simctl io $UDID screenshot .sisyphus/evidence/task-2-mock-data.png
      4. Run: xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); print(len([e for e in d.get('elements',[]) if e.get('label')]))"
         Expected: >= 5 (at least navigator items + tabs + status elements)
    Expected Result: Screenshot shows populated sidebar, tab bar, and status bar
    Failure Indicators: Blank sidebar, empty tab bar, "0" from jq (no labeled UI elements)
    Evidence: .sisyphus/evidence/task-2-mock-data.png

  Scenario: macOS build not broken by mock code
    Tool: Bash
    Preconditions: None
    Steps:
      1. Run: swift build --target Hyalo
         Expected: Build complete! (exit code 0)
    Expected Result: macOS target compiles clean — mock code is guarded by #if canImport(UIKit)
    Failure Indicators: Compilation errors referencing mock data or UIKit types
    Evidence: .sisyphus/evidence/task-2-macos-regression.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): add mock data for simulator visual testing`
  - Files: `Sources/HyaloiOS/Core/HyaloiOSModule.swift`, `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift`
  - Pre-commit: `swift build --target Hyalo`

- [ ] 3. Link Real libemacs.a in Simulator Build (Phase 5.3)

  **What to do**:
  - Update `iOS/project.yml` simulator linker flags to link real libemacs.a instead of using stubs:
    ```yaml
    "OTHER_LDFLAGS[sdk=iphonesimulator*]":
      - "-force_load"
      - "$(SRCROOT)/../../hyalo-feedstock-unified/emacs/src/libemacs.a"
      - "-lxml2"
      - "-ljansson"
      - "-lgmp"
      - "-lgnutls"
      - "-lnettle"
      - "-ltasn1"
      - "-lz"
      - "-liconv"
      - "-framework UIKit"
      - "-framework CoreGraphics"
      - "-framework CoreText"
      - "-framework QuartzCore"
    ```
  - Update `LIBRARY_SEARCH_PATHS[sdk=iphonesimulator*]` to include `ios-sim-deps/lib/`
  - Consider removing HyaloEmacsStubs dependency from HyaloKit in Package.swift (or gate with `#if targetEnvironment(simulator)`)
    - CAUTION: Do NOT delete HyaloEmacsStubs target entirely — keep as fallback until verified
  - Regenerate Xcode project: `swift run --package-path .. xcodegen generate`
  - Build: `xcodebuild -scheme HyaloApp -destination 'platform=iOS Simulator,id=$UDID' -derivedDataPath /tmp/hyalo-ios-build build`

  **Must NOT do**:
  - Do NOT delete HyaloEmacsStubs target from Package.swift until build succeeds with real libemacs.a
  - Do NOT modify device (iphoneos) linker flags — only simulator flags
  - Do NOT change any Swift source files — this is a build config task only

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Build system integration requires understanding XcodeGen project.yml format, SPM dependency resolution, and linker flag semantics. Potential for tricky link errors.
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 2 (sequential after Task 1)
  - **Blocks**: Task 4 (Emacs bootstrap)
  - **Blocked By**: Task 1 (needs libemacs.a to exist)

  **References**:

  **Pattern References**:
  - `iOS/project.yml:58 lines` — Current linker configuration. Device flags already link libemacs.a — mirror for simulator.
  - `PLAN.md:273-303` — Exact yaml changes and action items for this phase

  **API/Type References**:
  - `Package.swift:75-85` — HyaloKit target currently depends on HyaloEmacsStubs — evaluate whether to gate or remove
  - `Sources/HyaloEmacsStubs/EmacsStubs.c:49 lines` — 7 stub functions that will be superseded by real libemacs symbols

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a` — The artifact from Task 1
  - `~/Syntropment/hyalo-feedstock-unified/ios-sim-deps/lib/` — Simulator dependency libraries

  **WHY Each Reference Matters**:
  - project.yml device flags are the template — simulator flags should mirror them with sim-specific paths
  - Package.swift HyaloEmacsStubs dependency must be handled carefully to avoid duplicate symbols
  - Feedstock paths must be correct or linker will fail with "library not found"

  **Acceptance Criteria**:
  - [ ] `xcodebuild -scheme HyaloApp -destination 'platform=iOS Simulator,id=$UDID' build` succeeds
  - [ ] No "library not found" or "symbol not found" linker errors
  - [ ] No duplicate symbol errors between libemacs.a and HyaloEmacsStubs
  - [ ] `swift build --target Hyalo` still passes (macOS regression)

  **QA Scenarios:**

  ```
  Scenario: Simulator build links real libemacs.a
    Tool: Bash
    Preconditions: Task 1 completed (libemacs.a exists with platform 7)
    Steps:
      1. Run: cd iOS && swift run --package-path .. xcodegen generate
         Expected: exit code 0, HyaloApp.xcodeproj regenerated
      2. Run: xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp -destination "platform=iOS Simulator,id=$UDID" -derivedDataPath /tmp/hyalo-ios-build build 2>&1 | tail -5
         Expected: "BUILD SUCCEEDED"
      3. Run: otool -L /tmp/hyalo-ios-build/Build/Products/Debug-iphonesimulator/Hyalo.app/Frameworks/HyaloKit.framework/HyaloKit | head -10
         Expected: Shows framework dependencies (UIKit, CoreGraphics, etc.)
    Expected Result: Clean build with real Emacs symbols linked
    Failure Indicators: "ld: library not found", "duplicate symbol", "BUILD FAILED"
    Evidence: .sisyphus/evidence/task-3-sim-build.txt

  Scenario: macOS build unaffected
    Tool: Bash
    Preconditions: project.yml and/or Package.swift modified
    Steps:
      1. Run: swift build --target Hyalo 2>&1 | tail -3
         Expected: "Build complete!"
    Expected Result: macOS dynamic module still builds
    Failure Indicators: Compilation or link errors
    Evidence: .sisyphus/evidence/task-3-macos-regression.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): link real libemacs.a in simulator build`
  - Files: `iOS/project.yml`, `Package.swift` (if changed)
  - Pre-commit: `swift build --target Hyalo`

- [x] 4. Emacs Bootstrap in Simulator (Phase 5.4)

  **What to do**:
  - Build, install, and launch the app in simulator with real libemacs.a linked
  - Verify `ios_emacs_init` runs by checking console logs for `ios_set_main_emacs_view: view=<non-null>`
  - Verify `build.sh` copies all resources into the correct bundle locations:
    - `Hyalo.app/lisp/` (Emacs runtime lisp from feedstock + hyalo-ios.el, hyalo-channels-ios.el from hyalo-unified)
    - `Hyalo.app/etc/` (Emacs data files from feedstock)
    - `Hyalo.app/init/` (Hyalo init files from hyalo-unified)
    - `Hyalo.app/bootstrap-emacs.pdmp` (portable dumper from feedstock)
  - Verify `project.yml` resources section includes `HyaloApp/Resources`
  - Verify portable dump loads: `EmacsLifecycle.start()` resolves pdmp path
    - First checks `<Documents>/emacs-<fingerprint>.pdmp` (user-specific)
    - Falls back to `<bundle>/bootstrap-emacs.pdmp` (bundled)
  - If pdmp fingerprint mismatch: document the mismatch, note feedstock rebuild needed
  - Verify `*scratch*` buffer appears, no `(void-function ...)` errors
  - GATE: If Emacs does not boot in simulator, STOP and report. Do not proceed to later tasks.

  **Must NOT do**:
  - Do not modify macOS code paths
  - Do not change `build.sh` resource copy logic unless files are provably missing
  - Do not proceed past this task if Emacs does not boot

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Requires understanding the full Emacs bootstrap sequence (C init -> pdmp load -> Lisp init) and diagnosing potential failures at each stage
  - **Skills**: []
    - No specific skills needed; this is primarily shell command verification and log analysis
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser UI involved

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (sequential gate task)
  - **Blocks**: Tasks 8, 9, 10, 11, 12, 13, 16, 17 (all post-bootstrap work)
  - **Blocked By**: Task 3 (requires real libemacs.a linked)

  **References**:

  **Pattern References**:
  - `Sources/HyaloiOS/Core/EmacsLifecycle.swift:1-137` - Full bootstrap sequence: `setupEnvironment()` sets HOME/EMACSLOADPATH/EMACSDOC/init-directory, `start()` resolves pdmp path and calls `ios_emacs_init`
  - `iOS/build.sh:1-48` - Resource copy script: copies feedstock lisp/etc/pdmp and hyalo-unified lisp/init into Xcode project resources

  **API/Type References**:
  - `Sources/HyaloiOS/Core/EmacsCInterop.swift:1-46` - C function declarations: `ios_emacs_init`, `ios_get_fingerprint`, `ios_signal_event_available`
  - `iOS/HyaloApp/BridgingHeader.h:1-20` - 6 C function declarations bridging libemacs.a to Swift

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosterm.m` - Contains `ios_set_main_emacs_view` NSLog that confirms init ran
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosfns.m` - Contains `syms_of_iosfns` which registers iOS-specific Lisp functions

  **WHY Each Reference Matters**:
  - `EmacsLifecycle.swift` is THE entry point -- understand the exact sequence of env setup -> pdmp resolution -> C init call
  - `build.sh` determines what resources end up in the bundle; if anything is missing, this is where to look
  - `EmacsCInterop.swift` and `BridgingHeader.h` show the exact C symbols expected; if link fails, check these declarations
  - feedstock `iosterm.m` NSLog is the definitive signal that Emacs init ran successfully

  **Acceptance Criteria**:

  - [ ] `xcrun simctl launch --console booted org.gnu.hyalo` produces logs containing `ios_set_main_emacs_view`
  - [ ] `ls` of the `.app` bundle confirms `lisp/`, `etc/`, `init/`, `bootstrap-emacs.pdmp` all present
  - [ ] No `(void-function ...)` errors in console output
  - [ ] If pdmp fingerprint mismatch: documented in task output with exact fingerprint values

  **QA Scenarios:**

  ```
  Scenario: Emacs init runs in simulator
    Tool: Bash
    Preconditions: Task 3 complete (libemacs.a linked), app built and installed in simulator
    Steps:
      1. Build and install: cd iOS && ./build.sh
      2. Launch: xcrun simctl launch --console booted org.gnu.hyalo 2>&1 | tee /tmp/emacs-boot.log | head -200
      3. Search log: grep -c 'ios_set_main_emacs_view' /tmp/emacs-boot.log
    Expected Result: grep returns >= 1 (init function was called with non-null view)
    Failure Indicators: grep returns 0, or log shows SIGABRT/SIGSEGV before init
    Evidence: .sisyphus/evidence/task-4-emacs-init.txt

  Scenario: Bundle resources present
    Tool: Bash
    Preconditions: App built via build.sh
    Steps:
      1. Find app: APP=$(find /tmp/hyalo-ios-build -name 'Hyalo.app' -type d | head -1)
      2. Check lisp: ls $APP/lisp/hyalo-ios.el $APP/lisp/hyalo-channels-ios.el
      3. Check init: ls $APP/init/init-hyalo.el
      4. Check etc: ls $APP/etc/ | head -3
      5. Check pdmp: ls -la $APP/bootstrap-emacs.pdmp
    Expected Result: All files exist, pdmp is non-zero size
    Failure Indicators: 'No such file or directory' for any path
    Evidence: .sisyphus/evidence/task-4-bundle-resources.txt

  Scenario: Portable dump loads (or graceful fallback)
    Tool: Bash
    Preconditions: App launched in simulator
    Steps:
      1. Launch: xcrun simctl launch --console booted org.gnu.hyalo 2>&1 | tee /tmp/pdmp-check.log
      2. Wait 10s for bootstrap to complete
      3. Search: grep -E '(Loading|pdmp|dump|fingerprint|bootstrap)' /tmp/pdmp-check.log
    Expected Result: Log shows pdmp loading or lisp-from-source bootstrap completing
    Failure Indicators: Crash during dump load, infinite loop, or no output after 30s
    Evidence: .sisyphus/evidence/task-4-pdmp-load.txt
  ```

  **Commit**: YES
  - Message: `test(ios): verify emacs bootstrap in simulator`
  - Files: (no source changes expected -- verification task; commit only if build.sh or project.yml needed fixes)
  - Pre-commit: `swift build --target Hyalo`

- [ ] 5. Device Library Verification (Phase 6.1)

  **What to do**:
  - Verify `libemacs.a` architecture: `lipo -archs ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a` (expect `arm64`)
  - Verify platform: `otool -l ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a | grep -A2 LC_BUILD_VERSION` (expect platform 2 = ios, NOT platform 7 = iossimulator)
  - Check device deps: `ls ~/Syntropment/hyalo-feedstock-unified/ios-deps/lib/` -- if empty, document what needs to be built (libxml2, jansson, gmp, gnutls, nettle, tasn1 for arm64-apple-ios26.0)
  - Document findings for downstream tasks (Task 6 depends on this)

  **Must NOT do**:
  - Do not build the dependencies yourself (document only -- feedstock changes require human commits)
  - Do not modify any feedstock files

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Pure shell verification commands, no code changes
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None relevant

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2, 14)
  - **Blocks**: Task 6 (device build config depends on knowing library state)
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `iOS/project.yml:1-58` - Lines with `OTHER_LDFLAGS[sdk=iphoneos*]` show which device libraries are expected: `-lxml2`, `-ljansson`, `-lgmp`, `-lgnutls`, `-lnettle`, `-ltasn1`

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a` - The static library to verify
  - `~/Syntropment/hyalo-feedstock-unified/ios-deps/lib/` - Expected location of device dependency libraries
  - `~/Syntropment/hyalo-feedstock-unified/ios-sim-deps/lib/` - Simulator deps (for comparison)

  **WHY Each Reference Matters**:
  - `project.yml` linker flags tell us exactly which `.a` files are needed in `ios-deps/lib/`
  - If `ios-deps/lib/` is empty, Task 6 cannot proceed until deps are built
  - Comparing with `ios-sim-deps/lib/` confirms which libraries are expected

  **Acceptance Criteria**:

  - [ ] `lipo -archs` output documented (arm64 expected)
  - [ ] `otool -l` platform confirmed (ios vs iossimulator)
  - [ ] `ios-deps/lib/` contents listed or emptiness documented
  - [ ] If deps missing: list of required libraries documented with build instructions

  **QA Scenarios:**

  ```
  Scenario: Verify libemacs.a is device-only arm64
    Tool: Bash
    Preconditions: ~/Syntropment/hyalo-feedstock-unified exists
    Steps:
      1. Run: lipo -archs ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a
      2. Run: otool -l ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a | grep -A5 LC_BUILD_VERSION | head -10
    Expected Result: lipo shows 'arm64', otool shows 'platform 2' (iOS)
    Failure Indicators: lipo shows 'x86_64' (simulator), or platform 7 (simulator)
    Evidence: .sisyphus/evidence/task-5-libemacs-arch.txt

  Scenario: Device dependencies inventory
    Tool: Bash
    Preconditions: ~/Syntropment/hyalo-feedstock-unified exists
    Steps:
      1. Run: ls -la ~/Syntropment/hyalo-feedstock-unified/ios-deps/lib/ 2>&1
      2. Run: ls -la ~/Syntropment/hyalo-feedstock-unified/ios-sim-deps/lib/ 2>&1
    Expected Result: Both listings documented; ios-deps/lib may be empty (known issue)
    Failure Indicators: ios-deps/ directory does not exist at all
    Evidence: .sisyphus/evidence/task-5-device-deps.txt
  ```

  **Commit**: NO (verification only, no code changes)


- [ ] 6. Device Build Configuration (Phase 6.2)

  **What to do**:
  - Split `LIBRARY_SEARCH_PATHS` per-SDK in `iOS/project.yml`:
    - `LIBRARY_SEARCH_PATHS[sdk=iphoneos*]`: point to `hyalo-feedstock-unified/emacs/src` and `hyalo-feedstock-unified/ios-deps/lib`
    - `LIBRARY_SEARCH_PATHS[sdk=iphonesimulator*]`: point to `hyalo-feedstock-unified/emacs/src` and `hyalo-feedstock-unified/ios-sim-deps/lib`
  - Verify `ios-deps/lib/` is populated (from Task 5 findings). If empty, document which device dependencies need to be built: libxml2, jansson, gmp, gnutls, nettle, tasn1 for `arm64-apple-ios26.0`
  - Attempt device build: `xcodebuild -scheme HyaloApp -sdk iphoneos -configuration Debug`
  - If deps missing: document the blocker clearly and mark Task 7 as conditional
  - If deps present: build should succeed with code signing (requires Apple Developer account)

  **Must NOT do**:
  - Do not build device dependencies yourself (feedstock changes require human commits)
  - Do not modify feedstock files
  - Do not change simulator-specific paths (only add device-specific SDK-conditioned paths)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Requires understanding XcodeGen project.yml syntax, SDK-conditioned build settings, and diagnosing linker errors
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None relevant

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 15)
  - **Blocks**: Task 7 (device testing depends on successful device build)
  - **Blocked By**: Task 5 (need device library verification results)

  **References**:

  **Pattern References**:
  - `iOS/project.yml:1-58` - Current build settings including `OTHER_LDFLAGS[sdk=iphoneos*]` and `LIBRARY_SEARCH_PATHS`

  **API/Type References**:
  - XcodeGen `project.yml` settings spec: SDK-conditioned keys use `[sdk=iphoneos*]` and `[sdk=iphonesimulator*]` suffixes

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios-deps/lib/` - Device dependency libraries (may be empty)
  - `~/Syntropment/hyalo-feedstock-unified/ios-sim-deps/lib/` - Simulator dependency libraries (reference)

  **WHY Each Reference Matters**:
  - `project.yml` is the single source for build configuration -- the split must be exact to avoid linker errors
  - `ios-deps/lib/` emptiness is the likely blocker for device builds
  - `ios-sim-deps/lib/` shows the expected library set that `ios-deps/lib/` should mirror

  **Acceptance Criteria**:

  - [ ] `LIBRARY_SEARCH_PATHS` split into per-SDK entries in `project.yml`
  - [ ] `xcodegen generate` succeeds with new config
  - [ ] Device build attempted -- either succeeds or blocker documented
  - [ ] If blocked: clear documentation of missing deps and required actions

  **QA Scenarios:**

  ```
  Scenario: project.yml SDK-conditioned paths are correct
    Tool: Bash
    Preconditions: iOS/project.yml modified with per-SDK LIBRARY_SEARCH_PATHS
    Steps:
      1. Run: cd iOS && swift run --package-path .. xcodegen generate
      2. Run: xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp -sdk iphoneos -configuration Debug -showBuildSettings | grep LIBRARY_SEARCH_PATHS
      3. Run: xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp -sdk iphonesimulator -configuration Debug -showBuildSettings | grep LIBRARY_SEARCH_PATHS
    Expected Result: iphoneos settings point to ios-deps/lib, iphonesimulator settings point to ios-sim-deps/lib
    Failure Indicators: Both SDKs point to same path, or paths are missing
    Evidence: .sisyphus/evidence/task-6-search-paths.txt

  Scenario: Device build attempt
    Tool: Bash
    Preconditions: project.yml updated, xcodegen generated
    Steps:
      1. Run: cd iOS && xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp -sdk iphoneos -configuration Debug build 2>&1 | tail -50
    Expected Result: Either BUILD SUCCEEDED or clear linker error identifying missing libraries
    Failure Indicators: Cryptic crash, xcodebuild hangs, or unrelated compile errors
    Evidence: .sisyphus/evidence/task-6-device-build.txt
  ```

  **Commit**: YES
  - Message: `build(ios): split LIBRARY_SEARCH_PATHS per-SDK for device builds`
  - Files: `iOS/project.yml`
  - Pre-commit: `cd iOS && swift run --package-path .. xcodegen generate`

- [ ] 7. Device Testing (Phase 6.3)

  **What to do**:
  - **CONDITIONAL**: Only proceed if Task 6 device build succeeded. If blocked on missing deps, skip this task.
  - Install app on physical iPad: `xcrun devicectl device install app --device <DEVICE_UDID> /path/to/Hyalo.app`
  - Launch: `xcrun devicectl device process launch --device <DEVICE_UDID> org.gnu.hyalo`
  - Stream logs: `xcrun devicectl device process logstream --device <DEVICE_UDID> --predicate 'processImagePath CONTAINS "Hyalo"'`
  - Verify checkpoints from PLAN.md Phase 6.3:
    - App installs without code signing errors
    - App launches, no crash at dyld (HyaloKit.framework is embedded)
    - `ios_emacs_init` runs (check for NSLog from iosterm.m)
    - `ios_set_main_emacs_view` fires (EmacsView created and handed to Swift)
    - Emacs renders into UIView (text visible in editor area)
    - Hardware keyboard input reaches Emacs
    - Status bar updates from `hyalo-sync--push`

  **Must NOT do**:
  - Do not proceed if Task 6 build failed (conditional task)
  - Do not modify source code to fix device issues (report findings only)
  - Do not modify feedstock files

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Requires physical device interaction, log analysis, and diagnosing runtime issues on-device
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant -- native app, not web

  **Parallelization**:
  - **Can Run In Parallel**: YES (conditionally)
  - **Parallel Group**: Wave 3 (with Tasks 4, 8, 16, 17)
  - **Blocks**: None (device testing is an independent verification path)
  - **Blocked By**: Task 6 (device build must succeed first)

  **References**:

  **Pattern References**:
  - `Sources/HyaloiOS/Core/EmacsLifecycle.swift` - Bootstrap sequence to verify on device
  - `Sources/HyaloiOS/Core/HyaloiOSModule.swift` - Main coordinator, `ios_set_main_emacs_view` callback

  **External References**:
  - `xcrun devicectl` -- Apple device control CLI for install, launch, log streaming
  - PLAN.md Phase 6.3 checkpoints -- the canonical list of verification items

  **WHY Each Reference Matters**:
  - `EmacsLifecycle.swift` defines the exact NSLog messages to look for in device logs
  - `HyaloiOSModule.swift` contains the `ios_set_main_emacs_view` callback that confirms Emacs handed off its view
  - `xcrun devicectl` is the CLI tool for device interaction without Xcode GUI

  **Acceptance Criteria**:

  - [ ] App installed on physical iPad without code signing errors
  - [ ] App launches without dyld crash
  - [ ] `ios_emacs_init` confirmed in device logs
  - [ ] `ios_set_main_emacs_view` confirmed in device logs
  - [ ] Emacs renders text visible in UIView
  - [ ] Hardware keyboard input reaches Emacs
  - [ ] OR: Task skipped with documented blocker from Task 6

  **QA Scenarios:**

  ```
  Scenario: Device install and launch
    Tool: Bash
    Preconditions: Task 6 device build succeeded; physical iPad connected
    Steps:
      1. Run: xcrun devicectl list devices 2>&1 | head -10
      2. Run: xcrun devicectl device install app --device <UDID> /path/to/Hyalo.app 2>&1
      3. Run: xcrun devicectl device process launch --device <UDID> org.gnu.hyalo 2>&1
    Expected Result: Device listed, app installed, process launched with PID
    Failure Indicators: 'No connected devices', code signing error, crash on launch
    Evidence: .sisyphus/evidence/task-7-device-install.txt

  Scenario: Device log verification
    Tool: Bash
    Preconditions: App launched on device
    Steps:
      1. Run: xcrun devicectl device process logstream --device <UDID> --predicate 'processImagePath CONTAINS "Hyalo"' --timeout 10 2>&1 | head -100
    Expected Result: Logs show ios_emacs_init, ios_set_main_emacs_view, no SIGABRT/SIGSEGV
    Failure Indicators: No output (app crashed before logging), or abort/signal messages
    Evidence: .sisyphus/evidence/task-7-device-logs.txt

  Scenario: Skip documentation (if blocked)
    Tool: Bash
    Preconditions: Task 6 device build FAILED
    Steps:
      1. Document: 'Task 7 skipped: device build blocked on [specific reason from Task 6]'
    Expected Result: Clear skip documentation with specific blocker
    Evidence: .sisyphus/evidence/task-7-skipped.txt
  ```

  **Commit**: NO (verification only, no code changes)
- [ ] 8. Feedstock Patch: Single Dispatch DEFUN (Phase 7.2)

  **What to do**:
  - Patch `~/Syntropment/hyalo-feedstock-unified/ios/iosfns.m` to add the single dispatch DEFUN:
    - Before `syms_of_iosfns`, add `extern void hyalo_ios_dispatch(const char *channel, const char *payload) __attribute__((weak))` forward declaration
    - Add weak fallback no-op implementation of `hyalo_ios_dispatch`
    - Add `DEFUN("hyalo-ios-dispatch", ...)` that calls `CHECK_STRING` on both args then calls `hyalo_ios_dispatch(SSDATA(channel), SSDATA(payload))`
    - In `syms_of_iosfns`, add `defsubr(&Shyalo_ios_dispatch)`
  - Rebuild libemacs.a for BOTH simulator and device (reusing the build script from Task 1)
  - Verify the new DEFUN is registered: `(hyalo-ios-dispatch "test" "{}")` should return nil without crash
  - Document the exact patch as a diff so the human can commit it to feedstock

  **Must NOT do**:
  - Do NOT git commit to hyalo-feedstock-unified (document patch only, human commits feedstock)
  - Do NOT modify any other feedstock files beyond iosfns.m
  - Do NOT change existing DEFUN signatures in iosfns.m
  - Do NOT add more than one DEFUN (the whole point is single dispatch)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: C code modification in Emacs source requires understanding of Emacs internals (Lisp_Object, CHECK_STRING, DEFUN macro, defsubr registration)
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None relevant (feedstock C code, not Swift/Lisp)

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (sequential after Task 4)
  - **Blocks**: Tasks 9, 10, 11 (all depend on the DEFUN existing in libemacs.a)
  - **Blocked By**: Task 4 (Emacs must boot in simulator before adding new DEFUNs)

  **References**:

  **Pattern References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosfns.m` - Existing DEFUN patterns in the file (look for other `DEFUN` macros and `syms_of_iosfns` function to understand placement)

  **API/Type References**:
  - PLAN.md lines 516-543 - Exact C code for the DEFUN (copy verbatim)
  - `Lisp_Object`, `CHECK_STRING`, `SSDATA`, `Qnil` - Standard Emacs C API macros

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosfns.m` - The target file; study existing DEFUNs to match style
  - `__attribute__((weak))` - GCC/Clang weak symbol: if Swift provides `hyalo_ios_dispatch` via `@_cdecl`, it overrides the fallback no-op

  **WHY Each Reference Matters**:
  - `iosfns.m` existing DEFUNs show the exact macro syntax, doc string format, and arg count conventions
  - PLAN.md code blocks are the canonical patch -- deviate only if build errors require it
  - The weak attribute is essential: without it, the linker will fail with duplicate symbols when Swift provides the real implementation

  **Acceptance Criteria**:

  - [ ] `iosfns.m` contains exactly one new DEFUN: `hyalo-ios-dispatch`
  - [ ] `syms_of_iosfns` contains `defsubr(&Shyalo_ios_dispatch)`
  - [ ] libemacs.a rebuilt for simulator (arm64 sim slice)
  - [ ] Diff saved to `.sisyphus/evidence/task-8-feedstock-patch.diff`

  **QA Scenarios:**

  ```
  Scenario: DEFUN compiles without errors
    Tool: Bash
    Preconditions: iosfns.m patched
    Steps:
      1. Rebuild feedstock: cd ~/Syntropment/hyalo-feedstock-unified && make -j$(sysctl -n hw.ncpu) 2>&1 | tail -20
      2. Verify libemacs.a updated: ls -la ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a
      3. Verify symbol: nm ~/Syntropment/hyalo-feedstock-unified/emacs/src/libemacs.a | grep hyalo_ios_dispatch
    Expected Result: Build succeeds, nm shows both weak (W) definition and DEFUN wrapper symbol (Fhyalo_ios_dispatch)
    Failure Indicators: Compilation error in iosfns.m, or nm shows no matching symbol
    Evidence: .sisyphus/evidence/task-8-build-output.txt

  Scenario: DEFUN callable from Emacs (simulator)
    Tool: Bash
    Preconditions: Rebuilt libemacs.a linked in simulator app (may need rebuild after Task 3)
    Steps:
      1. Rebuild iOS app: cd ~/Syntropment/hyalo-unified/iOS && ./build.sh
      2. Launch in simulator: xcrun simctl launch --console booted org.gnu.hyalo 2>&1 | tee /tmp/defun-test.log &
      3. Wait 15s for Emacs boot
      4. Check symbol exists: grep -i 'hyalo.ios.dispatch\|syms_of_iosfns' /tmp/defun-test.log
    Expected Result: No crash, DEFUN registered (no void-function error if called)
    Failure Indicators: SIGABRT, 'void-function hyalo-ios-dispatch' in log
    Evidence: .sisyphus/evidence/task-8-defun-test.txt
  ```

  **Commit**: NO (feedstock patch -- human commits feedstock)

- [ ] 9. Swift Dispatch Router (Phase 7.3)

  **What to do**:
  - In `Sources/HyaloiOS/Bridge/ChannelBridge.swift`, implement the central dispatch router:
    - Add `@_cdecl("hyalo_ios_dispatch")` function that converts C strings to Swift strings, dispatches to `ChannelBridge.shared.dispatch()` on main queue
    - Implement `dispatch(channel: String, payload: String)` method with handler dictionary
    - Create handler dictionary mapping ~25 channel names to handler methods (see PLAN.md lines 585-611 for complete list)
    - Refactor existing `@_cdecl` function bodies into handler methods (they already contain the logic)
    - Keep existing `@_cdecl` functions as fallback (do NOT delete them yet)
  - Add `hyalo_ios_dispatch` stub to `Sources/HyaloEmacsStubs/EmacsStubs.c` (no-op for simulator-without-emacs builds)
  - Add `hyalo_ios_dispatch` declaration to `iOS/HyaloApp/BridgingHeader.h` if needed

  **Must NOT do**:
  - Do NOT delete existing `@_cdecl` functions (keep as fallback until dispatch is proven)
  - Do NOT change function signatures of existing handlers
  - Do NOT add new channel names not in PLAN.md (scope boundary)
  - Do NOT change thread dispatch pattern (must go through `DispatchQueue.main.async`)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Core architecture change to the bridge layer; requires understanding Swift @_cdecl, C interop, MainActor isolation, and existing handler logic
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `axiom:swiftui-performance`: Not relevant -- this is bridge/dispatch code, not UI
    - `axiom:liquid-glass`: Not relevant -- no UI work

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4
  - **Blocks**: Tasks 10, 11 (Lisp bridge and reverse channel depend on dispatch router)
  - **Blocked By**: Task 8 (feedstock DEFUN must exist for the @_cdecl to link against)

  **References**:

  **Pattern References**:
  - `Sources/HyaloiOS/Bridge/ChannelBridge.swift:1-*` - Existing @_cdecl functions and handler logic to refactor into dispatch methods
  - `Sources/HyaloMac/Core/Module.swift` - macOS module channel architecture for comparison (env.defun pattern vs C FFI pattern)

  **API/Type References**:
  - PLAN.md lines 556-611 - Exact Swift code for dispatch function, handler dictionary, and complete channel name list
  - `Sources/HyaloiOS/Core/HyaloiOSModule.swift` - The `@MainActor` singleton that handlers update

  **External References**:
  - `Sources/HyaloEmacsStubs/EmacsStubs.c` - Needs new stub: `void hyalo_ios_dispatch(const char *channel, const char *payload) {}`
  - `iOS/HyaloApp/BridgingHeader.h` - May need declaration for new C symbol

  **WHY Each Reference Matters**:
  - `ChannelBridge.swift` is the file being modified; existing @_cdecl bodies become the handler methods
  - PLAN.md handler dictionary is the canonical list of channel names -- must match exactly
  - `HyaloiOSModule.swift` shows the @MainActor state the handlers update; understand isolation requirements
  - `EmacsStubs.c` must provide the stub so the app compiles without real libemacs.a

  **Acceptance Criteria**:

  - [ ] `ChannelBridge.swift` contains `@_cdecl("hyalo_ios_dispatch")` top-level function
  - [ ] `ChannelBridge.dispatch()` method with handler dictionary containing all ~25 channel names
  - [ ] `EmacsStubs.c` contains `void hyalo_ios_dispatch(...)` no-op stub
  - [ ] `swift build --target Hyalo` passes (macOS regression)
  - [ ] `cd iOS && xcodebuild -scheme HyaloApp -destination 'platform=iOS Simulator' build` passes

  **QA Scenarios:**

  ```
  Scenario: Dispatch router compiles and links
    Tool: Bash
    Preconditions: ChannelBridge.swift modified, EmacsStubs.c updated
    Steps:
      1. macOS regression: swift build --target Hyalo 2>&1 | tail -5
      2. iOS build: cd iOS && swift run --package-path .. xcodegen generate && xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp -destination 'platform=iOS Simulator,name=iPad Pro 13-inch (M5)' build 2>&1 | tail -20
    Expected Result: Both builds succeed with exit code 0
    Failure Indicators: Duplicate symbol error (if @_cdecl conflicts with stub), undefined symbol, or type mismatch
    Evidence: .sisyphus/evidence/task-9-build.txt

  Scenario: Handler dictionary completeness
    Tool: Bash
    Preconditions: ChannelBridge.swift saved
    Steps:
      1. Count handlers: grep -c '".*":' Sources/HyaloiOS/Bridge/ChannelBridge.swift | head -1
      2. Spot-check key channels: grep -E '(navigator-update-buffers|status-update|color-theme|command-list)' Sources/HyaloiOS/Bridge/ChannelBridge.swift
    Expected Result: >= 20 handler entries, all 4 spot-checked channels present
    Failure Indicators: < 15 handlers, or missing critical channels
    Evidence: .sisyphus/evidence/task-9-handlers.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): implement Swift dispatch router in ChannelBridge`
  - Files: `Sources/HyaloiOS/Bridge/ChannelBridge.swift`, `Sources/HyaloEmacsStubs/EmacsStubs.c`, `iOS/HyaloApp/BridgingHeader.h`
  - Pre-commit: `swift build --target Hyalo`

- [ ] 10. Lisp Bridge Update (Phase 7.4)

  **What to do**:
  - Rewrite `lisp/hyalo-channels-ios.el` to use `hyalo-ios-dispatch` for ALL Emacs-to-Swift calls:
    - Replace all per-function `declare-function` stubs with wrapper functions calling `(hyalo-ios-dispatch "channel-name" json-payload)`
    - Each wrapper function name must match what `hyalo-status.el`, `hyalo-navigator.el`, etc. call (e.g., `hyalo-navigator-update-buffers`, `hyalo-status-update`, `hyalo-update-editor-tabs`)
    - Functions that take structured args (like `hyalo-status-update` with line, column, etc.) must JSON-encode their args
  - In `lisp/hyalo-ios.el`, keep existing no-op channel-setup functions (they are called by shared code but do nothing on iOS since dispatch is direct)
  - Ensure `hyalo-sync--push` (from `hyalo-status.el`) routes to iOS variants when `(eq window-system 'ios)`
  - Verify all function signatures match what the callers in `hyalo-status.el`, `hyalo-navigator.el`, `hyalo-appearance.el` expect

  **Must NOT do**:
  - Do NOT change channel names (must match Swift handler dictionary keys from Task 9)
  - Do NOT change Emacs Lisp API signatures (must remain compatible with macOS path)
  - Do NOT modify `hyalo-channels.el` (macOS channel file) or `hyalo-status.el` (shared caller)
  - Do NOT change JSON payload formats (must match existing macOS protocol)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Requires understanding of both the macOS channel architecture and iOS dispatch pattern; must ensure signature compatibility across the two code paths
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None relevant (Emacs Lisp, not Swift/UI)

  **Parallelization**:
  - **Can Run In Parallel**: YES (can run alongside Task 11)
  - **Parallel Group**: Wave 4 (with Tasks 11, 12, 13)
  - **Blocks**: Final verification (channel bridge must be complete)
  - **Blocked By**: Task 9 (dispatch router must exist for channel names to be correct)

  **References**:

  **Pattern References**:
  - `lisp/hyalo-channels-ios.el:1-*` - Current iOS channel bridge to rewrite
  - `lisp/hyalo-channels.el:1-*` - macOS channel implementation for comparison (function names and signatures to match)
  - `lisp/hyalo-status.el` - Caller that uses `hyalo-sync--push` to dispatch to platform-specific functions

  **API/Type References**:
  - PLAN.md lines 627-643 - Exact Elisp code for dispatch wrapper functions
  - `lisp/hyalo-navigator.el` - Calls `hyalo-navigator-update-buffers`, `hyalo-navigator-set-active-file`, etc.
  - `lisp/hyalo-appearance.el` - Calls appearance-related functions

  **External References**:
  - `lisp/hyalo-ios.el` - Module loader with no-op channel setup functions (keep as-is)

  **WHY Each Reference Matters**:
  - `hyalo-channels-ios.el` is the file being rewritten; current content shows existing stubs to replace
  - `hyalo-channels.el` (macOS) shows the canonical function signatures that iOS must match exactly
  - `hyalo-status.el` is the primary caller; must verify it can dispatch to iOS variants
  - PLAN.md code blocks show the exact dispatch pattern to implement

  **Acceptance Criteria**:

  - [ ] `hyalo-channels-ios.el` uses `hyalo-ios-dispatch` for all Emacs-to-Swift calls
  - [ ] All function names match macOS `hyalo-channels.el` public API
  - [ ] No `declare-function` stubs for removed @_cdecl functions remain
  - [ ] `(require 'hyalo-channels-ios)` succeeds without error
  - [ ] `swift build --target Hyalo` passes (macOS regression -- Lisp changes should not affect Swift build)

  **QA Scenarios:**

  ```
  Scenario: Lisp bridge loads without errors
    Tool: Bash
    Preconditions: hyalo-channels-ios.el rewritten
    Steps:
      1. Byte-compile check: emacs --batch -f batch-byte-compile lisp/hyalo-channels-ios.el 2>&1
      2. Require test: emacs --batch --eval '(progn (add-to-list (quote load-path) "lisp") (require (quote hyalo-channels-ios)))' 2>&1
    Expected Result: No errors, no warnings about void functions
    Failure Indicators: 'void-function hyalo-ios-dispatch', 'wrong number of arguments', byte-compile warnings
    Evidence: .sisyphus/evidence/task-10-byte-compile.txt

  Scenario: Function signature compatibility
    Tool: Bash
    Preconditions: hyalo-channels-ios.el rewritten
    Steps:
      1. Extract function names from macOS: grep 'defun hyalo-' lisp/hyalo-channels.el | sed 's/.*defun \(hyalo-[^ ]*\).*/\1/' | sort > /tmp/macos-fns.txt
      2. Extract function names from iOS: grep 'defun hyalo-' lisp/hyalo-channels-ios.el | sed 's/.*defun \(hyalo-[^ ]*\).*/\1/' | sort > /tmp/ios-fns.txt
      3. Compare: diff /tmp/macos-fns.txt /tmp/ios-fns.txt
    Expected Result: All public API functions in macOS also exist in iOS (iOS may have fewer internal helpers)
    Failure Indicators: Missing public functions that hyalo-status.el or hyalo-navigator.el call
    Evidence: .sisyphus/evidence/task-10-fn-compat.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): rewrite hyalo-channels-ios.el for single dispatch`
  - Files: `lisp/hyalo-channels-ios.el`
  - Pre-commit: `swift build --target Hyalo`


- [ ] 11. Reverse Channel: Swift-to-Emacs (Phase 7.5)

  **What to do**:
  - Design and implement the Swift-to-Emacs command queue:
    - In `ChannelBridge.swift`, add a thread-safe command queue (array + lock or actor-based)
    - Implement `@_cdecl("hyalo_ios_pop_command")` that dequeues and returns the next command string (or NULL if empty)
    - Add `enqueueCommand(_ elisp: String)` method that pushes to queue and calls `ios_signal_event_available()` to wake Emacs event loop
  - Patch feedstock `iosfns.m`:
    - Add `extern const char *hyalo_ios_pop_command(void) __attribute__((weak))` forward declaration
    - Add `DEFUN("hyalo-ios-pop-command", ...)` that calls `hyalo_ios_pop_command()` and returns `build_string(cmd)` or `Qnil`
    - Add `defsubr(&Shyalo_ios_pop_command)` in `syms_of_iosfns`
  - Wire navigator tap -> Emacs buffer switch:
    - When user taps a buffer in NavigatorAreaView, `ChannelBridge.enqueueCommand("(switch-to-buffer \"*scratch*\")")`
    - Emacs event loop picks up via `ios_read_socket` calling `hyalo-ios-pop-command`
  - Add `hyalo_ios_pop_command` stub to `EmacsStubs.c` (returns NULL)
  - MVP format: command strings are s-expressions like `"(switch-to-buffer \"name\")"`

  **Must NOT do**:
  - Do NOT use polling to check the queue (must use `ios_signal_event_available` to wake Emacs)
  - Do NOT change the Emacs-to-Swift direction (that is Task 9-10)
  - Do NOT add unbounded queue growth -- document maximum queue size or use bounded buffer
  - Do NOT commit feedstock changes (document patch, human commits)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Requires concurrent programming (thread-safe queue), C interop (@_cdecl), and understanding of Emacs event loop (ios_read_socket, ios_signal_event_available)
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `axiom:swift-concurrency`: Could be relevant for actor-based queue but @_cdecl cannot be actor-isolated

  **Parallelization**:
  - **Can Run In Parallel**: YES (can run alongside Tasks 10, 12, 13)
  - **Parallel Group**: Wave 4
  - **Blocks**: Final verification (reverse channel must work for full E2E)
  - **Blocked By**: Task 9 (dispatch router must exist; ChannelBridge.swift is the same file)

  **References**:

  **Pattern References**:
  - `Sources/HyaloiOS/Bridge/ChannelBridge.swift` - Add queue and enqueueCommand here alongside dispatch router
  - `Sources/HyaloiOS/Core/EmacsCInterop.swift` - `ios_signal_event_available` declaration already exists

  **API/Type References**:
  - PLAN.md lines 657-688 - Full design for reverse channel including DEFUN, queue, and ios_read_socket integration
  - `ios_signal_event_available()` - C function in feedstock that wakes the Emacs event loop

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosfns.m` - Needs second DEFUN: `hyalo-ios-pop-command`
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosterm.m` - Contains `ios_read_socket` where pop-command integration goes

  **WHY Each Reference Matters**:
  - `ChannelBridge.swift` is the file being extended with queue + enqueue + pop
  - `EmacsCInterop.swift` confirms `ios_signal_event_available` is already declared -- use it
  - PLAN.md reverse channel design is the canonical specification
  - `iosterm.m` `ios_read_socket` is where Emacs checks for events -- the pop-command call goes there

  **Acceptance Criteria**:

  - [ ] `ChannelBridge.swift` has `commandQueue`, `enqueueCommand()`, and `@_cdecl("hyalo_ios_pop_command")`
  - [ ] Queue is thread-safe (lock or serial DispatchQueue for mutations)
  - [ ] `EmacsStubs.c` has `hyalo_ios_pop_command` stub returning NULL
  - [ ] Feedstock patch documented for `iosfns.m` (second DEFUN)
  - [ ] `swift build --target Hyalo` passes (macOS regression)
  - [ ] iOS simulator build passes

  **QA Scenarios:**

  ```
  Scenario: Reverse channel compiles and links
    Tool: Bash
    Preconditions: ChannelBridge.swift updated with queue, EmacsStubs.c has pop stub
    Steps:
      1. macOS regression: swift build --target Hyalo 2>&1 | tail -5
      2. iOS build: cd iOS && ./build.sh 2>&1 | tail -20
    Expected Result: Both builds succeed
    Failure Indicators: Duplicate symbol hyalo_ios_pop_command, undefined symbol
    Evidence: .sisyphus/evidence/task-11-build.txt

  Scenario: Command queue thread safety
    Tool: Bash
    Preconditions: ChannelBridge.swift with queue
    Steps:
      1. Verify lock/serial queue usage: grep -E '(NSLock|DispatchQueue|actor|@Sendable)' Sources/HyaloiOS/Bridge/ChannelBridge.swift
      2. Verify pop returns NULL on empty: grep -A3 'hyalo_ios_pop_command' Sources/HyaloEmacsStubs/EmacsStubs.c
    Expected Result: Lock/queue mechanism present; stub returns NULL
    Failure Indicators: No synchronization, or stub returns non-null
    Evidence: .sisyphus/evidence/task-11-thread-safety.txt

  Scenario: Navigator tap enqueues command (if Emacs running in sim)
    Tool: Bash
    Preconditions: Emacs booted in simulator, reverse channel linked
    Steps:
      1. Launch: xcrun simctl launch --console booted org.gnu.hyalo 2>&1 | tee /tmp/reverse-channel.log &
      2. Wait 15s for boot
      3. Look for: grep -i 'enqueue\|pop.command\|switch-to-buffer' /tmp/reverse-channel.log
    Expected Result: Log shows command enqueued and/or popped
    Failure Indicators: No queue activity, crash when tapping navigator
    Evidence: .sisyphus/evidence/task-11-reverse-channel.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): implement reverse channel Swift-to-Emacs`
  - Files: `Sources/HyaloiOS/Bridge/ChannelBridge.swift`, `Sources/HyaloEmacsStubs/EmacsStubs.c`
  - Pre-commit: `swift build --target Hyalo`

- [ ] 12. init-hyalo.el iOS Adaptation (Phase 8.1)

  **What to do**:
  - Add iOS blocks to `init/init-hyalo.el` alongside existing macOS `(eq window-system 'ns)` blocks:
    - Add `(use-package hyalo-ios :ensure nil :if (eq window-system 'ios) :config (require 'hyalo-channels-ios) (hyalo-window--early-setup))`
    - Add `(when (eq window-system 'ios) (add-hook 'window-setup-hook #'hyalo-window-setup))`
  - Gate macOS-only packages with `(eq window-system 'ns)`:
    - `hyalo-compile` (native compilation tracking -- not available on iOS)
    - `hyalo-package` async build features (no package management on iOS)
    - `hyalo-system` (system info panel -- macOS-specific data)
  - Ensure shared packages work on both platforms:
    - `hyalo-status` hooks use `window-buffer-change-functions` (platform-independent)
    - `hyalo-navigator`, `hyalo-appearance`, `hyalo-themes` should work unchanged
  - Verify `window-system` is `'ios` on iOS (feedstock `syms_of_iosterm` registers this)

  **Must NOT do**:
  - Do NOT remove any existing macOS blocks (only ADD iOS blocks alongside them)
  - Do NOT change `hyalo-channels.el` (macOS channel file)
  - Do NOT change any `hyalo-*.el` files in `lisp/` -- only change `init/init-hyalo.el`
  - Do NOT add iOS-only features not present in macOS version

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Requires careful understanding of init file structure and platform gating; must not break macOS path
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None relevant (Emacs Lisp init configuration)

  **Parallelization**:
  - **Can Run In Parallel**: YES (can run alongside Tasks 10, 11, 13)
  - **Parallel Group**: Wave 4
  - **Blocks**: Task 13 (init-bootstrap depends on init-hyalo being iOS-aware)
  - **Blocked By**: Task 10 (Lisp bridge must exist for `(require 'hyalo-channels-ios)` to work)

  **References**:

  **Pattern References**:
  - `init/init-hyalo.el:1-*` - Current macOS-only init file; existing `(eq window-system 'ns)` blocks show pattern to replicate

  **API/Type References**:
  - PLAN.md lines 698-721 - Exact Elisp code for iOS blocks
  - `lisp/hyalo-ios.el` - The iOS module loader that `(use-package hyalo-ios)` will load
  - `lisp/hyalo-channels-ios.el` - Required by the iOS use-package block

  **External References**:
  - `~/Syntropment/hyalo-feedstock-unified/ios/iosterm.m` - `syms_of_iosterm` registers `'ios` as window-system value

  **WHY Each Reference Matters**:
  - `init-hyalo.el` is the file being modified; study existing structure to add iOS blocks in the right place
  - PLAN.md code blocks are the canonical iOS init configuration
  - feedstock `iosterm.m` confirms `'ios` is the correct window-system symbol

  **Acceptance Criteria**:

  - [ ] `init-hyalo.el` contains `(eq window-system 'ios)` blocks
  - [ ] macOS-only packages gated with `(eq window-system 'ns)`
  - [ ] `emacs --batch -l init/init-hyalo.el` on macOS still loads without error (regression)
  - [ ] No changes to any `lisp/hyalo-*.el` files

  **QA Scenarios:**

  ```
  Scenario: init-hyalo.el loads on macOS without regression
    Tool: Bash
    Preconditions: init-hyalo.el modified with iOS blocks
    Steps:
      1. Syntax check: emacs --batch --eval '(byte-compile-file "init/init-hyalo.el")' 2>&1
      2. macOS load: emacs --batch -l init/init-hyalo.el 2>&1 | grep -i 'error\|warning'
    Expected Result: No errors, no warnings; macOS path still works
    Failure Indicators: 'void-variable window-system', 'wrong-type-argument', byte-compile errors
    Evidence: .sisyphus/evidence/task-12-macos-regression.txt

  Scenario: iOS blocks present and correct
    Tool: Bash
    Preconditions: init-hyalo.el modified
    Steps:
      1. Check iOS block: grep -c 'eq window-system .ios' init/init-hyalo.el
      2. Check macOS gating: grep -c 'eq window-system .ns' init/init-hyalo.el
      3. Verify no removed macOS blocks: diff <(git show HEAD:init/init-hyalo.el | grep 'window-system') <(grep 'window-system' init/init-hyalo.el)
    Expected Result: iOS blocks added (count > 0), macOS blocks still present (count same or higher), no macOS blocks removed
    Failure Indicators: 0 iOS blocks, fewer macOS blocks than before
    Evidence: .sisyphus/evidence/task-12-ios-blocks.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): add iOS conditional blocks to init-hyalo.el`
  - Files: `init/init-hyalo.el`
  - Pre-commit: `swift build --target Hyalo`

- [ ] 13. init-bootstrap.el iOS Path (Phase 8.2)

  **What to do**:
  - Identify all packages loaded by init files that require runtime installation (straight.el, package-vc-install, etc.)
  - Decide approach: either create `init-bootstrap-ios.el` that skips package management entirely, OR gate package-install blocks in `init-bootstrap.el` with `(not (eq window-system 'ios))`
  - Bundle all required .el files: ensure `iOS/build.sh` copies all needed packages into `Resources/lisp/` or `Resources/packages/`
  - Add bundle path to `EMACSLOADPATH` in `Sources/HyaloiOS/Core/EmacsLifecycle.swift:setupEnvironment()`
  - Verify that on iOS, all `(require ...)` calls in init files succeed without network access or git

  **Must NOT do**:
  - Do NOT remove macOS package management (only gate it with `(not (eq window-system 'ios))`)
  - Do NOT change the package list for macOS
  - Do NOT require network access on iOS (all packages must be pre-bundled)
  - Do NOT modify `init-bootstrap.el` if creating a separate `init-bootstrap-ios.el` (keep both)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Requires understanding the full init dependency chain, package management on macOS, and resource bundling pipeline
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - None relevant (Emacs Lisp + build script)

  **Parallelization**:
  - **Can Run In Parallel**: NO (depends on Task 12)
  - **Parallel Group**: Wave 4 (after Task 12)
  - **Blocks**: Final verification
  - **Blocked By**: Task 12 (init-hyalo must have iOS blocks before bootstrap path matters)

  **References**:

  **Pattern References**:
  - `init/init-bootstrap.el:1-*` - Current bootstrap with package management; identify blocks to gate
  - `iOS/build.sh:1-48` - Resource copy script; may need to copy additional package .el files

  **API/Type References**:
  - PLAN.md lines 723-737 - Design for iOS bootstrap path
  - `Sources/HyaloiOS/Core/EmacsLifecycle.swift:setupEnvironment()` - Where EMACSLOADPATH is set; may need additional path

  **External References**:
  - `lisp/` directory - All hyalo .el files (already bundled by build.sh)
  - `init/` directory - All init files (already bundled by build.sh)

  **WHY Each Reference Matters**:
  - `init-bootstrap.el` is the file to gate or fork; understand what it installs and from where
  - `build.sh` determines what ends up in the bundle; may need to add package directories
  - `EmacsLifecycle.setupEnvironment()` sets load paths; must include bundled packages path
  - PLAN.md design specifies shipping all packages pre-bundled

  **Acceptance Criteria**:

  - [ ] All init files load without error on iOS (no network/git required)
  - [ ] macOS bootstrap still works (package management unchanged)
  - [ ] `EMACSLOADPATH` in EmacsLifecycle includes bundled packages path
  - [ ] `iOS/build.sh` copies all needed package .el files
  - [ ] No `(require ...)` fails with 'file-error' on iOS path

  **QA Scenarios:**

  ```
  Scenario: macOS bootstrap unaffected
    Tool: Bash
    Preconditions: init-bootstrap.el modified (or new init-bootstrap-ios.el created)
    Steps:
      1. Byte-compile: emacs --batch --eval '(byte-compile-file "init/init-bootstrap.el")' 2>&1
      2. Load test: emacs --batch -l init/init-bootstrap.el 2>&1 | grep -i 'error' | head -5
    Expected Result: No compilation errors, no load errors
    Failure Indicators: 'void-function', 'wrong-type-argument', package errors
    Evidence: .sisyphus/evidence/task-13-macos-bootstrap.txt

  Scenario: Bundle includes all required packages
    Tool: Bash
    Preconditions: build.sh updated (if needed)
    Steps:
      1. Run: cd iOS && ./build.sh --resources-only 2>&1 | tail -10
      2. Find app resources: find iOS/HyaloApp/Resources/lisp -name '*.el' | wc -l
      3. Check key packages: ls iOS/HyaloApp/Resources/lisp/hyalo-ios.el iOS/HyaloApp/Resources/lisp/hyalo-channels-ios.el 2>&1
    Expected Result: >= 20 .el files bundled, key iOS files present
    Failure Indicators: Missing .el files, build.sh errors
    Evidence: .sisyphus/evidence/task-13-bundle-contents.txt

  Scenario: iOS init loads without network (simulator)
    Tool: Bash
    Preconditions: App installed in simulator with bundled packages
    Steps:
      1. Launch: xcrun simctl launch --console booted org.gnu.hyalo 2>&1 | tee /tmp/init-test.log &
      2. Wait 20s
      3. Check: grep -i 'file-error\|Cannot open\|package.*error' /tmp/init-test.log
    Expected Result: No file-error or package errors in log
    Failure Indicators: 'Cannot open load file', 'file-error', package install attempts
    Evidence: .sisyphus/evidence/task-13-ios-init.txt
  ```

  **Commit**: YES
  - Message: `feat(ios): gate package bootstrap for iOS`
  - Files: `init/init-bootstrap.el` (or new `init/init-bootstrap-ios.el`), `iOS/build.sh` (if modified), `Sources/HyaloiOS/Core/EmacsLifecycle.swift` (if load path updated)
  - Pre-commit: `swift build --target Hyalo`

- [ ] 14. Appearance System — Wire iOS Dark/Light Mode

  **What to do**:
  - Implement `platformIsDarkMode()` for iOS using `UITraitCollection.current.userInterfaceStyle`
  - Wire `.preferredColorScheme()` modifier in `HyaloiOSNavigationLayout` to respect workspace appearance setting from `HyaloWorkspaceState`
  - Ensure `.regularMaterial` backgrounds (used for sidebar, utility area) adapt correctly to dark mode
  - Test dark/light switching via simctl: `xcrun simctl ui $UDID appearance --style dark` / `--style light`
  - Verify the appearance toggle in the inspector panel (if visible in stub mode) triggers the correct SwiftUI environment update

  **Must NOT do**:
  - Do NOT change macOS appearance logic in HyaloMac/
  - Do NOT implement Emacs theme synchronization (that requires live Emacs, covered by channel bridge tasks)
  - Do NOT add new appearance channels — reuse existing `hyalo-set-workspace-appearance` pattern from HyaloShared

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Small scoped change — one function implementation + one modifier wiring, 1-2 files
  - **Skills**: []
    - No specialized skills needed; standard SwiftUI + UIKit knowledge
  - **Skills Evaluated but Omitted**:
    - `axiom:liquid-glass`: Not relevant — this is UITraitCollection wiring, not material/glass effects
    - `axiom:swiftui-performance`: Not relevant — no performance-sensitive rendering

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2, 5)
  - **Blocks**: Task 15 (multitasking testing verifies appearance in rotated states)
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `Sources/HyaloShared/Core/HyaloWorkspaceState.swift` — `appearanceMode` property, used by macOS to set window appearance. iOS should read this same property.
  - `Sources/HyaloShared/Core/Platform.swift` — Contains `platformIsDarkMode()` using `NSApp.effectiveAppearance` on macOS. iOS equivalent uses `UITraitCollection`.

  **API/Type References**:
  - `Sources/HyaloShared/Core/HyaloWorkspaceState.swift:AppearanceMode` — enum with `.light`, `.dark`, `.system` cases

  **External References**:
  - Apple: `UITraitCollection.current.userInterfaceStyle` — `.dark` or `.light`
  - SwiftUI: `.preferredColorScheme(.dark)` / `.preferredColorScheme(.light)` modifier

  **WHY Each Reference Matters**:
  - HyaloWorkspaceState.swift contains the shared appearance state — iOS must read the same `appearanceMode` to stay consistent with the macOS code path
  - Platform.swift contains the cross-platform `platformIsDarkMode()` — iOS mirrors the macOS pattern with UIKit API

  **Acceptance Criteria**:
  - [ ] `platformIsDarkMode()` returns correct value on iOS (true in dark, false in light)
  - [ ] `.preferredColorScheme()` applied to root SwiftUI view
  - [ ] `xcrun simctl ui $UDID appearance --style dark` triggers dark mode in app

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: Dark mode switch via simctl
    Tool: Bash
    Preconditions: App installed and running in simulator ($UDID set)
    Steps:
      1. xcrun simctl ui $UDID appearance --style light
      2. sleep 2
      3. xcrun simctl io $UDID screenshot /tmp/appearance-light.png
      4. xcrun simctl ui $UDID appearance --style dark
      5. sleep 2
      6. xcrun simctl io $UDID screenshot /tmp/appearance-dark.png
      7. Compare screenshots: dark.png should have darker backgrounds than light.png
    Expected Result: Two visually distinct screenshots — light has white/light backgrounds, dark has dark/black backgrounds. Material backgrounds (.regularMaterial) adapt.
    Failure Indicators: Both screenshots look identical, or app crashes on appearance change
    Evidence: .sisyphus/evidence/task-14-appearance-light.png, .sisyphus/evidence/task-14-appearance-dark.png

  Scenario: Appearance defaults to system setting
    Tool: Bash
    Preconditions: App freshly installed (no UserDefaults for appearance), system set to dark
    Steps:
      1. xcrun simctl ui $UDID appearance --style dark
      2. xcrun simctl terminate $UDID org.gnu.hyalo
      3. xcrun simctl launch $UDID org.gnu.hyalo
      4. sleep 3
      5. xcrun simctl io $UDID screenshot /tmp/appearance-system-dark.png
    Expected Result: App launches in dark mode matching system appearance
    Failure Indicators: App launches in light mode despite system dark mode
    Evidence: .sisyphus/evidence/task-14-appearance-system.png
  ```

  **Commit**: YES
  - Message: `feat(ios): wire appearance system for iOS`
  - Files: `Sources/HyaloiOS/` or `Sources/HyaloShared/` (platform-conditional)
  - Pre-commit: `swift build --target Hyalo`

- [ ] 15. iPad Multitasking Testing — Verify Layout in All Configurations

  **What to do**:
  - Test `NavigationSplitView` column collapsing in portrait vs landscape orientation
  - Screenshot both landscape and portrait orientations via `xcrun simctl io`
  - Test Split View (Stage Manager) behavior — verify app handles width changes gracefully
  - Verify status bar remains at bottom in all configurations
  - Verify toolbar items do not overflow in compact widths
  - Check `NavigationSplitView` column widths for both iPad Pro 11" (2388x1668) and 13" (2732x2048)
  - Use `xcrun simctl ui $UDID dump-state` to inspect accessibility hierarchy and confirm layout structure

  **Must NOT do**:
  - Do NOT modify layout code unless a critical bug is found (this is a testing task)
  - Do NOT test on device (simulator only for this task)
  - If bugs are found, document them as issues — do NOT fix inline unless trivial (< 5 lines)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Multi-step verification requiring simulator manipulation, screenshots, and layout analysis across multiple device configs
  - **Skills**: []
    - No specialized skills needed; standard simctl + screenshot analysis
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not applicable — native iOS simulator, not web browser
    - `axiom:liquid-glass`: Testing only, no implementation

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 6)
  - **Blocks**: None (testing task, results are evidence files)
  - **Blocked By**: Task 14 (appearance must work to verify dark mode in rotated states), Task 2 (mock data to have visible UI)

  **References**:

  **Pattern References**:
  - `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` — Root SwiftUI layout using `NavigationSplitView` with 3-column structure
  - `Sources/HyaloShared/Window/StatusBarView.swift` — Status bar that must remain visible at bottom

  **API/Type References**:
  - `iOS/project.yml` — Device destinations and supported orientations

  **External References**:
  - `xcrun simctl io $UDID screenshot` — Screenshot capture
  - `xcrun simctl ui $UDID dump-state` — UI accessibility hierarchy inspection

  **WHY Each Reference Matters**:
  - HyaloiOSNavigationLayout.swift is the root layout — all multitasking behavior depends on how NavigationSplitView is configured there
  - StatusBarView.swift must remain visible and correctly positioned regardless of orientation/split state

  **Acceptance Criteria**:
  - [ ] Landscape screenshot captured and shows 3-column layout
  - [ ] Portrait screenshot captured and shows collapsed sidebar
  - [ ] Status bar visible at bottom in both orientations
  - [ ] No toolbar overflow in compact width (portrait / Split View)

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: Landscape layout verification (iPad Pro 13")
    Tool: Bash
    Preconditions: App running in iPad Pro 13-inch simulator, landscape orientation
    Steps:
      1. xcrun simctl io $UDID screenshot /tmp/multitask-landscape.png
      2. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); elems=[e for e in d.get('elements',[]) if 'NavigationSplitView' in e.get('type','') or e.get('identifier')]; print(json.dumps(elems,indent=2))" > /tmp/multitask-landscape-tree.json
      3. Verify screenshot shows sidebar (navigator), content (editor), and detail (inspector) columns
      4. Verify status bar visible at bottom of screenshot
    Expected Result: 3-column layout visible, status bar at bottom, no clipping or overflow
    Failure Indicators: Sidebar missing, columns overlapping, status bar hidden or displaced
    Evidence: .sisyphus/evidence/task-15-landscape.png, .sisyphus/evidence/task-15-landscape-tree.json

  Scenario: Portrait layout verification
    Tool: Bash
    Preconditions: App running in iPad Pro 13-inch simulator, portrait orientation
    Steps:
      1. xcrun simctl spawn $UDID notifyutil -p com.apple.springboard.orientation
      2. sleep 2
      3. xcrun simctl io $UDID screenshot /tmp/multitask-portrait.png
      4. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); elems=[e for e in d.get('elements',[]) if e.get('identifier')]; print(json.dumps(elems,indent=2))" > /tmp/multitask-portrait-tree.json
      5. Verify sidebar is collapsed (auto-hidden) in portrait
      6. Verify status bar visible at bottom
    Expected Result: Content fills width, sidebar auto-collapsed, status bar at bottom
    Failure Indicators: Sidebar permanently visible eating screen space, status bar missing
    Evidence: .sisyphus/evidence/task-15-portrait.png, .sisyphus/evidence/task-15-portrait-tree.json

  Scenario: Toolbar compact width check
    Tool: Bash
    Preconditions: App in portrait or narrow Split View
    Steps:
      1. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); [print(e.get('label','')) for e in d.get('elements',[]) if 'toolbar' in e.get('type','').lower() or 'button' in e.get('type','').lower()]" > /tmp/toolbar-items.txt
      2. Verify no truncated labels or "..." in toolbar button text
      3. xcrun simctl io $UDID screenshot /tmp/multitask-toolbar.png
    Expected Result: All toolbar items visible without overflow or truncation
    Failure Indicators: "..." in button labels, items pushed off-screen, layout warnings in console
    Evidence: .sisyphus/evidence/task-15-toolbar.png
  ```

  **Commit**: NO (testing task — evidence files only)

- [ ] 16. Command Palette Testing — Verify Cmd+P and Cmd+O Sheets

  **What to do**:
  - Verify Cmd+P triggers `CommandPaletteView` sheet presentation in simulator
  - Verify Cmd+O triggers `OpenQuicklyView` sheet presentation in simulator
  - Test search filtering with hardware keyboard input (type characters, verify list filters)
  - Test sheet dismissal: swipe down gesture and Escape key
  - Use `xcrun simctl ui $UDID dump-state` to confirm sheet elements are present in accessibility tree
  - Screenshot each sheet open state

  **Must NOT do**:
  - Do NOT modify CommandPaletteView or OpenQuicklyView code (this is testing only)
  - Do NOT test with real Emacs data — mock data from Task 2 is sufficient
  - If sheets do not work, document the issue — do NOT attempt to fix

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Straightforward verification task — trigger keyboard shortcuts, take screenshots, check accessibility tree
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not applicable — native iOS simulator, not web browser

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Tasks 4, 7, 8, 17)
  - **Blocks**: None (testing task)
  - **Blocked By**: Task 2 (mock data needed for command list content)

  **References**:

  **Pattern References**:
  - `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` — Contains `.sheet` presentations for `CommandPaletteView` and `OpenQuicklyView`, triggered by `showCommandPalette` / `showOpenQuickly` state
  - `Sources/HyaloShared/CommandPalette/CommandPaletteView.swift` — The sheet view to verify renders
  - `Sources/HyaloShared/OpenQuickly/OpenQuicklyView.swift` — The file search sheet to verify renders

  **External References**:
  - `xcrun simctl ui $UDID dump-state` — Inspect accessibility tree for sheet presence
  - `xcrun simctl io $UDID sendkey` — Send hardware keyboard events to simulator

  **WHY Each Reference Matters**:
  - HyaloiOSNavigationLayout.swift defines HOW sheets are triggered — executor needs to understand the state binding to verify correct behavior
  - CommandPaletteView/OpenQuicklyView are the views that must actually render — executor verifies their accessibility tree elements

  **Acceptance Criteria**:
  - [ ] Cmd+P opens CommandPaletteView sheet (screenshot evidence)
  - [ ] Cmd+O opens OpenQuicklyView sheet (screenshot evidence)
  - [ ] Typing in search field filters the list (`xcrun simctl ui $UDID dump-state` shows filtered items)
  - [ ] Sheet dismisses on swipe down or Escape

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: Cmd+P opens command palette sheet
    Tool: Bash
    Preconditions: App running in simulator with mock data (Task 2), hardware keyboard connected
    Steps:
      1. xcrun simctl io $UDID sendkey command-p
      2. sleep 1
      3. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); items=[e for e in d if 'command' in str(e.get('label','')).lower() or 'palette' in str(e.get('label','')).lower() or 'search' in str(e.get('label','')).lower()]; json.dump(items,open('/tmp/palette-tree.json','w'))"
      4. xcrun simctl io $UDID screenshot /tmp/command-palette.png
      5. Verify palette-tree.json has >= 1 element (sheet is visible)
    Expected Result: Sheet visible with search field and command list items from mock data
    Failure Indicators: Empty palette-tree.json, no sheet in screenshot, crash on key press
    Evidence: .sisyphus/evidence/task-16-cmd-p.png, .sisyphus/evidence/task-16-palette-tree.json

  Scenario: Cmd+O opens file search sheet
    Tool: Bash
    Preconditions: App running, mock data loaded
    Steps:
      1. xcrun simctl io $UDID sendkey command-o
      2. sleep 1
      3. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); items=[e for e in d if 'open' in str(e.get('label','')).lower() or 'quickly' in str(e.get('label','')).lower() or 'file' in str(e.get('label','')).lower() or 'search' in str(e.get('label','')).lower()]; json.dump(items,open('/tmp/open-quickly-tree.json','w'))"
      4. xcrun simctl io $UDID screenshot /tmp/open-quickly.png
      5. Verify open-quickly-tree.json has >= 1 element
    Expected Result: File search sheet visible with search field
    Failure Indicators: Empty JSON, no visible sheet, crash
    Evidence: .sisyphus/evidence/task-16-cmd-o.png, .sisyphus/evidence/task-16-open-quickly-tree.json

  Scenario: Sheet dismissal via Escape
    Tool: Bash
    Preconditions: Command palette sheet open (from previous scenario)
    Steps:
      1. xcrun simctl io $UDID sendkey command-p   # open sheet
      2. sleep 1
      3. xcrun simctl io $UDID sendkey escape   # dismiss
      4. sleep 1
      5. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); items=[e for e in d if 'palette' in str(e.get('label','')).lower()]; json.dump(items,open('/tmp/dismissed.json','w'))"
      6. Verify dismissed.json is empty or has 0 elements (sheet gone)
    Expected Result: Sheet dismissed, main app view visible
    Failure Indicators: Sheet still visible after Escape, crash on dismiss
    Evidence: .sisyphus/evidence/task-16-dismiss.json
  ```

  **Commit**: NO (testing task — evidence files only)

- [ ] 17. Terminal Integration Assessment — SwiftTerm on iPad

  **What to do**:
  - Check if SwiftTerm has a `SwiftUI.TerminalView` that works on iOS (not just AppKit `NSView`-based)
  - Inspect the SwiftTerm dependency in Package.swift — check which products/targets are available
  - If `SwiftUI.TerminalView` exists for iOS:
    - Create `Sources/HyaloiOS/UtilityArea/UtilityAreaTerminalViewiOS.swift`
    - Replace the current `#if os(macOS)` EmptyView stub with a real terminal view
    - Wire it into the utility area tab bar alongside diagnostics
  - If no iOS-compatible terminal view exists:
    - Document findings: what SwiftTerm offers for iOS, what's missing
    - Keep the `EmptyView` stub — show only the diagnostics tab in utility area
    - Create a TODO comment in the code: `// TODO: SwiftTerm iOS terminal when available`
  - Either way, verify the utility area renders correctly with at least the diagnostics tab

  **Must NOT do**:
  - Do NOT build a custom terminal emulator — use SwiftTerm or defer
  - Do NOT add new dependencies for terminal support
  - Do NOT break the macOS terminal integration in HyaloMac/
  - Do NOT spend more than 30 minutes researching — if unclear, defer with documentation

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Research + conditional small implementation (one file) or documentation of deferral. Bounded scope.
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `axiom:swiftui-performance`: Not applicable for terminal assessment

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Tasks 4, 7, 8, 16)
  - **Blocks**: None
  - **Blocked By**: Task 2 (need mock data to verify utility area renders)

  **References**:

  **Pattern References**:
  - `Sources/HyaloMac/UtilityArea/UtilityAreaTerminalView.swift` — macOS terminal using AppKit NSViewRepresentable wrapping SwiftTerm.TerminalView. Shows the pattern to replicate for iOS if available.
  - `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` — Utility area is the bottom panel — currently has diagnostics tab and potentially a stub for terminal

  **API/Type References**:
  - `Package.swift` — SwiftTerm dependency declaration (check which products are included)

  **External References**:
  - SwiftTerm GitHub: `https://github.com/migueldeicaza/SwiftTerm` — Check if `SwiftTermView` (SwiftUI) supports iOS

  **WHY Each Reference Matters**:
  - The macOS terminal view shows exactly what needs to be replicated (or adapted) for iOS — same API surface, different UIKit wrapper
  - Package.swift tells us which SwiftTerm products we have access to (may need to add `.product(name: "SwiftTermView")` for iOS)

  **Acceptance Criteria**:
  - [ ] Assessment documented: SwiftTerm iOS support status (YES with path, or NO with reason)
  - [ ] If YES: `UtilityAreaTerminalViewiOS.swift` created and compiles
  - [ ] If NO: EmptyView stub retained, TODO comment added, findings documented
  - [ ] Utility area renders in simulator with at least diagnostics tab visible
  - [ ] `swift build --target Hyalo` passes (macOS regression)

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: SwiftTerm iOS capability check
    Tool: Bash
    Preconditions: Package.swift accessible
    Steps:
      1. grep -n 'SwiftTerm' Package.swift
      2. swift package dump-package | jq '.dependencies[] | select(.identity | test("swiftterm"; "i"))'
      3. If SwiftTerm found: check its Package.swift for iOS platform support
         git -C .build/checkouts/SwiftTerm cat-file -p HEAD:Package.swift 2>/dev/null || cat .build/checkouts/SwiftTerm/Package.swift
      4. Grep for: `platforms:.*iOS` and `TerminalView.*View` in SwiftTerm sources
    Expected Result: Clear YES/NO answer with evidence on SwiftTerm iOS support
    Failure Indicators: SwiftTerm not in dependencies at all, or Package.swift has no iOS platform
    Evidence: .sisyphus/evidence/task-17-swiftterm-assessment.txt

  Scenario: Utility area renders with diagnostics
    Tool: Bash
    Preconditions: App running in simulator with mock data
    Steps:
      1. xcrun simctl io $UDID screenshot /tmp/utility-area.png
      2. xcrun simctl ui $UDID dump-state | python3 -c "import sys,json; d=json.load(sys.stdin); items=[e for e in d if 'diagnostic' in str(e.get('label','')).lower() or 'terminal' in str(e.get('label','')).lower() or 'utility' in str(e.get('label','')).lower()]; print(json.dumps(items,indent=2))"
    Expected Result: Utility area visible at bottom with at least diagnostics tab
    Failure Indicators: No utility area visible, crash when toggling utility area
    Evidence: .sisyphus/evidence/task-17-utility-area.png

  Scenario: macOS build unaffected
    Tool: Bash
    Preconditions: Any changes made to iOS files
    Steps:
      1. swift build --target Hyalo 2>&1 | tail -3
    Expected Result: "Build complete!" — macOS terminal still works
    Failure Indicators: Compilation errors referencing UIKit or iOS-only types
    Evidence: .sisyphus/evidence/task-17-macos-regression.txt
  ```

  **Commit**: YES (if implementation changes made)
  - Message: `feat(ios): assess terminal integration for iPad`
  - Files: `Sources/HyaloiOS/UtilityArea/UtilityAreaTerminalViewiOS.swift` (if created)
  - Pre-commit: `swift build --target Hyalo`

---

## Final Verification Wave

> 4 review agents run in PARALLEL. ALL must APPROVE. Rejection -> fix -> re-run.

- [ ] F1. **Plan Compliance Audit** — `oracle`
  Read PLAN.md Phases 5-9 end-to-end. For each action item: verify implementation exists (read file, run command). For each "Must NOT Have": search codebase for forbidden patterns — reject with file:line if found. Check evidence files exist in .sisyphus/evidence/. Compare deliverables against PLAN.md.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [ ] F2. **Code Quality Review** — `unspecified-high`
  Run `swift build --target Hyalo` (macOS regression). Run `xcodebuild -scheme HyaloApp -destination 'platform=iOS Simulator'`. Review all changed files for: force-unwraps, empty catches, print() in prod code, commented-out code, unused imports. Check for AI slop: excessive comments, over-abstraction, placeholder implementations.
  Output: `macOS Build [PASS/FAIL] | iOS Build [PASS/FAIL] | Files [N clean/N issues] | VERDICT`

- [ ] F3. **Real Manual QA** — `unspecified-high`
  Start from clean state. Build and install fresh. Execute EVERY QA scenario from EVERY task — follow exact steps, capture evidence. Test cross-task integration (channel bridge end-to-end, init loading, appearance). Save to `.sisyphus/evidence/final-qa/`.
  Output: `Scenarios [N/N pass] | Integration [N/N] | Edge Cases [N tested] | VERDICT`

 [ ] F4. **Scope Fidelity Check** — `unspecified-high`
  For each task: read "What to do", read actual changes. Verify 1:1 — everything specified was built (no missing), nothing beyond spec was built (no creep). Check "Must NOT do" compliance. Verify PLAN.md was NOT modified. Verify macOS HyaloMac/ sources unchanged. Flag unaccounted changes.
  Output: `Tasks [N/N compliant] | Scope [CLEAN/N issues] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **T1**: No commit (feedstock build artifact, not in hyalo-unified git)
- **T2**: `feat(ios): add mock data for simulator visual testing` — Sources/HyaloiOS/
- **T3**: `feat(ios): link real libemacs.a in simulator build` — iOS/project.yml, Package.swift
- **T4**: `fix(ios): verify Emacs bootstrap resources and paths` — iOS/build.sh (if changed)
- **T5**: No commit (verification only)
- **T6**: `feat(ios): configure device build with per-SDK library paths` — iOS/project.yml
- **T7**: No commit (device testing only)
- **T8**: No commit (feedstock patch, human commits feedstock)
- **T9**: `feat(ios): implement Swift dispatch router in ChannelBridge` — Sources/HyaloiOS/Bridge/ChannelBridge.swift
- **T10**: `feat(ios): rewrite hyalo-channels-ios.el for single dispatch` — lisp/hyalo-channels-ios.el
- **T11**: `feat(ios): implement reverse channel Swift-to-Emacs` — Sources/HyaloiOS/Bridge/ChannelBridge.swift
- **T12**: `feat(ios): add iOS conditional blocks to init-hyalo.el` — init/init-hyalo.el
- **T13**: `feat(ios): gate package bootstrap for iOS` — init/init-bootstrap.el or new init-bootstrap-ios.el
- **T14**: `feat(ios): wire appearance system for iOS` — Sources/HyaloiOS/ or Sources/HyaloShared/
- **T15**: No commit (testing only)
- **T16**: No commit (testing only)
- **T17**: `feat(ios): assess terminal integration for iPad` — Sources/HyaloiOS/ (if changes made)

---

## Success Criteria

### Verification Commands
```bash
# macOS regression
swift build --target Hyalo  # Expected: Build complete!

# iOS simulator build
cd iOS && swift run --package-path .. xcodegen generate && xcodebuild -project HyaloApp.xcodeproj -scheme HyaloApp -destination "platform=iOS Simulator,id=$UDID" -derivedDataPath /tmp/hyalo-ios-build build  # Expected: BUILD SUCCEEDED

# Simulator launch
xcrun simctl install $UDID /tmp/hyalo-ios-build/Build/Products/Debug-iphonesimulator/Hyalo.app
xcrun simctl launch --console $UDID org.gnu.hyalo 2>&1 | head -50  # Expected: no crash, Emacs init logs

# Channel bridge
# In Emacs: (hyalo-ios-dispatch "navigator-update-buffers" "[{\"name\":\"*scratch*\"}]")  # Expected: nil, no crash

# Reverse channel
# Tap buffer in navigator -> Emacs switches buffer (console log confirms)
```

### Final Checklist
- [ ] All "Must Have" present (libemacs.a linked, dispatch DEFUN, Swift router, iOS init)
- [ ] All "Must NOT Have" absent (no PLAN.md edits, no HyaloMac changes, no new deps)
- [ ] macOS `swift build --target Hyalo` passes
- [ ] iOS simulator builds and launches
- [ ] Emacs boots and renders text
- [ ] Channel bridge flows data both directions
- [ ] Evidence screenshots saved in .sisyphus/evidence/