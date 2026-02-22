# Fix macOS Build — Complete Phase 1 Type Migration

## TL;DR

> **Quick Summary**: The macOS build is broken (1845 errors) because Phase 1 migration moved 56 files to HyaloShared but left several types defined only in HyaloMac that HyaloShared references. Fix by moving cross-platform types to HyaloShared and adding `#if os()` guards for platform-specific ones.
> 
> **Deliverables**:
> - `swift build` passes with 0 errors
> - All cross-platform types live in HyaloShared
> - Platform-specific types guarded with `#if os(macOS)` + iOS stubs
> - PLAN.md updated to reflect corrective work
> 
> **Estimated Effort**: Medium
> **Parallel Execution**: YES - 4 waves
> **Critical Path**: Task 1 (HyaloDesign) -> Task 6 (EffectView) -> Task 9 (#if guards) -> Task 11 (build verify)

---

## Context

### Original Request
Port Hyalo from macOS to iPadOS while keeping the macOS version fully functional at all times. The immediate blocker is that `swift build` fails with 1845 errors because HyaloShared references types that only exist in HyaloMac.

### Interview Summary
**Key Discussions**:
- Platform-specific strategy: User chose `#if os()` in HyaloShared (not protocols or decoupling)
- iOS terminal: Include terminal in plan (not deferred)
- Plan approach: Update existing PLAN.md (not fresh plan)

**Research Findings**:
- `HyaloDesign`, `CommandItem`, `OpenQuicklyItem`, `HyaloContentUnavailableView`, `GeometrySizeTracker` are purely cross-platform — move as-is
- `EffectView` is NSViewRepresentable — needs macOS guard + iOS `EmptyView` stub
- `instantPopover` uses NSPopover — needs macOS guard + iOS `.popover()` fallback
- `dropdownItemStyle` is actually pure SwiftUI (`.onHover`) — move as-is
- `TerminalPalette` data model is cross-platform; terminal views are AppKit+SwiftTerm
- `InspectorAppearanceView` deeply coupled to macOS (NSAppearance, HyaloModule.allControllers)

### Metis Review
**Identified Gaps** (addressed):
- Build verification after each move: Plan includes build-verify after each wave
- iOS stub strategy: `EmptyView()` for views, no-op for modifiers
- Access control: Types moved to HyaloShared will retain existing access modifiers (most are already internal, which works within the same target)
- Move ordering: Dependencies between types checked — `OpenQuicklyItem` does not depend on `CommandItem`
- Atomic commits: Each wave is independently committable
- No refactoring during move: Pure cut-paste only

---

## Work Objectives

### Core Objective
Fix the macOS build by moving misplaced types from HyaloMac to HyaloShared, adding `#if os()` guards for platform-specific code, and verifying the build compiles cleanly.

### Concrete Deliverables
- New files in HyaloShared: `HyaloDesign.swift`, `CommandItem.swift`, `OpenQuicklyItem.swift`, `HyaloContentUnavailableView.swift`, `GeometrySizeTracker.swift`, `EffectView.swift`, `InstantPopoverModifier.swift`, `DropdownItemStyle.swift`, `TerminalPalette.swift`
- Modified files in HyaloShared: `UtilityAreaView.swift`, `UtilityAreaViewModel.swift`, `InspectorTab.swift`
- Cleaned up files in HyaloMac: `HyaloShared.swift` (reduced), `CommandPaletteManager.swift` (reduced), `VibrancyViews.swift` (reduced), `InstantPopoverModifier.swift` (removed or reduced)
- Updated `PLAN.md` with corrective notes

### Definition of Done
- [ ] `swift build` exits with 0 errors
- [ ] No types in HyaloMac are referenced from HyaloShared
- [ ] PLAN.md Phase 1 reflects the corrective work done

### Must Have
- macOS build passes after every wave (incremental verification)
- Pure cut-paste — no refactoring, no renaming, no API changes during move
- `#if os(macOS)` for all AppKit/SwiftTerm types, with `#else` stubs on iOS

### Must NOT Have (Guardrails)
- Do NOT refactor any type during the move (no "while we're at it" changes)
- Do NOT change any public API signatures
- Do NOT add new dependencies to Package.swift
- Do NOT touch HyaloiOS files (those come in Phase 2)
- Do NOT modify any Emacs Lisp files
- Do NOT add excessive comments or documentation to moved code
- Do NOT abstract types into protocols (user explicitly chose `#if os()` approach)

---

## Verification Strategy (MANDATORY)

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed. No exceptions.

### Test Decision
- **Infrastructure exists**: NO (no unit test framework configured)
- **Automated tests**: None
- **Framework**: N/A
- **Primary verification**: `swift build` must exit 0

### QA Policy
Every task includes a build verification step. Evidence saved to `.sisyphus/evidence/task-{N}-build.txt`.

- **All tasks**: Use Bash — `swift build 2>&1` — check exit code and error count
- **Move tasks**: Verify old location no longer defines the type, new location does

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately — pure cross-platform types, ALL independent):
├── Task 1: Move HyaloDesign to HyaloShared [quick]
├── Task 2: Move CommandItem to HyaloShared [quick]
├── Task 3: Move OpenQuicklyItem to HyaloShared [quick]
├── Task 4: Move HyaloContentUnavailableView to HyaloShared [quick]
├── Task 5: Move GeometrySizeTracker to HyaloShared [quick]
└── Verify: swift build — errors should drop significantly

Wave 2 (After Wave 1 — platform-specific types with #if os):
├── Task 6: Move EffectView with #if os(macOS) + iOS stub [quick]
├── Task 7: Move InstantPopover system with #if os(macOS) + iOS fallback [quick]
├── Task 8: Move DropdownItemStyle to HyaloShared (pure SwiftUI) [quick]
└── Verify: swift build — more errors resolved

Wave 3 (After Wave 2 — terminal + inspector guards):
├── Task 9: Move TerminalPalette model to HyaloShared + #if guards in UtilityArea [quick]
├── Task 10: Add #if os(macOS) guard for InspectorAppearanceView in InspectorTab [quick]
└── Verify: swift build — should be 0 errors

Wave 4 (After Wave 3 — cleanup and documentation):
├── Task 11: Final build verification + cleanup dead code in HyaloMac [quick]
├── Task 12: Update PLAN.md with corrective Phase 1 notes [quick]
└── Verify: swift build — confirmed 0 errors

Wave FINAL (After ALL tasks — independent review):
├── Task F1: Plan compliance audit [oracle]
├── Task F2: Code quality review — no leftover references [unspecified-high]
└── Task F3: Scope fidelity check — only planned moves, no creep [deep]

Critical Path: Task 1-5 → Task 6-8 → Task 9-10 → Task 11-12 → F1-F3
Parallel Speedup: ~60% faster than sequential
Max Concurrent: 5 (Wave 1)
```

### Dependency Matrix

| Task | Depends On | Blocks |
|------|-----------|--------|
| 1 (HyaloDesign) | — | 6, 7, 8, 11 |
| 2 (CommandItem) | — | 11 |
| 3 (OpenQuicklyItem) | — | 11 |
| 4 (ContentUnavailable) | — | 11 |
| 5 (GeometrySizeTracker) | — | 11 |
| 6 (EffectView) | Wave 1 done | 9, 11 |
| 7 (InstantPopover) | Wave 1 done | 11 |
| 8 (DropdownItemStyle) | Wave 1 done | 11 |
| 9 (TerminalPalette + guards) | Wave 2 done | 11 |
| 10 (InspectorAppearance guard) | Wave 2 done | 11 |
| 11 (Final build verify) | 1-10 | 12, F1-F3 |
| 12 (Update PLAN.md) | 11 | F1-F3 |
| F1-F3 | 11, 12 | — |

### Agent Dispatch Summary

- **Wave 1**: 5 tasks — T1-T5 all `quick`
- **Wave 2**: 3 tasks — T6-T8 all `quick`
- **Wave 3**: 2 tasks — T9-T10 all `quick`
- **Wave 4**: 2 tasks — T11 `quick`, T12 `quick`
- **FINAL**: 3 tasks — F1 `oracle`, F2 `unspecified-high`, F3 `deep`

---

## TODOs

### Wave 1 — Pure Cross-Platform Types (ALL independent, run in parallel)

- [x] 1. Move HyaloDesign to HyaloShared

  **What to do**:
  - Create `Sources/HyaloShared/Core/HyaloDesign.swift`
  - Copy the `HyaloDesign` enum (lines 10-75) from `Sources/HyaloMac/Core/HyaloShared.swift`
  - This includes all nested enums: `CornerRadius`, `Padding`, `Height`, `Width`, `Spacing`, `FontSize`, `IconSize`, `Animation`
  - Remove lines 10-75 from `Sources/HyaloMac/Core/HyaloShared.swift`
  - Keep `import SwiftUI` at top of new file
  - Do NOT change any values or add/remove any properties

  **Must NOT do**:
  - Do not rename anything
  - Do not change access modifiers
  - Do not add documentation comments

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 2, 3, 4, 5)
  - **Blocks**: Tasks 6, 7, 8, 11 (Wave 2 and final verify)
  - **Blocked By**: None (can start immediately)

  **References**:
  - `Sources/HyaloMac/Core/HyaloShared.swift:10-75` — Source: the full HyaloDesign enum to copy
  - `Sources/HyaloShared/Core/Platform.swift` — Pattern reference: existing file in HyaloShared/Core/ for naming/structure convention
  - `Sources/HyaloShared/Navigator/FindNavigatorView.swift:113,122,125` — Consumer: uses `HyaloDesign.FontSize.*` — will compile once moved
  - `Sources/HyaloShared/StatusBar/StatusBarView.swift` — Consumer: likely references HyaloDesign constants

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Core/HyaloDesign.swift` exists with the HyaloDesign enum
  - [ ] `HyaloDesign` is no longer defined in `Sources/HyaloMac/Core/HyaloShared.swift`
  - [ ] `grep -r 'enum HyaloDesign' Sources/HyaloShared/` returns exactly 1 match
  - [ ] `grep -r 'enum HyaloDesign' Sources/HyaloMac/` returns 0 matches

  **QA Scenarios:**
  ```
  Scenario: HyaloDesign type available in HyaloShared
    Tool: Bash
    Steps:
      1. grep -r 'enum HyaloDesign' Sources/HyaloShared/Core/HyaloDesign.swift
      2. grep -c 'enum HyaloDesign' Sources/HyaloMac/Core/HyaloShared.swift — expect 0
      3. swift build 2>&1 | grep 'HyaloDesign' | grep 'error:' | wc -l — expect significant reduction
    Expected Result: HyaloDesign found in HyaloShared, not in HyaloMac
    Evidence: .sisyphus/evidence/task-1-hyalodesign-move.txt

  Scenario: No duplicate definitions
    Tool: Bash
    Steps:
      1. grep -rn 'enum HyaloDesign' Sources/ — expect exactly 1 result in HyaloShared
    Expected Result: Single definition in HyaloShared/Core/HyaloDesign.swift
    Evidence: .sisyphus/evidence/task-1-no-duplicates.txt
  ```

  **Commit**: YES (groups with Wave 1)
  - Message: `fix(shared): move HyaloDesign to HyaloShared`
  - Files: `Sources/HyaloShared/Core/HyaloDesign.swift`, `Sources/HyaloMac/Core/HyaloShared.swift`

- [x] 2. Move CommandItem to HyaloShared

  **What to do**:
  - Create `Sources/HyaloShared/CommandPalette/CommandItem.swift`
  - Copy from `Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift`:
    - `CommandItem` struct (lines 12-19)
    - `FuzzyMatchable` conformance for CommandItem (lines 22-26)
  - Remove those lines from CommandPaletteManager.swift
  - Add `import Foundation` at top (for Codable)
  - The `FuzzyMatchable` protocol is defined in `Sources/HyaloShared/CommandPalette/FuzzyMatcher.swift` — it is already in HyaloShared, so the conformance will compile

  **Must NOT do**:
  - Do not move `CommandPaletteManager` class (it stays in HyaloMac)
  - Do not change the struct fields or conformances

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 3, 4, 5)
  - **Blocks**: Task 11 (final verify)
  - **Blocked By**: None

  **References**:
  - `Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift:12-26` — Source: CommandItem struct + FuzzyMatchable extension
  - `Sources/HyaloShared/CommandPalette/FuzzyMatcher.swift` — Dependency: defines the `FuzzyMatchable` protocol that CommandItem conforms to
  - `Sources/HyaloShared/CommandPalette/CommandPaletteViewModel.swift` — Consumer: uses CommandItem type
  - `Sources/HyaloShared/CommandPalette/CommandPaletteView.swift` — Consumer: renders CommandItem list

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/CommandPalette/CommandItem.swift` exists
  - [ ] `CommandItem` no longer defined in `CommandPaletteManager.swift`
  - [ ] `grep -r 'struct CommandItem' Sources/HyaloShared/` returns 1 match
  - [ ] `grep -r 'struct CommandItem' Sources/HyaloMac/` returns 0 matches

  **QA Scenarios:**
  ```
  Scenario: CommandItem available in HyaloShared
    Tool: Bash
    Steps:
      1. grep -rn 'struct CommandItem' Sources/HyaloShared/CommandPalette/CommandItem.swift
      2. grep -c 'struct CommandItem' Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift — expect 0
    Expected Result: CommandItem in HyaloShared only
    Evidence: .sisyphus/evidence/task-2-commanditem-move.txt
  ```

  **Commit**: YES (groups with Wave 1)
  - Message: `fix(shared): move CommandItem to HyaloShared`
  - Files: `Sources/HyaloShared/CommandPalette/CommandItem.swift`, `Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift`

- [x] 3. Move OpenQuicklyItem to HyaloShared

  **What to do**:
  - Create `Sources/HyaloShared/CommandPalette/OpenQuicklyItem.swift`
  - Copy from `Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift`:
    - `OpenQuicklyItem` struct (lines 32-38)
    - `FuzzyMatchable` conformance for OpenQuicklyItem (lines 40-45)
  - Remove those lines from CommandPaletteManager.swift
  - Add `import Foundation` at top

  **Must NOT do**:
  - Do not move `CommandPaletteManager` class
  - Do not change the struct fields

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2, 4, 5)
  - **Blocks**: Task 11
  - **Blocked By**: None

  **References**:
  - `Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift:32-45` — Source: OpenQuicklyItem struct + FuzzyMatchable extension
  - `Sources/HyaloShared/CommandPalette/FuzzyMatcher.swift` — Dependency: FuzzyMatchable protocol
  - `Sources/HyaloShared/CommandPalette/OpenQuicklyViewModel.swift` — Consumer: uses OpenQuicklyItem type extensively
  - `Sources/HyaloShared/CommandPalette/OpenQuicklyView.swift` — Consumer: renders OpenQuicklyItem list

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/CommandPalette/OpenQuicklyItem.swift` exists
  - [ ] `OpenQuicklyItem` no longer defined in `CommandPaletteManager.swift`
  - [ ] `grep -r 'struct OpenQuicklyItem' Sources/HyaloShared/` returns 1 match

  **QA Scenarios:**
  ```
  Scenario: OpenQuicklyItem available in HyaloShared
    Tool: Bash
    Steps:
      1. grep -rn 'struct OpenQuicklyItem' Sources/HyaloShared/CommandPalette/OpenQuicklyItem.swift
      2. grep -c 'struct OpenQuicklyItem' Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift — expect 0
    Expected Result: OpenQuicklyItem in HyaloShared only
    Evidence: .sisyphus/evidence/task-3-openquicklyitem-move.txt
  ```

  **Commit**: YES (groups with Wave 1)
  - Message: `fix(shared): move OpenQuicklyItem to HyaloShared`
  - Files: `Sources/HyaloShared/CommandPalette/OpenQuicklyItem.swift`, `Sources/HyaloMac/CommandPalette/CommandPaletteManager.swift`

- [x] 4. Move HyaloContentUnavailableView to HyaloShared

  **What to do**:
  - Create `Sources/HyaloShared/Shared/HyaloContentUnavailableView.swift`
  - Copy from `Sources/HyaloMac/Core/HyaloShared.swift`:
    - `HyaloContentUnavailableView<Actions: View>` struct (lines 143-200)
  - Remove lines 143-200 from `Sources/HyaloMac/Core/HyaloShared.swift`
  - Keep `import SwiftUI` at top of new file

  **Must NOT do**:
  - Do not change the generic constraint or view body

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2, 3, 5)
  - **Blocks**: Task 11
  - **Blocked By**: None

  **References**:
  - `Sources/HyaloMac/Core/HyaloShared.swift:143-200` — Source: the full generic view
  - `Sources/HyaloShared/Navigator/FindNavigatorView.swift:136` — Consumer: uses `HyaloContentUnavailableView("Search", ...)`
  - `Sources/HyaloShared/Navigator/DiagnosticsNavigatorView.swift` — Consumer: likely uses this for empty states

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Shared/HyaloContentUnavailableView.swift` exists
  - [ ] `grep -r 'struct HyaloContentUnavailableView' Sources/HyaloShared/` returns 1 match
  - [ ] `grep -r 'struct HyaloContentUnavailableView' Sources/HyaloMac/` returns 0 matches

  **QA Scenarios:**
  ```
  Scenario: HyaloContentUnavailableView in HyaloShared
    Tool: Bash
    Steps:
      1. grep -rn 'struct HyaloContentUnavailableView' Sources/HyaloShared/
      2. grep -c 'struct HyaloContentUnavailableView' Sources/HyaloMac/Core/HyaloShared.swift — expect 0
    Expected Result: Single definition in HyaloShared/Shared/
    Evidence: .sisyphus/evidence/task-4-contentunavailable-move.txt
  ```

  **Commit**: YES (groups with Wave 1)
  - Message: `fix(shared): move HyaloContentUnavailableView to HyaloShared`
  - Files: `Sources/HyaloShared/Shared/HyaloContentUnavailableView.swift`, `Sources/HyaloMac/Core/HyaloShared.swift`

- [x] 5. Move GeometrySizeTracker to HyaloShared

  **What to do**:
  - Create `Sources/HyaloShared/Shared/GeometrySizeTracker.swift`
  - Copy from `Sources/HyaloMac/Core/HyaloShared.swift`:
    - `GeometrySizeTracker` struct and its `Dimension` enum (lines 80-113)
  - Remove lines 80-113 from `Sources/HyaloMac/Core/HyaloShared.swift`
  - Keep `import SwiftUI` at top of new file

  **Must NOT do**:
  - Do not change the Dimension enum or view body

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2, 3, 4)
  - **Blocks**: Task 11
  - **Blocked By**: None

  **References**:
  - `Sources/HyaloMac/Core/HyaloShared.swift:80-113` — Source: GeometrySizeTracker + Dimension enum
  - Uses only SwiftUI (GeometryReader, PreferenceKey) — fully cross-platform

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Shared/GeometrySizeTracker.swift` exists
  - [ ] `grep -r 'struct GeometrySizeTracker' Sources/HyaloShared/` returns 1 match
  - [ ] `grep -r 'struct GeometrySizeTracker' Sources/HyaloMac/` returns 0 matches

  **QA Scenarios:**
  ```
  Scenario: GeometrySizeTracker in HyaloShared
    Tool: Bash
    Steps:
      1. grep -rn 'struct GeometrySizeTracker' Sources/HyaloShared/
      2. grep -c 'struct GeometrySizeTracker' Sources/HyaloMac/Core/HyaloShared.swift — expect 0
    Expected Result: Single definition in HyaloShared/Shared/
    Evidence: .sisyphus/evidence/task-5-geometrysizetracker-move.txt
  ```

  **Commit**: YES (groups with Wave 1)
  - Message: `fix(shared): move GeometrySizeTracker to HyaloShared`
  - Files: `Sources/HyaloShared/Shared/GeometrySizeTracker.swift`, `Sources/HyaloMac/Core/HyaloShared.swift`

### Wave 2 — Platform-Specific Types with #if os (after Wave 1)

- [x] 6. Move EffectView with #if os(macOS) guard + iOS stub

  **What to do**:
  - Create `Sources/HyaloShared/Appearance/EffectView.swift`
  - Wrap the entire implementation in `#if os(macOS)` ... `#else` ... `#endif`:
    - macOS block: Copy the `EffectView` struct (lines 81-102) from `Sources/HyaloMac/Appearance/VibrancyViews.swift` — this is an NSViewRepresentable wrapping NSVisualEffectView with `material` and `blendingMode` parameters
    - iOS block: Provide a stub `EffectView` that accepts the same parameters but returns `EmptyView()` or a `.background(.ultraThinMaterial)` equivalent
  - Remove `EffectView` (lines 81-102) from VibrancyViews.swift in HyaloMac
  - The iOS stub must match the same init signature: `EffectView(_ material: NSVisualEffectView.Material, blendingMode: NSVisualEffectView.BlendingMode)` — but on iOS those AppKit types don't exist, so the iOS stub should use platform-appropriate parameters or a simplified init
  - IMPORTANT: On iOS, `NSVisualEffectView.Material` and `.BlendingMode` don't exist. The iOS stub needs its own parameter types or use raw values. Look at how EffectView is actually called in consumers to determine the minimal API needed.
  - Consumers call: `EffectView(.sidebar, blendingMode: .behindWindow)` — so the iOS stub can simply ignore parameters and render a material background

  **Must NOT do**:
  - Do not change the macOS implementation
  - Do not change how consumers call EffectView

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 7, 8)
  - **Blocks**: Tasks 9, 11
  - **Blocked By**: Wave 1 complete

  **References**:
  - `Sources/HyaloMac/Appearance/VibrancyViews.swift:81-102` — Source: the full EffectView NSViewRepresentable
  - `Sources/HyaloShared/CommandPalette/CommandPaletteView.swift:63` — Consumer: `EffectView(.sidebar, blendingMode: .behindWindow)`
  - `Sources/HyaloShared/CommandPalette/OpenQuicklyView.swift:65` — Consumer: same usage pattern
  - `Sources/HyaloShared/Appearance/` — Pattern: existing appearance files in HyaloShared

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Appearance/EffectView.swift` exists
  - [ ] Contains `#if os(macOS)` and `#else` blocks
  - [ ] `EffectView` no longer defined in `Sources/HyaloMac/Appearance/VibrancyViews.swift`

  **QA Scenarios:**
  ```
  Scenario: EffectView compiles on macOS with existing call sites
    Tool: Bash
    Steps:
      1. grep -n '#if os(macOS)' Sources/HyaloShared/Appearance/EffectView.swift — expect match
      2. grep -n '#else' Sources/HyaloShared/Appearance/EffectView.swift — expect match
      3. grep -n '#endif' Sources/HyaloShared/Appearance/EffectView.swift — expect match
      4. grep -c 'struct EffectView' Sources/HyaloMac/Appearance/VibrancyViews.swift — expect 0
    Expected Result: EffectView in HyaloShared with platform guards, removed from HyaloMac
    Evidence: .sisyphus/evidence/task-6-effectview-move.txt

  Scenario: No EffectView errors in build
    Tool: Bash
    Steps:
      1. swift build 2>&1 | grep 'EffectView' | grep 'error:' | wc -l — expect 0
    Expected Result: Zero EffectView-related build errors
    Evidence: .sisyphus/evidence/task-6-effectview-build.txt
  ```

  **Commit**: YES (groups with Wave 2)
  - Message: `fix(shared): move EffectView with platform guards`
  - Files: `Sources/HyaloShared/Appearance/EffectView.swift`, `Sources/HyaloMac/Appearance/VibrancyViews.swift`

- [x] 7. Move InstantPopover system with #if os(macOS) guard + iOS fallback

  **What to do**:
  - Create `Sources/HyaloShared/Shared/InstantPopoverModifier.swift`
  - The full file `Sources/HyaloMac/Shared/InstantPopoverModifier.swift` (181 lines) contains:
    - `InstantPopoverModifier` (ViewModifier using NSPopover)
    - `InstantPopoverPresenter` (NSObject, NSPopoverDelegate)
    - `Edge.nsRectEdge` extension
    - `instantPopover` View extension
    - `InstantPopoverContainer` (View)
    - `DropdownItemStyleModifier` and `dropdownItemStyle` extension — HOWEVER, these will be moved separately in Task 8
  - Wrap the NSPopover-related types in `#if os(macOS)` block
  - For `#else` (iOS): provide `instantPopover` extension that delegates to SwiftUI `.popover()` with animation disabled if possible
  - The `instantPopover` call signature from consumers: `.instantPopover(isPresented: $showDropdown, arrowEdge: .bottom) { content }`
  - iOS fallback: `.popover(isPresented: isPresented, arrowEdge: arrowEdge) { content }`
  - Delete or empty the original file in HyaloMac (but check Task 8 first — if DropdownItemStyle is in same file, coordinate)

  **Must NOT do**:
  - Do not change the macOS NSPopover implementation
  - Do not change the View extension signature

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 6, 8)
  - **Blocks**: Task 11
  - **Blocked By**: Wave 1 complete

  **References**:
  - `Sources/HyaloMac/Shared/InstantPopoverModifier.swift:1-155` — Source: full popover system (excludes dropdown style at end)
  - `Sources/HyaloShared/Toolbar/EnvironmentDropDownView.swift:50` — Consumer: `.instantPopover(isPresented:arrowEdge:content:)`
  - `Sources/HyaloShared/Toolbar/UserHostDropDownView.swift` — Consumer: likely same pattern

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Shared/InstantPopoverModifier.swift` exists with `#if os(macOS)` guard
  - [ ] iOS fallback provides `.instantPopover()` using `.popover()`
  - [ ] Original file in HyaloMac is removed or reduced (only if Task 8 extracts dropdown separately)

  **QA Scenarios:**
  ```
  Scenario: instantPopover compiles for macOS
    Tool: Bash
    Steps:
      1. grep -n 'func instantPopover' Sources/HyaloShared/Shared/InstantPopoverModifier.swift — expect match
      2. grep -n '#if os(macOS)' Sources/HyaloShared/Shared/InstantPopoverModifier.swift — expect match
      3. swift build 2>&1 | grep 'instantPopover' | grep 'error:' | wc -l — expect 0
    Expected Result: instantPopover available in HyaloShared, no build errors
    Evidence: .sisyphus/evidence/task-7-instantpopover-move.txt
  ```

  **Commit**: YES (groups with Wave 2)
  - Message: `fix(shared): move instantPopover with platform guards`
  - Files: `Sources/HyaloShared/Shared/InstantPopoverModifier.swift`, `Sources/HyaloMac/Shared/InstantPopoverModifier.swift`

- [x] 8. Move DropdownItemStyle to HyaloShared (pure SwiftUI)

  **What to do**:
  - Create `Sources/HyaloShared/Shared/DropdownItemStyle.swift`
  - Copy from `Sources/HyaloMac/Shared/InstantPopoverModifier.swift`:
    - `DropdownItemStyleModifier` (lines ~159-175) — uses `.onHover`, `@State var isHovered`, pure SwiftUI
    - `dropdownItemStyle` View extension (lines ~177-180)
  - Remove those lines from the HyaloMac file
  - This is pure SwiftUI — no `#if os()` needed
  - NOTE: `.onHover` works on macOS. On iOS it does nothing (no hover events). This is fine — the style will simply not highlight on iOS, which is acceptable behavior.

  **Must NOT do**:
  - Do not add `#if os()` — this is cross-platform
  - Do not change the modifier implementation

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 6, 7)
  - **Blocks**: Task 11
  - **Blocked By**: Wave 1 complete

  **References**:
  - `Sources/HyaloMac/Shared/InstantPopoverModifier.swift:159-180` — Source: DropdownItemStyleModifier + extension
  - `Sources/HyaloShared/Shared/DropdownOptionView.swift:32` — Consumer: `.dropdownItemStyle(isSelected:style:)`

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Shared/DropdownItemStyle.swift` exists
  - [ ] `grep -r 'dropdownItemStyle' Sources/HyaloShared/Shared/DropdownItemStyle.swift` returns match
  - [ ] `grep -r 'DropdownItemStyleModifier' Sources/HyaloMac/` returns 0 matches

  **QA Scenarios:**
  ```
  Scenario: dropdownItemStyle available in HyaloShared
    Tool: Bash
    Steps:
      1. grep -rn 'struct DropdownItemStyleModifier' Sources/HyaloShared/Shared/DropdownItemStyle.swift
      2. grep -c 'DropdownItemStyleModifier' Sources/HyaloMac/Shared/InstantPopoverModifier.swift — expect 0
      3. swift build 2>&1 | grep 'dropdownItemStyle' | grep 'error:' | wc -l — expect 0
    Expected Result: DropdownItemStyle in HyaloShared, no build errors
    Evidence: .sisyphus/evidence/task-8-dropdownstyle-move.txt
  ```

  **Commit**: YES (groups with Wave 2)
  - Message: `fix(shared): move DropdownItemStyle to HyaloShared`
  - Files: `Sources/HyaloShared/Shared/DropdownItemStyle.swift`, `Sources/HyaloMac/Shared/InstantPopoverModifier.swift`

### Wave 3 — Terminal + Inspector Guards (after Wave 2)

- [x] 9. Move TerminalPalette to HyaloShared + add #if os guards in UtilityArea files

  **What to do**:
  This task has two parts:

  **Part A: Move TerminalPalette model**
  - Create `Sources/HyaloShared/Shared/TerminalPalette.swift`
  - Copy `TerminalPalette` class (lines 16-42) from `Sources/HyaloMac/Inspector/InspectorTerminalView.swift`
  - Also copy `terminalDefaultFontSize` constant (line 47) if it exists outside `#if os(macOS)` scope
  - The whole `InspectorTerminalView.swift` file is already `#if os(macOS)` guarded, so removing TerminalPalette from inside it means we need to place it OUTSIDE the guard in the new file (no guard needed — TerminalPalette is an `@Observable` class with plain properties, no AppKit)
  - Keep `import SwiftUI` and `import Observation` at top

  **Part B: Add #if os(macOS) guards in UtilityArea files**
  - In `Sources/HyaloShared/UtilityArea/UtilityAreaView.swift`:
    - Wrap references to `UtilityAreaTerminalView` and `UtilityAreaTerminalHolder` in `#if os(macOS)` blocks
    - In the `#else` block, provide a placeholder (e.g., `Text("Terminal not available")` or `EmptyView()`)
    - Also wrap `TerminalPalette.shared` references if they appear in view code that's terminal-specific
  - In `Sources/HyaloShared/UtilityArea/UtilityAreaViewModel.swift`:
    - Wrap the `UtilityAreaTerminalHolder()` creation (line ~19) in `#if os(macOS)` with an appropriate fallback

  **Must NOT do**:
  - Do not change TerminalPalette properties or API
  - Do not move UtilityAreaTerminalView or UtilityAreaTerminalHolder (they stay in HyaloMac)
  - Do not touch InspectorTerminalView.swift beyond removing the TerminalPalette definition

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Task 10)
  - **Blocks**: Task 11
  - **Blocked By**: Wave 2 complete

  **References**:
  - `Sources/HyaloMac/Inspector/InspectorTerminalView.swift:16-42` — Source: TerminalPalette class
  - `Sources/HyaloShared/UtilityArea/UtilityAreaView.swift:73-74` — Modify: wrap terminal references
  - `Sources/HyaloShared/UtilityArea/UtilityAreaViewModel.swift:19` — Modify: wrap UtilityAreaTerminalHolder() creation
  - `Sources/HyaloMac/UtilityArea/UtilityAreaTerminalView.swift` — Reference only: UtilityAreaTerminalView and UtilityAreaTerminalHolder stay here

  **Acceptance Criteria**:
  - [ ] File `Sources/HyaloShared/Shared/TerminalPalette.swift` exists with TerminalPalette class
  - [ ] `UtilityAreaView.swift` compiles without referencing HyaloMac-only types directly
  - [ ] `UtilityAreaViewModel.swift` compiles without referencing HyaloMac-only types directly
  - [ ] `swift build 2>&1 | grep -E 'TerminalPalette|UtilityAreaTerminal' | grep 'error:' | wc -l` returns 0

  **QA Scenarios:**
  ```
  Scenario: Terminal types properly guarded
    Tool: Bash
    Steps:
      1. grep -n '#if os(macOS)' Sources/HyaloShared/UtilityArea/UtilityAreaView.swift — expect match
      2. grep -n '#if os(macOS)' Sources/HyaloShared/UtilityArea/UtilityAreaViewModel.swift — expect match
      3. grep -rn 'class TerminalPalette' Sources/HyaloShared/ — expect match in TerminalPalette.swift
      4. swift build 2>&1 | grep -E 'TerminalPalette|UtilityAreaTerminal' | grep 'error:' | wc -l — expect 0
    Expected Result: All terminal references guarded, TerminalPalette in HyaloShared
    Evidence: .sisyphus/evidence/task-9-terminal-guards.txt
  ```

  **Commit**: YES (groups with Wave 3)
  - Message: `fix(shared): move TerminalPalette and guard terminal references`
  - Files: `Sources/HyaloShared/Shared/TerminalPalette.swift`, `Sources/HyaloShared/UtilityArea/UtilityAreaView.swift`, `Sources/HyaloShared/UtilityArea/UtilityAreaViewModel.swift`, `Sources/HyaloMac/Inspector/InspectorTerminalView.swift`

- [x] 10. Add #if os(macOS) guard for InspectorAppearanceView in InspectorTab

  **What to do**:
  - In `Sources/HyaloShared/Inspector/InspectorTab.swift`:
    - Find the reference to `InspectorAppearanceView()` (line ~35)
    - Wrap it in `#if os(macOS)` with an `#else` providing `EmptyView()` or `Text("Appearance settings not available")`
  - Do NOT move `InspectorAppearanceView` itself — it stays in HyaloMac because it's deeply coupled to `NSAppearance`, `HyaloModule.allControllers`, and other macOS-specific APIs

  **Must NOT do**:
  - Do not move InspectorAppearanceView.swift
  - Do not change other tabs in InspectorTab.swift

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Task 9)
  - **Blocks**: Task 11
  - **Blocked By**: Wave 2 complete

  **References**:
  - `Sources/HyaloShared/Inspector/InspectorTab.swift:35` — Modify: wrap InspectorAppearanceView() in `#if os(macOS)`
  - `Sources/HyaloMac/Inspector/InspectorAppearanceView.swift` — Reference only: stays in HyaloMac, deeply macOS-coupled

  **Acceptance Criteria**:
  - [ ] `InspectorTab.swift` contains `#if os(macOS)` around InspectorAppearanceView reference
  - [ ] `swift build 2>&1 | grep 'InspectorAppearanceView' | grep 'error:' | wc -l` returns 0

  **QA Scenarios:**
  ```
  Scenario: InspectorAppearanceView properly guarded
    Tool: Bash
    Steps:
      1. grep -n '#if os(macOS)' Sources/HyaloShared/Inspector/InspectorTab.swift — expect match
      2. grep -n 'InspectorAppearanceView' Sources/HyaloShared/Inspector/InspectorTab.swift — expect inside #if block
      3. swift build 2>&1 | grep 'InspectorAppearanceView' | grep 'error:' | wc -l — expect 0
    Expected Result: InspectorAppearanceView guarded, no build errors
    Evidence: .sisyphus/evidence/task-10-inspector-guard.txt
  ```

  **Commit**: YES (groups with Wave 3)
  - Message: `fix(shared): guard InspectorAppearanceView reference for macOS only`
  - Files: `Sources/HyaloShared/Inspector/InspectorTab.swift`

### Wave 4 — Cleanup and Documentation (after Wave 3)

- [x] 11. Final build verification + cleanup dead code in HyaloMac

  **What to do**:
  - Run `swift build` and verify 0 errors
  - Check `Sources/HyaloMac/Core/HyaloShared.swift` — after removing HyaloDesign (10-75), GeometrySizeTracker (80-113), and HyaloContentUnavailableView (143-200), only `HyaloVibrancyBackground` (122-138) should remain. If the file is nearly empty, consider if it should be renamed to `HyaloVibrancyBackground.swift` for clarity — but ONLY rename, no refactoring
  - Check `Sources/HyaloMac/Shared/InstantPopoverModifier.swift` — after Tasks 7 and 8 remove all content, this file should be deleted
  - Verify no duplicate type definitions exist (same type in both HyaloMac and HyaloShared)
  - Run `swift build 2>&1 | grep 'error:' | wc -l` and save output as evidence

  **Must NOT do**:
  - Do not change any type implementations
  - Do not add features

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after all Wave 1-3 tasks)
  - **Blocks**: Task 12, F1-F3
  - **Blocked By**: Tasks 1-10

  **References**:
  - All files modified in Tasks 1-10
  - `swift build` as primary verification

  **Acceptance Criteria**:
  - [ ] `swift build` exits with 0 errors
  - [ ] `swift build 2>&1 | tail -1` contains "Build complete!"
  - [ ] No type defined in both HyaloMac and HyaloShared (no duplicates)

  **QA Scenarios:**
  ```
  Scenario: Clean macOS build
    Tool: Bash
    Steps:
      1. swift build 2>&1 | grep 'error:' | wc -l — expect 0
      2. swift build 2>&1 | tail -1 — expect 'Build complete!'
    Expected Result: Zero errors, build succeeds
    Evidence: .sisyphus/evidence/task-11-final-build.txt

  Scenario: No duplicate type definitions
    Tool: Bash
    Steps:
      1. for type in HyaloDesign CommandItem OpenQuicklyItem HyaloContentUnavailableView GeometrySizeTracker EffectView TerminalPalette; do echo "==$type=="; grep -rn "struct $type\|class $type\|enum $type" Sources/; done
    Expected Result: Each type appears exactly once, in HyaloShared only
    Evidence: .sisyphus/evidence/task-11-no-duplicates.txt
  ```

  **Commit**: YES (groups with Wave 4)
  - Message: `fix(shared): cleanup dead code after type migration`
  - Files: any cleaned up files in HyaloMac

- [x] 12. Update PLAN.md with corrective Phase 1 notes

  **What to do**:
  - In `PLAN.md`, under Phase 1:
    - Add a new subsection `### 1.6 Phase 1 Corrective: Type Migration Fix` documenting what was done
    - List all types moved and guards added
    - Note the root cause: Phase 1 migration left types in HyaloMac that HyaloShared referenced
  - Update `## Critical Files Status` table to reflect new files
  - Ensure Phase 1 status stays `DONE` (the corrective work completes it)
  - Update CHANGELOG.md under [Unreleased] > Fixed

  **Must NOT do**:
  - Do not change Phase 2-4 content
  - Do not restructure the plan document

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after Task 11)
  - **Blocks**: F1-F3
  - **Blocked By**: Task 11

  **References**:
  - `PLAN.md:37-65` — Phase 1 section to update
  - `PLAN.md:409-448` — File layout and status tables to update
  - `CHANGELOG.md` — Add entry under [Unreleased] > Fixed

  **Acceptance Criteria**:
  - [ ] PLAN.md contains `### 1.6 Phase 1 Corrective` section
  - [ ] CHANGELOG.md has entry about type migration fix
  - [ ] File layout section reflects new files in HyaloShared

  **QA Scenarios:**
  ```
  Scenario: PLAN.md updated correctly
    Tool: Bash
    Steps:
      1. grep -n 'Phase 1 Corrective' PLAN.md — expect match
      2. grep -n 'type migration' CHANGELOG.md — expect match (case-insensitive)
    Expected Result: Both documents updated with corrective notes
    Evidence: .sisyphus/evidence/task-12-docs-update.txt
  ```

  **Commit**: YES (groups with Wave 4)
  - Message: `docs(plan): document Phase 1 corrective type migration`
  - Files: `PLAN.md`, `CHANGELOG.md`
---

## Final Verification Wave (MANDATORY — after ALL implementation tasks)

> 3 review agents run in PARALLEL. ALL must APPROVE. Rejection -> fix -> re-run.

- [ ] F1. **Plan Compliance Audit** — `oracle`
  Read the plan end-to-end. For each "Must Have": verify implementation exists (run `swift build`, check file locations). For each "Must NOT Have": search codebase for forbidden patterns — reject with file:line if found. Check evidence files exist in `.sisyphus/evidence/`. Compare deliverables against plan.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [ ] F2. **Code Quality Review** — `unspecified-high`
  Run `swift build`. Review all moved files for: stale imports, dead references to old locations, types still defined in both HyaloMac and HyaloShared (duplicates). Search for any remaining "cannot find type" errors. Verify no `#if os()` blocks were left incomplete (missing `#else` or `#endif`).
  Output: `Build [PASS/FAIL] | Duplicate Types [CLEAN/N issues] | Stale References [CLEAN/N issues] | VERDICT`

- [ ] F3. **Scope Fidelity Check** — `deep`
  For each task: read "What to do", read actual file changes. Verify 1:1 — everything in spec was moved (no missing), nothing beyond spec was changed (no creep). Check "Must NOT do" compliance. Flag any files that were modified but not listed in the plan.
  Output: `Tasks [N/N compliant] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **Wave 1**: `fix(shared): move cross-platform types from HyaloMac to HyaloShared` — HyaloDesign.swift, CommandItem.swift, OpenQuicklyItem.swift, HyaloContentUnavailableView.swift, GeometrySizeTracker.swift, modified HyaloMac source files
- **Wave 2**: `fix(shared): add platform-specific types with #if os guards` — EffectView.swift, InstantPopoverModifier.swift, DropdownItemStyle.swift, modified HyaloMac source files
- **Wave 3**: `fix(shared): add terminal and inspector platform guards` — TerminalPalette.swift, UtilityAreaView.swift, UtilityAreaViewModel.swift, InspectorTab.swift
- **Wave 4**: `docs(plan): update PLAN.md with Phase 1 corrective notes` — PLAN.md

---

## Success Criteria

### Verification Commands
```bash
swift build 2>&1 | grep "error:" | wc -l  # Expected: 0
swift build 2>&1 | tail -1                 # Expected: "Build complete!"
```

### Final Checklist
- [ ] `swift build` exits 0 with "Build complete!"
- [ ] All "Must Have" present (types in HyaloShared, #if guards, PLAN.md updated)
- [ ] All "Must NOT Have" absent (no refactoring, no new deps, no HyaloiOS changes)
- [ ] All evidence files in `.sisyphus/evidence/`
