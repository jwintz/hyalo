# Draft: Hyalo iOS Port - Next Steps

## Current State (assessed 2026-02-22)

### macOS Build: BROKEN
The macOS `swift build` is currently failing with ~100+ errors. All errors stem from the Phase 1 migration that moved files from `Sources/Hyalo/` into `Sources/HyaloShared/` (cross-platform) and `Sources/HyaloMac/` (macOS-only). Several types/views that HyaloShared references are still only defined in HyaloMac:

**Missing types in HyaloShared (defined in HyaloMac):**
- `HyaloDesign` (enum, design constants) — in `HyaloMac/Core/HyaloShared.swift`
- `CommandItem` (struct) — in `HyaloMac/CommandPalette/CommandPaletteManager.swift`
- `OpenQuicklyItem` (struct) — in `HyaloMac/CommandPalette/CommandPaletteManager.swift`
- `HyaloContentUnavailableView` (struct) — in `HyaloMac/Core/HyaloShared.swift`
- `EffectView` (NSViewRepresentable) — in `HyaloMac/Appearance/VibrancyViews.swift` (AppKit-specific)
- `TerminalPalette` — in HyaloMac terminal code
- `UtilityAreaTerminalView` — in `HyaloMac/UtilityArea/UtilityAreaTerminalView.swift`
- `UtilityAreaTerminalHolder` — in HyaloMac terminal code
- `InspectorAppearanceView` — references macOS-specific appearance controls
- `instantPopover` (view modifier) — in `HyaloMac/Shared/InstantPopoverModifier.swift`
- `dropdownItemStyle` (view modifier) — in HyaloMac

### Phase 1: DONE (per PLAN.md) — but build is broken
- Package.swift restructured (3 targets: HyaloShared, Hyalo/macOS, HyaloKit/iOS)
- 56 files migrated to HyaloShared
- 17 files in HyaloMac
- 6 files in HyaloiOS (skeleton)
- Platform abstraction layer (Platform.swift)

### Phase 2: IN PROGRESS
- Tasks 2.1-2.6 all marked as TODO
- HyaloiOS files exist but are skeleton/stub implementations
- ChannelBridge has basic navigator/editor/status/appearance stubs

### Key Insight: Phase 1 is NOT actually done
The migration left HyaloShared referencing types that still live in HyaloMac. This is a dependency inversion — HyaloShared cannot depend on HyaloMac. These types must be either:
1. Moved to HyaloShared (if truly cross-platform)
2. Abstracted via protocols (if platform-specific)
3. Wrapped in `#if os()` blocks (if partially platform-specific)

## Categorized Missing Types

### Category A: Purely cross-platform (move to HyaloShared)
- `HyaloDesign` — pure constants, no AppKit/UIKit
- `CommandItem` — Codable model struct
- `OpenQuicklyItem` — Codable model struct
- `HyaloContentUnavailableView` — pure SwiftUI
- `GeometrySizeTracker` — pure SwiftUI (already in HyaloShared.swift but file is in HyaloMac target)

### Category B: Platform-specific (need abstraction or #if os)
- `EffectView` — NSViewRepresentable (macOS) vs UIViewRepresentable (iOS)
- `HyaloVibrancyBackground` — NSViewRepresentable
- `instantPopover` — uses AppKit popover APIs
- `dropdownItemStyle` — may use AppKit-specific styling
- `InspectorAppearanceView` — references macOS-specific appearance controls
- `UtilityAreaTerminalView` / `UtilityAreaTerminalHolder` / `TerminalPalette` — SwiftTerm with AppKit

## Open Questions
- [pending] What's the priority: fix macOS build first, or continue iOS work?
- [pending] Should terminal views be stubbed out on iOS for now?
- [pending] FuzzyMatcher generic issue — is FuzzyMatchable conformance missing for the item types?

## PLAN.md Accuracy
- Phase 1 is marked DONE but the build is broken — should be marked IN PROGRESS
- Phase 2 items 2.1-2.6 are accurate but blocked by Phase 1 completion
- The PLAN.md does mention "2.3 Resolve Type Visibility" but undersells the scope — it's not just access control, several types are in the wrong target entirely
