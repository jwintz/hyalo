// EditorTabViewModel.swift - Editor tab bar state
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

// MARK: - Editor Tab Model

struct EditorTab: Codable, Identifiable, Hashable {
    let id: String
    let name: String
    let icon: String?
    let isModified: Bool
    let isTemporary: Bool
    let filePath: String?
}

// MARK: - View Model

@available(macOS 26.0, *)
@MainActor
@Observable
final class EditorTabViewModel {
    var tabs: [EditorTab] = []
    var selectedTabId: String?

    // Callbacks (set by channel)
    var onTabSelect: ((String) -> Void)?
    var onTabClose: ((String) -> Void)?
    var onNavigateBack: (() -> Void)?
    var onNavigateForward: (() -> Void)?

    var selectedTab: EditorTab? {
        tabs.first { $0.id == selectedTabId }
    }

    // MARK: - User Actions (called from SwiftUI)
    // Swift does NOT modify local state.  Emacs is the single source of truth.
    // The callback tells Emacs to switch; Emacs pushes the new state back
    // via hyalo-sync--push within ~20ms (requires wakeEmacs).

    func selectTab(_ tab: EditorTab) {
        onTabSelect?(tab.id)
        HyaloModule.wakeEmacs()
    }

    func closeTab(_ tab: EditorTab) {
        onTabClose?(tab.id)
        HyaloModule.wakeEmacs()
    }

    // MARK: - Updates from Emacs (callbacks)

    func updateTabs(_ newTabs: [EditorTab]) {
        let incoming = Dictionary(uniqueKeysWithValues: newTabs.map { ($0.id, $0) })
        var result = tabs.compactMap { existing -> EditorTab? in
            incoming[existing.id]
        }
        let kept = Set(result.map(\.id))
        for tab in newTabs where !kept.contains(tab.id) {
            result.append(tab)
        }
        tabs = result
    }

    /// Called from Emacs via hyalo-select-editor-tab when a buffer becomes active.
    /// Emacs is the single source of truth — always accept.
    func onTabSelected(_ id: String) {
        selectedTabId = id
    }

    /// Direct tab selection without logging (for internal use).
    func selectTabById(_ id: String) {
        selectedTabId = id
    }
}
