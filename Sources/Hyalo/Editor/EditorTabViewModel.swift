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

    // Track the tab/buffer that was sent to Emacs to detect stale callbacks.
    // Set immediately in selectTab() and selectFile(), checked in onTabSelected().
    private var pendingTabId: String?

    // Callbacks (set by channel)
    var onTabSelect: ((String) -> Void)?
    var onTabClose: ((String) -> Void)?
    var onNavigateBack: (() -> Void)?
    var onNavigateForward: (() -> Void)?

    var selectedTab: EditorTab? {
        tabs.first { $0.id == selectedTabId }
    }

    // MARK: - User Actions (called from SwiftUI)

    func selectTab(_ tab: EditorTab) {
        // Set pendingTabId to detect stale callbacks from Emacs.
        pendingTabId = tab.id
        selectedTabId = tab.id
        onTabSelect?(tab.id)
    }

    /// Called when user selects a file from the navigator.
    /// Sets pendingTabId so we can detect stale tab callbacks.
    func selectFile(_ path: String, bufferName: String) {
        pendingTabId = bufferName
        // The actual tab selection happens via hyalo-select-editor-tab from Emacs
    }

    func closeTab(_ tab: EditorTab) {
        onTabClose?(tab.id)
        tabs.removeAll { $0.id == tab.id }
        if selectedTabId == tab.id {
            selectedTabId = tabs.last?.id
        }
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

    /// Called from Emacs via hyalo-select-editor-tab when a file is opened.
    /// Checks pendingTabId to skip stale callbacks.
    func onTabSelected(_ id: String) {
        // Skip stale callbacks from Emacs.
        // pendingTabId tracks the most recent user-initiated selection.
        guard let pending = pendingTabId else {
            selectedTabId = id
            return
        }
        if id != pending {
            return
        }
        selectedTabId = id
        // DO NOT clear pendingTabId - it tracks the most recent user action
        // and protects against late stale callbacks
    }

    /// Direct tab selection without stale check (for internal use).
    func selectTabById(_ id: String) {
        selectedTabId = id
    }
}
