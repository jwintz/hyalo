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



    /// The tab id set by the last UI-initiated click, and when.
    /// Used to reject stale hook calls that arrive within the roundtrip window.
    private var uiSelectId: String?
    private var uiSelectTime: Date = .distantPast

    /// Roundtrip guard window — 100ms covers channel roundtrip + hook firing.
    private static let guardWindow: TimeInterval = 0.1

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
        selectedTabId = tab.id
        uiSelectId = tab.id
        uiSelectTime = Date()
        NSLog("[Hyalo:EditorTab] selectTab: id=%@", tab.id)
        onTabSelect?(tab.id)
    }

    /// Called when the user selects a file from the navigator or buffer list.
    /// Sets the guard so stale hook calls during the roundtrip are rejected.
    func selectFile(_ path: String, bufferName: String) {
        uiSelectId = bufferName
        uiSelectTime = Date()
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

    /// Called from Emacs via hyalo-select-editor-tab when a buffer becomes active.
    /// Within 100ms of a UI-initiated tab click, only accepts the matching id
    /// to prevent stale hook calls from reverting the selection.  Outside that
    /// window (windmove, programmatic switch), all updates pass through.
    ///
    /// The guard is NOT cleared on match — it stays active for the full 100ms
    /// window.  Clearing on the first echo opened a 1ms gap where a stale hook
    /// call could revert the selection before the window expired.
    func onTabSelected(_ id: String) {
        if let pending = uiSelectId,
           Date().timeIntervalSince(uiSelectTime) < Self.guardWindow,
           id != pending {
            NSLog("[Hyalo:EditorTab] onTabSelected: GUARDED id=%@ (pending=%@)", id, pending)
            return
        }
        NSLog("[Hyalo:EditorTab] onTabSelected: id=%@ selectedTabId=%@",
              id, selectedTabId ?? "nil")
        selectedTabId = id
    }

    /// Direct tab selection without stale check (for internal use).
    func selectTabById(_ id: String) {
        NSLog("[Hyalo:EditorTab] selectTabById: id=%@", id)
        selectedTabId = id
    }
}
