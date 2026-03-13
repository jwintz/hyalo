// EditorTabViewModel.swift - Editor tab bar state
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

// MARK: - Editor Tab Model

public struct EditorTab: Codable, Identifiable, Hashable {
    public let id: String
    public let name: String
    public let icon: String?
    public let isModified: Bool
    public let isTemporary: Bool
    public let filePath: String?

    public init(id: String, name: String, icon: String?, isModified: Bool, isTemporary: Bool, filePath: String?) {
        self.id = id
        self.name = name
        self.icon = icon
        self.isModified = isModified
        self.isTemporary = isTemporary
        self.filePath = filePath
    }
}

// MARK: - View Model

@available(macOS 26.0, *)
@MainActor
@Observable
public final class EditorTabViewModel {
    public init() {}
    public var tabs: [EditorTab] = []
    public var selectedTabId: String? {
        didSet { updateSelectedTab() }
    }

    /// Cached selected tab — avoids O(n) scan in view body.
    public private(set) var selectedTab: EditorTab?

    // Callbacks (set by channel)
    public var onTabSelect: ((String) -> Void)?
    public var onTabClose: ((String) -> Void)?
    public var onNavigateBack: (() -> Void)?
    public var onNavigateForward: (() -> Void)?

    private func updateSelectedTab() {
        selectedTab = tabs.first { $0.id == selectedTabId }
    }

    // MARK: - User Actions (called from SwiftUI)
    // Swift does NOT modify local state.  Emacs is the single source of truth.
    // The callback tells Emacs to switch; Emacs pushes the new state back
    // via hyalo-sync--push within ~20ms (requires wakeEmacs).

    public func selectTab(_ tab: EditorTab) {
        onTabSelect?(tab.id)
        wakeEmacs()
    }

    public func closeTab(_ tab: EditorTab) {
        onTabClose?(tab.id)
        wakeEmacs()
    }

    // MARK: - Updates from Emacs (callbacks)

    public func updateTabs(_ newTabs: [EditorTab]) {
        let incoming = Dictionary(uniqueKeysWithValues: newTabs.map { ($0.id, $0) })
        var result = tabs.compactMap { existing -> EditorTab? in
            incoming[existing.id]
        }
        let kept = Set(result.map(\.id))
        for tab in newTabs where !kept.contains(tab.id) {
            result.append(tab)
        }
        tabs = result
        updateSelectedTab()
    }

    /// Called from Emacs via hyalo-select-editor-tab when a buffer becomes active.
    /// Emacs is the single source of truth — always accept.
    public func onTabSelected(_ id: String) {
        selectedTabId = id
    }

    /// Direct tab selection without logging (for internal use).
    public func selectTabById(_ id: String) {
        selectedTabId = id
    }
}
