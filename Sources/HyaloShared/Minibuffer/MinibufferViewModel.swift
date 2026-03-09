// MinibufferViewModel.swift - Observable view model for the minibuffer bridge
// Swift owns rendering; Emacs owns filtering/ordering.

import Foundation
import SwiftUI

/// JSON payload sent from Emacs for show/update.
public struct MinibufferPayload: Codable {
    public let sessionId: Int
    public let prompt: String
    public let input: String
    public let candidates: [MinibufferCandidate]
    public let selectedIndex: Int
    public let totalCandidates: Int
    public let historyMode: Bool?
}

@available(macOS 26.0, *)
@Observable
public final class MinibufferViewModel {
    public init() {}

    /// Reusable decoder — avoids allocating a new JSONDecoder per show/update call.
    private static let decoder = JSONDecoder()

    // MARK: - State

    public var isActive: Bool = false
    public var sessionId: Int = 0
    public var prompt: String = ""
    public var input: String = ""
    public var candidates: [MinibufferCandidate] = []
    public var selectedIndex: Int = -1
    public var totalCandidates: Int = 0
    public var historyMode: Bool = false

    /// Max annotation character count across visible candidates, used for column alignment.
    /// Computed on each candidate update — O(n) where n ≤ maxCandidates (50).
    public var annotationColumnChars: Int = 0

    private static let maxAnnotationColumnChars = 55

    // MARK: - Callbacks (wired by platform integration)

    public var onInputChanged: ((String) -> Void)?
    public var onCandidateSelected: ((Int) -> Void)?
    public var onAbort: (() -> Void)?
    public var onHistoryPrev: (() -> Void)?
    public var onHistoryNext: (() -> Void)?
    public var onTabComplete: (() -> Void)?

    // MARK: - Lifecycle

    /// Counts pending Emacs-originated input changes that must not be echoed back.
    /// Incremented before setting `input` from Emacs; decremented in SwiftUI's onChange.
    /// Uses a counter (not bool) so it survives the async gap between setting a property
    /// and SwiftUI firing onChange on the next render pass.
    private var pendingEmacsInputUpdates = 0

    public func show(from jsonData: Data) {
        guard let payload = try? Self.decoder.decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo] Failed to decode minibuffer show payload")
            return
        }
        sessionId = payload.sessionId
        prompt = payload.prompt
        historyMode = payload.historyMode ?? false
        candidates = payload.candidates
        selectedIndex = payload.selectedIndex
        totalCandidates = payload.totalCandidates
        recomputeAnnotationColumnWidth()
        // Reset stale sync flag from any previous session
        pendingEmacsSync = false
        // Set input from Emacs without triggering the inject-input callback
        pendingEmacsInputUpdates += 1
        input = payload.input
        isActive = true
    }

    /// Set true before sending a history-nav or tab-complete action to Emacs.
    /// `update()` will sync the returned `input` into the TextField once (suppressed callback).
    public var pendingEmacsSync: Bool = false

    public func update(from jsonData: Data) {
        #if DEBUG
        let t0 = CFAbsoluteTimeGetCurrent()
        #endif
        guard let payload = try? Self.decoder.decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo:Minibuffer] Failed to decode update payload (%d bytes)", jsonData.count)
            return
        }
        #if DEBUG
        let t1 = CFAbsoluteTimeGetCurrent()
        #endif
        // Reject stale updates from a previous session
        guard payload.sessionId == sessionId else {
            NSLog("[Hyalo:Minibuffer] stale update: got session %d, expected %d",
                  payload.sessionId, sessionId)
            return
        }
        candidates = payload.candidates
        selectedIndex = payload.selectedIndex
        totalCandidates = payload.totalCandidates
        recomputeAnnotationColumnWidth()
        // Sync input from Emacs when explicitly requested (history nav, tab completion)
        if pendingEmacsSync && payload.input != input {
            pendingEmacsSync = false
            pendingEmacsInputUpdates += 1
            input = payload.input
            NSLog("[Hyalo:Minibuffer] update: synced input from Emacs: %@", payload.input)
        } else {
            pendingEmacsSync = false
        }
        #if DEBUG
        let t2 = CFAbsoluteTimeGetCurrent()
        NSLog("[Hyalo:Minibuffer] update: decode=%.1fms assign=%.1fms cands=%d",
              (t1 - t0) * 1000, (t2 - t1) * 1000, candidates.count)
        #endif
    }

    public func hide() {
        isActive = false
        candidates = []
        selectedIndex = -1
        input = ""
        prompt = ""
        historyMode = false
        pendingEmacsSync = false
        pendingEmacsInputUpdates = 0
    }

    // MARK: - Layout

    private func recomputeAnnotationColumnWidth() {
        var maxLen = 0
        for c in candidates {
            if c.annotation.count > maxLen { maxLen = c.annotation.count }
        }
        annotationColumnChars = min(maxLen, Self.maxAnnotationColumnChars)
    }

    // MARK: - Navigation

    public func selectNext() {
        guard !candidates.isEmpty else { return }
        if selectedIndex < candidates.count - 1 {
            selectedIndex += 1
        } else {
            selectedIndex = 0
        }
    }

    public func selectPrevious() {
        guard !candidates.isEmpty else { return }
        if selectedIndex > 0 {
            selectedIndex -= 1
        } else {
            selectedIndex = candidates.count - 1
        }
    }

    public func confirm() {
        if historyMode {
            // Free-text / history mode: always submit current input.
            // After arrow navigation, the Emacs minibuffer text is already the
            // history item (updated by previous-history-element), so -1 is always correct.
            onCandidateSelected?(-1)
        } else if selectedIndex >= 0 && selectedIndex < candidates.count {
            // Completion mode: let vertico pick the highlighted candidate.
            onCandidateSelected?(selectedIndex)
        } else {
            // No valid vertico selection: submit current input.
            onCandidateSelected?(-1)
        }
    }

    // MARK: - Emacs-delegated actions (history nav, tab completion)

    public func historyPrev() {
        pendingEmacsSync = true
        onHistoryPrev?()
    }

    public func historyNext() {
        pendingEmacsSync = true
        onHistoryNext?()
    }

    public func tabComplete() {
        pendingEmacsSync = true
        onTabComplete?()
    }

    /// Called by MinibufferView's onChange. Returns true if the change should be
    /// forwarded to Emacs, false if it originated from Emacs (suppress echo-back).
    public func consumeEmacsInputUpdate() -> Bool {
        if pendingEmacsInputUpdates > 0 {
            pendingEmacsInputUpdates -= 1
            return false
        }
        return true
    }
}
