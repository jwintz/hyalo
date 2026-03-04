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

@available(macOS 26.0, iOS 26.0, *)
@Observable
public final class MinibufferViewModel {
    public init() {}

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

    // MARK: - Lifecycle

    /// Suppresses `onInputChanged` during `show()` to avoid echoing the initial input back to Emacs.
    private var suppressInputCallback = false

    public func show(from jsonData: Data) {
        guard let payload = try? JSONDecoder().decode(MinibufferPayload.self, from: jsonData) else {
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
        // Set input from Emacs (e.g. compile-command default) without triggering callback
        suppressInputCallback = true
        input = payload.input
        suppressInputCallback = false
        isActive = true
    }

    public func update(from jsonData: Data) {
        let t0 = CFAbsoluteTimeGetCurrent()
        guard let payload = try? JSONDecoder().decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo:Minibuffer] Failed to decode update payload (%d bytes)", jsonData.count)
            return
        }
        let t1 = CFAbsoluteTimeGetCurrent()
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
        let t2 = CFAbsoluteTimeGetCurrent()
        NSLog("[Hyalo:Minibuffer] update: decode=%.1fms assign=%.1fms cands=%d",
              (t1 - t0) * 1000, (t2 - t1) * 1000, candidates.count)
    }

    public func hide() {
        isActive = false
        candidates = []
        selectedIndex = -1
        input = ""
        prompt = ""
        historyMode = false
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
        if historyMode && (selectedIndex < 0 || candidates.isEmpty) {
            // Free-text: submit current input as-is (index -1 signals confirm-input)
            onCandidateSelected?(-1)
        } else if selectedIndex >= 0 && selectedIndex < candidates.count {
            onCandidateSelected?(selectedIndex)
        } else if !candidates.isEmpty {
            onCandidateSelected?(0)
        } else {
            // No candidates at all (e.g. empty history): submit current input
            onCandidateSelected?(-1)
        }
    }

    /// Whether the input callback should fire. Used by `MinibufferView`'s onChange.
    public var shouldFireInputCallback: Bool {
        !suppressInputCallback
    }
}
