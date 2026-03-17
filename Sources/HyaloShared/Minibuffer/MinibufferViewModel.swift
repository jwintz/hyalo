// MinibufferViewModel.swift - Observable view model for the minibuffer bridge
// Swift owns rendering; Emacs owns filtering/ordering and text editing.

import Foundation
import SwiftUI

/// JSON payload sent from Emacs for show/update.
public struct MinibufferPayload: Codable {
    public let sessionId: Int
    public let prompt: String
    public let input: String
    public let cursorPosition: Int?
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
    public var cursorPosition: Int = 0
    public var candidates: [MinibufferCandidate] = []
    public var selectedIndex: Int = -1
    public var totalCandidates: Int = 0
    public var historyMode: Bool = false

    /// Max annotation character count across visible candidates, used for column alignment.
    /// Computed on each candidate update — O(n) where n ≤ maxCandidates (50).
    public var annotationColumnChars: Int = 0

    private static let maxAnnotationColumnChars = 55

    // MARK: - Callbacks (wired by platform integration)

    public var onCandidateSelected: ((Int) -> Void)?
    public var onAbort: (() -> Void)?

    // MARK: - Lifecycle

    public func show(from jsonData: Data) {
        guard let payload = try? Self.decoder.decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo] Failed to decode minibuffer show payload")
            return
        }
        sessionId = payload.sessionId
        prompt = payload.prompt
        input = payload.input
        cursorPosition = payload.cursorPosition ?? payload.input.count
        historyMode = payload.historyMode ?? false
        candidates = payload.candidates
        selectedIndex = payload.selectedIndex
        totalCandidates = payload.totalCandidates
        recomputeAnnotationColumnWidth()
        isActive = true
    }

    public func update(from jsonData: Data) {
        guard let payload = try? Self.decoder.decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo:Minibuffer] Failed to decode update payload (%d bytes)", jsonData.count)
            return
        }
        // Reject stale updates from a previous session
        guard payload.sessionId == sessionId else { return }
        input = payload.input
        cursorPosition = payload.cursorPosition ?? payload.input.count
        candidates = payload.candidates
        selectedIndex = payload.selectedIndex
        totalCandidates = payload.totalCandidates
        recomputeAnnotationColumnWidth()
    }

    public func hide() {
        isActive = false
        candidates = []
        selectedIndex = -1
        input = ""
        cursorPosition = 0
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
}
