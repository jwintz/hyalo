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

    // MARK: - Callbacks (wired by platform integration)

    public var onInputChanged: ((String) -> Void)?
    public var onCandidateSelected: ((Int) -> Void)?
    public var onAbort: (() -> Void)?

    // MARK: - Lifecycle

    public func show(from jsonData: Data) {
        guard let payload = try? JSONDecoder().decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo] Failed to decode minibuffer show payload")
            return
        }
        sessionId = payload.sessionId
        prompt = payload.prompt
        input = ""
        candidates = payload.candidates
        selectedIndex = payload.selectedIndex
        totalCandidates = payload.totalCandidates
        isActive = true
    }

    public func update(from jsonData: Data) {
        guard let payload = try? JSONDecoder().decode(MinibufferPayload.self, from: jsonData) else {
            NSLog("[Hyalo] Failed to decode minibuffer update payload")
            return
        }
        // Reject stale updates from a previous session
        guard payload.sessionId == sessionId else { return }
        candidates = payload.candidates
        selectedIndex = payload.selectedIndex
        totalCandidates = payload.totalCandidates
    }

    public func hide() {
        isActive = false
        candidates = []
        selectedIndex = -1
        input = ""
        prompt = ""
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
        if selectedIndex >= 0 && selectedIndex < candidates.count {
            onCandidateSelected?(selectedIndex)
        } else if !candidates.isEmpty {
            onCandidateSelected?(0)
        }
    }
}
