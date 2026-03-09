// MinibufferCandidate.swift - Data model for minibuffer completion candidates
// Decoded from JSON sent by Emacs's completion framework (vertico/fido/generic).

import Foundation

public struct MinibufferCandidate: Identifiable, Hashable, Codable {
    public let text: String
    public let annotation: String
    public let selected: Bool
    /// Character-position ranges [start, end) where completion matches were highlighted.
    /// Nil when no highlight data was sent (e.g. history mode, generic fallback).
    public let matchRanges: [[Int]]?

    public var id: String { text }

    public init(text: String, annotation: String = "", selected: Bool = false, matchRanges: [[Int]]? = nil) {
        self.text = text
        self.annotation = annotation
        self.selected = selected
        self.matchRanges = matchRanges
    }
}
