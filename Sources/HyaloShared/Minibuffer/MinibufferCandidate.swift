// MinibufferCandidate.swift - Data model for minibuffer completion candidates
// Decoded from JSON sent by Emacs's completion framework (vertico/fido/generic).

import Foundation

public struct MinibufferCandidate: Identifiable, Hashable, Codable {
    public let text: String
    public let annotation: String
    public let selected: Bool

    public var id: String { text }

    public init(text: String, annotation: String = "", selected: Bool = false) {
        self.text = text
        self.annotation = annotation
        self.selected = selected
    }
}
