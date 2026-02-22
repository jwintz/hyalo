import Foundation

public struct CommandItem: Identifiable, Hashable, Codable {
    public var id: String { name }  // Stable: command names are unique
    public var name: String
    public var description: String
    public var icon: String
    public var keybinding: String?
    public var category: String?
}

@available(macOS 26.0, iOS 26.0, *)
extension CommandItem: FuzzyMatchable {
    public var fuzzyMatchText: String {
        name + " " + description
    }
}
