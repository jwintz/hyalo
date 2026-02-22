import Foundation

struct CommandItem: Identifiable, Hashable, Codable {
    var id: String { name }  // Stable: command names are unique
    var name: String
    var description: String
    var icon: String
    var keybinding: String?
    var category: String?
}

@available(macOS 26.0, *)
extension CommandItem: FuzzyMatchable {
    var fuzzyMatchText: String {
        name + " " + description
    }
}
