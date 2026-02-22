import Foundation

struct OpenQuicklyItem: Identifiable, Hashable, Codable {
    var id: String { path }
    var name: String
    var path: String
    var icon: String
    var relativePath: String?
}

extension OpenQuicklyItem: FuzzyMatchable {
    var fuzzyMatchText: String {
        name + " " + (relativePath ?? path)
    }
}
