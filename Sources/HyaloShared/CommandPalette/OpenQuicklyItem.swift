import Foundation

public struct OpenQuicklyItem: Identifiable, Hashable, Codable {
    public var id: String { path }
    public var name: String
    public var path: String
    public var icon: String
    public var relativePath: String?
}

extension OpenQuicklyItem: FuzzyMatchable {
    public var fuzzyMatchText: String {
        name + " " + (relativePath ?? path)
    }
}
