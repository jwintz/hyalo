// HyaloFilePayload.swift - File tree node model for filesystem-backed navigator
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Simple recursive tree node used by the native file navigator.
// Each node has a name, absolute path, directory flag, and optional children.

import Foundation

/// A single node in the file tree. Directories have children; files do not.
/// Uses `path` as stable identity so SwiftUI can diff across tree rebuilds.
public final class FileTreeNode: Identifiable {
    public var id: String { path }
    public let name: String
    public let path: String
    public let isDirectory: Bool
    public var children: [FileTreeNode]?

    public init(name: String, path: String, isDirectory: Bool, children: [FileTreeNode]? = nil) {
        self.name = name
        self.path = path
        self.isDirectory = isDirectory
        self.children = children
    }
}
