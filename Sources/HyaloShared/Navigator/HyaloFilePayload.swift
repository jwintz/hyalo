// HyaloFilePayload.swift - Minimal FileContents for filesystem-backed file tree
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Hyalo does not load file contents into the navigator — Emacs handles editing.
// This payload stores only the filesystem path; data() returns empty data.

import Foundation
import Files

public struct HyaloFilePayload: FileContents, Sendable, Equatable {
    public let path: String

    public init(name: String, data: Data) throws {
        // Called by Files library when constructing from FileWrapper.
        // We don't use FileWrapper-based construction — this is a fallback.
        self.path = name
    }

    public init(path: String) {
        self.path = path
    }

    public func data() throws -> Data {
        // Navigator does not serialize file contents
        Data()
    }

    public mutating func flush() throws {
        // No-op: contents are on disk, managed by Emacs
    }

    public var text: String? { nil }
}
