// HyaloFilePayload.swift - Minimal FileContents for filesystem-backed file tree
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Hyalo does not load file contents into the navigator — Emacs handles editing.
// This payload stores only the filesystem path; data() returns empty data.

import Foundation
import Files

struct HyaloFilePayload: FileContents, Sendable, Equatable {
    let path: String

    init(name: String, data: Data) throws {
        // Called by Files library when constructing from FileWrapper.
        // We don't use FileWrapper-based construction — this is a fallback.
        self.path = name
    }

    init(path: String) {
        self.path = path
    }

    func data() throws -> Data {
        // Navigator does not serialize file contents
        Data()
    }

    mutating func flush() throws {
        // No-op: contents are on disk, managed by Emacs
    }

    var text: String? { nil }
}
