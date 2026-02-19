// HyaloFileTreeBuilder.swift - Builds FileTree from filesystem directory
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Scans a project root directory and produces a Files.FileTree<HyaloFilePayload>
// with git status annotations. Runs entirely in Swift — no Emacs round-trip.

import Foundation
import OrderedCollections
import Files

@available(macOS 26.0, *)
@MainActor
enum HyaloFileTreeBuilder {

    /// Directories to exclude from the file tree.
    private static let excludedDirectories: Set<String> = [
        ".git", ".build", ".swiftpm", "node_modules", "__pycache__",
        ".DS_Store", "DerivedData", ".local"
    ]

    /// Build a FileTree from a filesystem directory.
    ///
    /// - Parameters:
    ///   - root: Absolute path to the project root directory.
    ///   - maxDepth: Maximum recursion depth (default 10).
    /// - Returns: A populated FileTree, or nil if the directory is unreadable.
    static func buildFileTree(
        root: String,
        maxDepth: Int = 10,
        foldersOnTop: Bool = true
    ) -> FileTree<HyaloFilePayload>? {
        guard let item = buildFullFileTree(root: root, maxDepth: maxDepth, foldersOnTop: foldersOnTop) else {
            return nil
        }
        return FileTree(files: item)
    }

    /// Build a root FileOrFolder item from a filesystem directory.
    static func buildFullFileTree(
        root: String,
        maxDepth: Int = 10,
        foldersOnTop: Bool = true
    ) -> FullFileOrFolder<HyaloFilePayload>? {
        let rootURL = URL(fileURLWithPath: root, isDirectory: true)

        guard let folder = buildFolder(at: rootURL, depth: maxDepth, foldersOnTop: foldersOnTop) else {
            return nil
        }

        return FullFileOrFolder<HyaloFilePayload>(folder: folder)
    }

    /// Build a git status lookup table for the project root.
    ///
    /// - Parameter root: Absolute path to the project root directory.
    /// - Returns: Dictionary mapping absolute file paths to status codes ("M", "A", "D", "?", "R").
    static func gitStatusMap(root: String) -> [String: String] {
        // Verify .git directory exists before spawning a subprocess
        let gitDir = (root as NSString).appendingPathComponent(".git")
        guard FileManager.default.fileExists(atPath: gitDir) else { return [:] }

        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/git")
        process.arguments = ["status", "--porcelain"]
        process.currentDirectoryURL = URL(fileURLWithPath: root)

        let pipe = Pipe()
        process.standardOutput = pipe
        process.standardError = FileHandle.nullDevice

        do {
            try process.run()
            process.waitUntilExit()
        } catch {
            return [:]
        }

        // Non-zero exit (e.g., corrupt repo) — return empty
        guard process.terminationStatus == 0 else { return [:] }

        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        guard let output = String(data: data, encoding: .utf8) else { return [:] }

        var result: [String: String] = [:]
        for line in output.components(separatedBy: "\n") where line.count >= 4 {
            let code = String(line.prefix(2))
            var file = String(line.dropFirst(3))
            // Handle renames: "R  old -> new"
            if let arrowRange = file.range(of: " -> ") {
                file = String(file[arrowRange.upperBound...])
            }
            let absolutePath = (root as NSString).appendingPathComponent(file)
            let status: String? = {
                if code.contains("M") { return "M" }
                if code.contains("A") { return "A" }
                if code.contains("D") { return "D" }
                if code.contains("R") { return "R" }
                if code.contains("?") { return "?" }
                return nil
            }()
            if let status {
                result[absolutePath] = status
            }
        }
        return result
    }

    // MARK: - Private

    private static func buildFolder(
        at url: URL,
        depth: Int,
        foldersOnTop: Bool
    ) -> FullFolder<HyaloFilePayload>? {
        guard depth > 0 else { return Folder(children: [:]) }

        let fm = FileManager.default
        guard let entries = try? fm.contentsOfDirectory(
            at: url,
            includingPropertiesForKeys: [.isDirectoryKey, .isHiddenKey],
            options: [.skipsHiddenFiles]
        ) else {
            return nil
        }

        var children = OrderedDictionary<String, FullFileOrFolder<HyaloFilePayload>>()

        // Classify and sort entries
        struct ClassifiedEntry {
            let url: URL
            let name: String
            let isDirectory: Bool
        }

        let classified: [ClassifiedEntry] = entries.compactMap { entry in
            let name = entry.lastPathComponent
            if excludedDirectories.contains(name) { return nil }
            let isDir = (try? entry.resourceValues(forKeys: [.isDirectoryKey]).isDirectory) ?? false
            return ClassifiedEntry(url: entry, name: name, isDirectory: isDir)
        }.sorted {
            $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending
        }

        if foldersOnTop {
            // Folders first
            for entry in classified where entry.isDirectory {
                if let subfolder = buildFolder(at: entry.url, depth: depth - 1, foldersOnTop: foldersOnTop) {
                    children[entry.name] = .folder(subfolder)
                }
            }
            // Then files
            for entry in classified where !entry.isDirectory {
                let payload = HyaloFilePayload(path: entry.url.path)
                let file = File<HyaloFilePayload>(contents: payload)
                children[entry.name] = .file(file)
            }
        } else {
            // Mixed alphabetical sorting
            for entry in classified {
                if entry.isDirectory {
                    if let subfolder = buildFolder(at: entry.url, depth: depth - 1, foldersOnTop: foldersOnTop) {
                        children[entry.name] = .folder(subfolder)
                    }
                } else {
                    let payload = HyaloFilePayload(path: entry.url.path)
                    let file = File<HyaloFilePayload>(contents: payload)
                    children[entry.name] = .file(file)
                }
            }
        }

        return Folder(children: children)
    }
}
