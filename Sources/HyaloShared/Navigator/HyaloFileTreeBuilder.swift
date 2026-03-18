// HyaloFileTreeBuilder.swift - Builds FileTreeNode from filesystem directory
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Scans a project root directory and produces a FileTreeNode tree.
// Runs entirely in Swift — no Emacs round-trip.
// FileManager operations run off-main-thread; results are cached.

import Foundation

@available(macOS 26.0, *)
@MainActor
public enum HyaloFileTreeBuilder {

    /// Directories to exclude from the file tree.
    nonisolated private static let excludedDirectories: Set<String> = [
        ".git", ".build", ".swiftpm", "node_modules", "__pycache__",
        ".DS_Store", "DerivedData", ".local"
    ]

    // MARK: - Cache

    private static var cachedRoot: String?
    private static var cachedTree: FileTreeNode?
    private static var cachedFoldersOnTop: Bool = true
    private static var cacheTimestamp: Date?
    private static let cacheTTL: TimeInterval = 5.0

    /// Invalidate the file tree cache (call after known filesystem changes).
    public static func invalidateCache() {
        cachedRoot = nil
        cachedTree = nil
        cacheTimestamp = nil
    }

    /// Build a FileTreeNode tree from a filesystem directory.
    ///
    /// Returns a cached result if the same root was scanned within `cacheTTL`.
    /// - Parameters:
    ///   - root: Absolute path to the project root directory.
    ///   - maxDepth: Maximum recursion depth (default 10).
    ///   - foldersOnTop: Sort folders before files.
    /// - Returns: A root FileTreeNode, or nil if the directory is unreadable.
    public static func buildFileTree(
        root: String,
        maxDepth: Int = 10,
        foldersOnTop: Bool = true
    ) -> FileTreeNode? {
        if let cached = cachedTree,
           cachedRoot == root,
           cachedFoldersOnTop == foldersOnTop,
           let ts = cacheTimestamp,
           Date().timeIntervalSince(ts) < cacheTTL {
            return cached
        }
        let rootURL = URL(fileURLWithPath: root, isDirectory: true)
        let result = buildNode(at: rootURL, depth: maxDepth, foldersOnTop: foldersOnTop)
        cachedRoot = root
        cachedTree = result
        cachedFoldersOnTop = foldersOnTop
        cacheTimestamp = Date()
        return result
    }

    /// Build a FileTreeNode tree asynchronously, off the main thread.
    public static func buildFileTreeAsync(
        root: String,
        maxDepth: Int = 10,
        foldersOnTop: Bool = true
    ) async -> FileTreeNode? {
        let rootCopy = root
        let depth = maxDepth
        let folders = foldersOnTop
        return await Task.detached(priority: .userInitiated) {
            let rootURL = URL(fileURLWithPath: rootCopy, isDirectory: true)
            return buildNodeOffMain(at: rootURL, depth: depth, foldersOnTop: folders)
        }.value
    }

    /// Build a git status lookup table for the project root.
    nonisolated public static func gitStatusMap(root: String) -> [String: String] {
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
        guard process.terminationStatus == 0 else { return [:] }
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        guard let output = String(data: data, encoding: .utf8) else { return [:] }
        var result: [String: String] = [:]
        result.reserveCapacity(64)
        for line in output.split(separator: "\n", omittingEmptySubsequences: true) where line.count >= 4 {
            let code = line.prefix(2)
            var fileSub = line.dropFirst(3)
            if let arrowRange = fileSub.range(of: " -> ") {
                fileSub = fileSub[arrowRange.upperBound...]
            }
            let absolutePath = (root as NSString).appendingPathComponent(String(fileSub))
            // Single-pass status classification using the two-char code
            let c0 = code[code.startIndex]
            let c1 = code[code.index(after: code.startIndex)]
            let status: String? =
                (c0 == "M" || c1 == "M") ? "M" :
                (c0 == "A" || c1 == "A") ? "A" :
                (c0 == "D" || c1 == "D") ? "D" :
                (c0 == "R" || c1 == "R") ? "R" :
                (c0 == "?" || c1 == "?") ? "?" : nil
            if let status {
                result[absolutePath] = status
            }
        }
        return result
    }

    // MARK: - Private (MainActor)

    private static func buildNode(
        at url: URL,
        depth: Int,
        foldersOnTop: Bool
    ) -> FileTreeNode? {
        buildNodeOffMain(at: url, depth: depth, foldersOnTop: foldersOnTop)
    }

    // MARK: - Private (nonisolated, safe for background)

    private nonisolated static func buildNodeOffMain(
        at url: URL,
        depth: Int,
        foldersOnTop: Bool
    ) -> FileTreeNode? {
        let fm = FileManager.default
        let name = url.lastPathComponent
        let path = url.path

        var isDir: ObjCBool = false
        guard fm.fileExists(atPath: path, isDirectory: &isDir) else { return nil }

        if !isDir.boolValue {
            return FileTreeNode(name: name, path: path, isDirectory: false)
        }

        guard depth > 0 else {
            return FileTreeNode(name: name, path: path, isDirectory: true, children: [])
        }

        // Prefetch isDirectory to avoid per-entry resourceValues calls
        guard let entries = try? fm.contentsOfDirectory(
            at: url,
            includingPropertiesForKeys: [.isDirectoryKey],
            options: [.skipsHiddenFiles]
        ) else {
            return nil
        }

        struct ClassifiedEntry {
            let url: URL
            let name: String
            let isDirectory: Bool
        }

        let classified: [ClassifiedEntry] = entries.compactMap { entry in
            let entryName = entry.lastPathComponent
            if excludedDirectories.contains(entryName) { return nil }
            let isEntryDir = (try? entry.resourceValues(forKeys: [.isDirectoryKey]).isDirectory) ?? false
            return ClassifiedEntry(url: entry, name: entryName, isDirectory: isEntryDir)
        }.sorted {
            if foldersOnTop && $0.isDirectory != $1.isDirectory {
                return $0.isDirectory
            }
            return $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending
        }

        let children: [FileTreeNode] = classified.compactMap { entry in
            if entry.isDirectory {
                return buildNodeOffMain(at: entry.url, depth: depth - 1, foldersOnTop: foldersOnTop)
            } else {
                return FileTreeNode(name: entry.name, path: entry.url.path, isDirectory: false)
            }
        }

        return FileTreeNode(name: name, path: path, isDirectory: true, children: children)
    }
}
