// ProjectNavigatorViewModel.swift - File tree state using native FileTreeNode
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Manages a FileTreeNode tree built from the filesystem.
// Provides filtering, active file tracking, and expansion state.
// Exposes a flattened row array for virtualized rendering.

import Foundation
import SwiftUI

/// A single visible row in the flattened tree, carrying its indentation depth.
public struct FlatRow: Identifiable {
    public var id: String { node.id }
    public let node: FileTreeNode
    public let depth: Int
}

@available(macOS 26.0, *)
@MainActor
@Observable
public final class ProjectNavigatorViewModel {
    // MARK: - File Tree State

    /// The root node of the file tree (unfiltered master copy).
    private var masterRoot: FileTreeNode?

    /// The root node after applying filters (displayed in the UI).
    public var displayRoot: FileTreeNode?

    /// Expanded folder paths (stable across tree rebuilds).
    public var expansions: Set<String> = []

    /// Selected node path.
    public var selection: String?

    /// Git status map: absolute path -> status code ("M", "A", "D", "?", "R")
    public var gitStatus: [String: String] = [:]

    /// Flattened visible rows for virtualized rendering.
    /// Only includes rows whose ancestors are all expanded.
    public var flattenedRows: [FlatRow] {
        guard let root = displayRoot else { return [] }
        var rows: [FlatRow] = []
        rows.reserveCapacity(256)
        flattenChildren(of: root, depth: 0, into: &rows)
        return rows
    }

    // MARK: - Filter State

    public var filterText: String = "" {
        didSet { applyFilters() }
    }

    public var sortFoldersOnTop: Bool = true {
        didSet { triggerRebuild() }
    }

    public var sourceControlFilter: Bool = false {
        didSet { applyFilters() }
    }

    // MARK: - Project State

    public private(set) var projectRoot: String?

    /// The active file path (set by Emacs on buffer switch).
    public var activeFilePath: String?

    // MARK: - Callbacks

    public var onFileSelect: ((String) -> Void)?

    // MARK: - Initialization

    public init() {}

    // MARK: - Project Root Update

    /// Set the project root and rebuild the file tree from the filesystem.
    public func setProjectRoot(_ root: String) {
        guard root != projectRoot else { return }

        let normalized = (root as NSString).standardizingPath
        let home = NSHomeDirectory()
        if normalized == home || normalized == "/" {
            return
        }

        // Release old tree and invalidate builder cache before switching roots
        masterRoot = nil
        displayRoot = nil
        HyaloFileTreeBuilder.invalidateCache()

        projectRoot = root
        triggerRebuild()
    }

    /// Explicit async rebuild trigger — avoids `Task {}` in `didSet`.
    private func triggerRebuild() {
        Task { await rebuildFileTreeAsync() }
    }

    /// Rebuild the file tree from the current project root.
    public func rebuildFileTree() {
        guard let root = projectRoot else { return }

        gitStatus = HyaloFileTreeBuilder.gitStatusMap(root: root)

        guard let rootNode = HyaloFileTreeBuilder.buildFileTree(
            root: root,
            foldersOnTop: sortFoldersOnTop
        ) else {
            return
        }

        self.masterRoot = rootNode
        applyFilters()

        // Auto-expand root
        if let display = displayRoot {
            expansions.insert(display.id)
        }
    }

    /// Rebuild the file tree asynchronously (off main thread).
    public func rebuildFileTreeAsync() async {
        guard let root = projectRoot else { return }

        let foldersOnTop = sortFoldersOnTop
        async let treeTask = HyaloFileTreeBuilder.buildFileTreeAsync(
            root: root, foldersOnTop: foldersOnTop)
        let gitTask = await Task.detached(priority: .userInitiated) {
            HyaloFileTreeBuilder.gitStatusMap(root: root)
        }.value

        let rootNode = await treeTask
        gitStatus = gitTask

        guard let rootNode else { return }
        self.masterRoot = rootNode
        applyFilters()

        if let display = displayRoot {
            expansions.insert(display.id)
        }
    }

    /// Apply current filters to the master tree and update displayRoot.
    private func applyFilters() {
        guard let master = masterRoot else { return }

        if filterText.isEmpty && !sourceControlFilter {
            displayRoot = master
        } else if let filtered = filterNode(master) {
            displayRoot = filtered
            // Expand all when filtering to show results
            expandAll(node: filtered)
        } else {
            displayRoot = FileTreeNode(name: master.name, path: master.path, isDirectory: true, children: [])
        }
    }

    private func filterNode(_ node: FileTreeNode) -> FileTreeNode? {
        if !node.isDirectory {
            if !filterText.isEmpty && !node.name.localizedCaseInsensitiveContains(filterText) {
                return nil
            }
            if sourceControlFilter && gitStatus[node.path] == nil {
                return nil
            }
            return node
        }

        guard let children = node.children else { return nil }
        let filteredChildren = children.compactMap { filterNode($0) }

        if !filteredChildren.isEmpty {
            return FileTreeNode(name: node.name, path: node.path, isDirectory: true, children: filteredChildren)
        }
        return nil
    }

    private func expandAll(node: FileTreeNode) {
        if node.isDirectory {
            expansions.insert(node.id)
            node.children?.forEach { expandAll(node: $0) }
        }
    }

    // MARK: - Active File Tracking

    /// Set the active file and update selection to match.
    /// Expands all ancestor directories so the file is visible in the tree.
    public func setActiveFile(_ path: String) {
        activeFilePath = path

        guard let root = projectRoot, let display = displayRoot else { return }
        guard path.hasPrefix(root) else { return }

        // Find the node matching this path and expand ancestors
        if let found = findAndExpand(path: path, in: display) {
            selection = found.id
        }
    }

    /// Walk the tree to find the node at the given path, expanding ancestors.
    private func findAndExpand(path: String, in node: FileTreeNode) -> FileTreeNode? {
        if node.path == path { return node }

        guard node.isDirectory, let children = node.children else { return nil }

        for child in children {
            if let found = findAndExpand(path: path, in: child) {
                expansions.insert(node.id)
                return found
            }
        }
        return nil
    }

    // MARK: - Actions

    public func selectFile(_ path: String) {
        onFileSelect?(path)
    }

    // MARK: - Tree Flattening

    /// Appends visible children of `node` at `depth` into `rows`.
    /// Skips collapsed subtrees entirely, so cost is O(visible rows).
    private func flattenChildren(of node: FileTreeNode, depth: Int, into rows: inout [FlatRow]) {
        guard let children = node.children else { return }
        for child in children {
            rows.append(FlatRow(node: child, depth: depth))
            if child.isDirectory && expansions.contains(child.id) {
                flattenChildren(of: child, depth: depth + 1, into: &rows)
            }
        }
    }

    /// Toggle expansion state for a directory node.
    public func toggleExpansion(_ nodeId: String) {
        var t = Transaction()
        t.animation = nil
        withTransaction(t) {
            if expansions.contains(nodeId) {
                expansions.remove(nodeId)
            } else {
                expansions.insert(nodeId)
            }
        }
    }
}
