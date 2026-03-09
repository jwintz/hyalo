// ProjectNavigatorViewModel.swift - File tree state using native FileTreeNode
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Manages a FileTreeNode tree built from the filesystem.
// Provides filtering, active file tracking, and expansion state.

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
public final class ProjectNavigatorViewModel {
    // MARK: - File Tree State

    /// The root node of the file tree (unfiltered master copy).
    private var masterRoot: FileTreeNode?

    /// The root node after applying filters (displayed in the UI).
    public var displayRoot: FileTreeNode?

    /// Expanded folder IDs.
    public var expansions: Set<UUID> = []

    /// Selected node ID.
    public var selection: UUID?

    /// Git status map: absolute path -> status code ("M", "A", "D", "?", "R")
    public var gitStatus: [String: String] = [:]

    // MARK: - Filter State

    public var filterText: String = "" {
        didSet { applyFilters() }
    }

    public var sortFoldersOnTop: Bool = true {
        didSet { rebuildFileTree() }
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

        projectRoot = root
        rebuildFileTree()
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
}
