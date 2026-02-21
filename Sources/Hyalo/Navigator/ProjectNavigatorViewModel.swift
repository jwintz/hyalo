// ProjectNavigatorViewModel.swift - File tree state using ProjectNavigator package
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Manages the FileTree and FileNavigatorViewState from the ProjectNavigator package.
// Builds the tree from the filesystem when the project root changes.
// Provides filtering, git status, and active file tracking.

import Foundation
import System
import Files
import ProjectNavigator
import OrderedCollections

@available(macOS 26.0, *)
@MainActor
@Observable
final class ProjectNavigatorViewModel {
    // MARK: - File Tree State

    /// The FileNavigatorViewState from the ProjectNavigator package.
    /// Manages selection, expansions, and edited labels.
    var viewState: FileNavigatorViewState<HyaloFilePayload>

    /// Git status map: absolute path -> status code ("M", "A", "D", "?", "R")
    var gitStatus: [String: String] = [:]

    /// Aggregate git status for folders: folder UUID -> highest-priority status
    /// among all descendant files. Computed once per tree rebuild.
    var folderGitStatus: [UUID: String] = [:]

    // MARK: - Filter State

    var filterText: String = "" {
        didSet { applyFilters() }
    }

    var sortFoldersOnTop: Bool = true {
        didSet { rebuildFileTree() } // Re-scan to apply sort order if needed, or re-apply
    }

    var sourceControlFilter: Bool = false {
        didSet { applyFilters() }
    }

    // MARK: - Project State

    /// Current project root directory.
    private(set) var projectRoot: String?

    /// The master root item (unfiltered, non-proxy)
    private var masterRootItem: FullFileOrFolder<HyaloFilePayload>?

    /// The active file path (set by Emacs on buffer switch).
    var activeFilePath: String?

    // MARK: - Callbacks

    var onFileSelect: ((String) -> Void)?

    // MARK: - Initialization

    init() {
        self.viewState = FileNavigatorViewState<HyaloFilePayload>()
    }

    // MARK: - Project Root Update

    /// Set the project root and rebuild the file tree from the filesystem.
    /// Rejects the user home directory and filesystem root as project roots
    /// because scanning them recursively would freeze the UI.
    func setProjectRoot(_ root: String) {
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
    /// Also refreshes git status.
    func rebuildFileTree() {
        guard let root = projectRoot else { return }

        // Build git status map (single subprocess call)
        gitStatus = HyaloFileTreeBuilder.gitStatusMap(root: root)

        // Build file tree from filesystem root item
        guard let rootItem = HyaloFileTreeBuilder.buildFullFileTree(
            root: root,
            foldersOnTop: sortFoldersOnTop
        ) else {
            return
        }
        
        self.masterRootItem = rootItem
        applyFilters()
    }

    /// Apply current filters to the master tree and update viewState.
    private func applyFilters() {
        guard let masterRoot = masterRootItem else { return }
        
        let filteredRoot: FullFileOrFolder<HyaloFilePayload>
        if filterText.isEmpty && !sourceControlFilter {
            filteredRoot = masterRoot
        } else {
            if let filtered = filterItem(masterRoot) {
                filteredRoot = filtered
            } else {
                // If everything is filtered out, show an empty root folder
                filteredRoot = .folder(Folder(children: [:]))
            }
        }

        // Preserve expansions and selection across rebuilds
        let oldExpansions = viewState.expansions
        let oldSelection = viewState.selection

        viewState = FileNavigatorViewState<HyaloFilePayload>(
            fileTree: FileTree(files: filteredRoot),
            expansions: oldExpansions,
            selection: oldSelection
        )

        // Auto-expand root folder
        viewState.expansions[viewState.fileTree.root.id] = true
        
        // Expand folders when filtering to show results
        if !filterText.isEmpty || sourceControlFilter {
            expandAll(item: viewState.fileTree.root)
        }

        // Compute aggregate git status for folders
        folderGitStatus = computeFolderGitStatus(root: viewState.fileTree.root)
    }

    private func filterItem(_ item: FullFileOrFolder<HyaloFilePayload>) -> FullFileOrFolder<HyaloFilePayload>? {
        switch item {
        case .file(let file):
            let path = file.contents.path
            let name = (path as NSString).lastPathComponent
            
            // Text filter
            if !filterText.isEmpty && !name.localizedCaseInsensitiveContains(filterText) {
                return nil
            }
            
            // Source control filter
            if sourceControlFilter && gitStatus[path] == nil {
                return nil
            }
            
            return item
            
        case .folder(let folder):
            var filteredChildren = OrderedDictionary<String, FullFileOrFolder<HyaloFilePayload>>()
            
            for (name, child) in folder.children {
                if let filteredChild = filterItem(child) {
                    filteredChildren[name] = filteredChild
                }
            }
            
            // A folder matches if its children match.
            if !filteredChildren.isEmpty {
                return .folder(Folder(children: filteredChildren, persistentID: folder.id))
            }
            
            return nil
        }
    }

    private func expandAll(item: ProxyFileOrFolder<HyaloFilePayload>) {
        if case .folder(let folder) = item {
            viewState.expansions[folder.id] = true
            for (_, child) in folder.children {
                expandAll(item: child)
            }
        }
    }

    // MARK: - Active File Tracking

    /// Set the active file and update selection to match.
    /// Expands all ancestor directories so the file is visible in the tree.
    func setActiveFile(_ path: String) {
        let oldActive = activeFilePath
        let oldSelection = viewState.selection
        activeFilePath = path

        guard let root = projectRoot else { return }
        guard path.hasPrefix(root) else { return }
        var relative = String(path.dropFirst(root.count))
        if relative.hasPrefix("/") {
            relative = String(relative.dropFirst())
        }
        guard !relative.isEmpty else { return }

        // Walk the tree from root, expanding each intermediate folder
        let filePath = System.FilePath(relative)
        expandAncestors(for: filePath)

        // Find and select the file UUID
        if let item = viewState.fileTree.lookup(filePath: filePath) {
            viewState.selection = item.id
        }
    }

    /// Expand all ancestor folders along the path so the target is visible.
    private func expandAncestors(for filePath: System.FilePath) {
        var components = Array(filePath.components)
        guard !components.isEmpty else { return }

        // Remove the last component (the file itself)
        components.removeLast()

        // Walk from root, expanding each folder
        // Always expand root
        if case .folder(let rootFolder) = viewState.fileTree.root {
            viewState.expansions[rootFolder.id] = true
        }

        var currentPath = System.FilePath()
        for component in components {
            currentPath.append(String(decoding: component))
            if let item = viewState.fileTree.lookup(filePath: currentPath),
               case .folder(let folder) = item {
                viewState.expansions[folder.id] = true
            }
        }
    }

    // MARK: - Actions
    // Swift does NOT modify local state.  Emacs is the single source of truth.

    func selectFile(_ path: String) {
        onFileSelect?(path)
    }

    // MARK: - Folder Git Status Aggregation

    /// Status priority: M > D > R > A > ? (most attention-worthy wins).
    private static let statusPriority: [String: Int] = [
        "M": 5, "D": 4, "R": 3, "A": 2, "?": 1
    ]

    /// Walk the proxy tree and compute aggregate git status per folder.
    /// Returns a mapping from folder UUID to the highest-priority status
    /// found among all descendant files.
    private func computeFolderGitStatus(
        root: ProxyFileOrFolder<HyaloFilePayload>
    ) -> [UUID: String] {
        var result: [UUID: String] = [:]
        _ = aggregateStatus(item: root, into: &result)
        return result
    }

    /// Recursively compute the highest-priority git status in a subtree.
    /// Returns the winning status string, or nil if no changes.
    @discardableResult
    private func aggregateStatus(
        item: ProxyFileOrFolder<HyaloFilePayload>,
        into result: inout [UUID: String]
    ) -> String? {
        switch item {
        case .file(let proxy):
            guard let file = proxy.file else { return nil }
            return gitStatus[file.contents.path]

        case .folder(let folder):
            var best: String? = nil
            var bestPri = 0

            for (_, child) in folder.children {
                if let childStatus = aggregateStatus(item: child, into: &result) {
                    let pri = Self.statusPriority[childStatus] ?? 0
                    if pri > bestPri {
                        bestPri = pri
                        best = childStatus
                    }
                }
            }

            if let best {
                result[folder.id] = best
            }
            return best
        }
    }
}
