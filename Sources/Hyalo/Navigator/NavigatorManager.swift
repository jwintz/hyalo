// NavigatorManager.swift - Manager for navigator sidebar state and Emacs integration
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d's NavigatorManager

import Foundation

// MARK: - Data Models

struct BufferInfo: Codable, Identifiable, Hashable {
    let id: String
    let name: String
    let path: String?
    let modified: Bool
    let icon: String?
}

struct SearchResult: Codable, Identifiable, Hashable {
    let id: String
    let file: String
    let line: Int
    let column: Int
    let text: String
    let matchRange: String?
}

// MARK: - Source Control Models

struct GitChangedFile: Codable, Identifiable, Hashable {
    let id: String
    let fileName: String
    let filePath: String
    let status: String      // "M", "A", "D", "?", "R"
    let isStaged: Bool
}

struct GitCommitEntry: Codable, Identifiable, Hashable {
    let id: String
    let hash: String
    let fullHash: String
    let message: String
    let author: String
    let authorEmail: String
    let date: String
    let refs: [String]
    let tag: String
}

// MARK: - Find Status

enum FindStatus {
    case none
    case searching
    case found
    case noResults
}

// MARK: - Navigator View Model (Legacy — transitioning to focused view models)

@available(macOS 26.0, *)
@MainActor
@Observable
final class NavigatorViewModel {
    var selectedTab: NavigatorTab? = .project
    var tabItems: [NavigatorTab] = NavigatorTab.allCases

    // DEPRECATED: Use BufferListViewModel instead
    var bufferFilter: String = ""
    var bufferList: [BufferInfo] = []
    var selectedBuffer: String?
    var activeBuffer: String?

    // DEPRECATED: Use SearchViewModel instead
    var searchQuery: String = ""
    var searchResults: [SearchResult] = []
    var findStatus: FindStatus = .none
    var searchResultCount: Int = 0
    var searchFileCount: Int = 0

    // DEPRECATED: Use SourceControlViewModel instead
    var changedFiles: [GitChangedFile] = []
    var commitHistory: [GitCommitEntry] = []
}

// MARK: - Navigator Manager

@available(macOS 26.0, *)
@MainActor
final class NavigatorManager {
    static let shared = NavigatorManager()

    // Legacy view model (transitioning to focused view models)
    let viewModel = NavigatorViewModel()

    // Focused view models
    let projectNavigatorViewModel = ProjectNavigatorViewModel()
    let bufferListViewModel = BufferListViewModel()
    let searchViewModel = SearchViewModel()
    let sourceControlViewModel = SourceControlViewModel()

    // Callbacks to Emacs (set by channel)
    var onBufferSelect: ((String) -> Void)?
    var onBufferClose: ((String) -> Void)?
    var onBufferSave: ((String) -> Void)?
    var onFileSelect: ((String) -> Void)?
    var onSearchExecute: ((String) -> Void)?
    var onSearchResultSelect: ((String, Int, Int) -> Void)?
    var onCommitSelect: ((String) -> Void)?
    var onChangedFileSelect: ((String) -> Void)?

    private init() {
        // Wire up focused view model callbacks
        projectNavigatorViewModel.onFileSelect = { [weak self] path in
            // Notify editor tab view model to set pending tab for stale callback detection
            if let wc = HyaloModule.activeController {
                let bufferName = (path as NSString).lastPathComponent
                wc.editorTabViewModel.selectFile(path, bufferName: bufferName)
            }
            self?.onFileSelect?(path)
            HyaloModule.wakeEmacs()
        }
        bufferListViewModel.onBufferSelect = { [weak self] name in
            // Notify editor tab view model to set pending tab for stale callback detection
            if let wc = HyaloModule.activeController {
                wc.editorTabViewModel.selectFile(name, bufferName: name)
            }
            self?.onBufferSelect?(name)
            HyaloModule.wakeEmacs()
        }
        bufferListViewModel.onBufferClose = { [weak self] name in
            self?.onBufferClose?(name)
        }
        bufferListViewModel.onBufferSave = { [weak self] name in
            self?.onBufferSave?(name)
        }
        searchViewModel.onSearchExecute = { [weak self] query in
            self?.onSearchExecute?(query)
        }
        searchViewModel.onSearchResultSelect = { [weak self] file, line, col in
            self?.onSearchResultSelect?(file, line, col)
        }
    }

    // MARK: - Updates from Emacs

    func updateBufferList(_ buffers: [BufferInfo]) {
        // Update both legacy and focused view models during transition
        let incoming = Dictionary(uniqueKeysWithValues: buffers.map { ($0.id, $0) })
        var result = viewModel.bufferList.compactMap { existing -> BufferInfo? in
            incoming[existing.id]
        }
        let kept = Set(result.map(\.id))
        for buffer in buffers where !kept.contains(buffer.id) {
            result.append(buffer)
        }
        viewModel.bufferList = result

        // Update focused view model
        bufferListViewModel.updateBuffers(buffers)
    }

    /// Set the project root directory and rebuild the file tree in Swift.
    /// Called from Emacs when the project root changes.
    func setProjectRoot(_ root: String) {
        projectNavigatorViewModel.setProjectRoot(root)
    }

    /// Refresh the file tree from the current project root.
    /// Called from Emacs on navigator-refresh.
    func refreshFileTree() {
        projectNavigatorViewModel.rebuildFileTree()
    }

    func updateSearchResults(_ results: [SearchResult]) {
        NSLog("[Hyalo:Search] updateSearchResults: %d results", results.count)
        viewModel.searchResults = results
        searchViewModel.results = results
    }

    func setActiveBuffer(_ name: String) {
        viewModel.selectedBuffer = name
        bufferListViewModel.setActiveBuffer(name)
    }

    func setActiveFile(_ path: String) {
        projectNavigatorViewModel.setActiveFile(path)
    }

    // MARK: - Actions to Emacs

    func selectBuffer(_ bufferName: String) {
        onBufferSelect?(bufferName)
    }

    func closeBuffer(_ bufferName: String) {
        onBufferClose?(bufferName)
    }

    func selectFile(_ filePath: String) {
        onFileSelect?(filePath)
    }

    func executeSearch(_ query: String) {
        NSLog("[Hyalo:Search] executeSearch: query=%@", query)
        viewModel.findStatus = .searching
        searchViewModel.status = .searching
        onSearchExecute?(query)
    }

    func saveBuffer(_ bufferName: String) {
        onBufferSave?(bufferName)
    }

    // MARK: - Source Control Updates

    func updateChangedFiles(_ files: [GitChangedFile]) {
        viewModel.changedFiles = files
        sourceControlViewModel.updateChangedFiles(files)
    }

    func updateCommitHistory(_ commits: [GitCommitEntry]) {
        viewModel.commitHistory = commits
        sourceControlViewModel.updateCommitHistory(commits)
    }

    func updateSearchStatus(resultCount: Int, fileCount: Int) {
        NSLog("[Hyalo:Search] updateSearchStatus: %d results in %d files", resultCount, fileCount)
        viewModel.searchResultCount = resultCount
        viewModel.searchFileCount = fileCount
        viewModel.findStatus = resultCount > 0 ? .found : .noResults
        searchViewModel.updateResults(
            searchViewModel.results,
            resultCount: resultCount,
            fileCount: fileCount
        )
    }
}
