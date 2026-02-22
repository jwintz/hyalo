// NavigatorManager.swift - Manager for navigator sidebar state and Emacs integration
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d's NavigatorManager

import Foundation

// MARK: - Data Models

public struct BufferInfo: Codable, Identifiable, Hashable {
    public let id: String
    public let name: String
    public let path: String?
    public let modified: Bool
    public let icon: String?
}

public struct SearchResult: Codable, Identifiable, Hashable {
    public let id: String
    public let file: String
    public let line: Int
    public let column: Int
    public let text: String
    public let matchRange: String?
}

// MARK: - Source Control Models

public struct GitChangedFile: Codable, Identifiable, Hashable {
    public let id: String
    public let fileName: String
    public let filePath: String
    public let status: String      // "M", "A", "D", "?", "R"
    public let isStaged: Bool
}

public struct GitCommitEntry: Codable, Identifiable, Hashable {
    public let id: String
    public let hash: String
    public let fullHash: String
    public let message: String
    public let author: String
    public let authorEmail: String
    public let date: String
    public let refs: [String]
    public let tag: String
}

// MARK: - Find Status

public enum FindStatus {
    case none
    case searching
    case found
    case noResults
}

// MARK: - Navigator View Model (Legacy — transitioning to focused view models)

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class NavigatorViewModel {
    public var selectedTab: NavigatorTab? = .project
    public var tabItems: [NavigatorTab] = NavigatorTab.allCases

    // DEPRECATED: Use BufferListViewModel instead
    public var bufferFilter: String = ""
    public var bufferList: [BufferInfo] = []
    public var selectedBuffer: String?
    public var activeBuffer: String?

    // DEPRECATED: Use SearchViewModel instead
    public var searchQuery: String = ""
    public var searchResults: [SearchResult] = []
    public var findStatus: FindStatus = .none
    public var searchResultCount: Int = 0
    public var searchFileCount: Int = 0

    // DEPRECATED: Use SourceControlViewModel instead
    public var changedFiles: [GitChangedFile] = []
    public var commitHistory: [GitCommitEntry] = []

    public init() {}
}

// MARK: - Navigator Manager

@available(macOS 26.0, iOS 26.0, *)
@MainActor
public final class NavigatorManager {
    public static let shared = NavigatorManager()

    // Legacy view model (transitioning to focused view models)
    public let viewModel = NavigatorViewModel()

    // Focused view models
    public let projectNavigatorViewModel = ProjectNavigatorViewModel()
    public let bufferListViewModel = BufferListViewModel()
    public let searchViewModel = SearchViewModel()
    public let sourceControlViewModel = SourceControlViewModel()

    // Callbacks to Emacs (set by channel)
    public var onBufferSelect: ((String) -> Void)?
    public var onBufferClose: ((String) -> Void)?
    public var onBufferSave: ((String) -> Void)?
    public var onFileSelect: ((String) -> Void)?
    public var onSearchExecute: ((String) -> Void)?
    public var onSearchResultSelect: ((String, Int, Int) -> Void)?
    public var onCommitSelect: ((String) -> Void)?
    public var onChangedFileSelect: ((String) -> Void)?

    private init() {
        // Wire up focused view model callbacks
        projectNavigatorViewModel.onFileSelect = { [weak self] path in
            self?.onFileSelect?(path)
            wakeEmacs()
        }
        bufferListViewModel.onBufferSelect = { [weak self] name in
            self?.onBufferSelect?(name)
            wakeEmacs()
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

    public func updateBufferList(_ buffers: [BufferInfo]) {
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
    public func setProjectRoot(_ root: String) {
        projectNavigatorViewModel.setProjectRoot(root)
    }

    /// Refresh the file tree from the current project root.
    /// Called from Emacs on navigator-refresh.
    public func refreshFileTree() {
        projectNavigatorViewModel.rebuildFileTree()
    }

    public func updateSearchResults(_ results: [SearchResult]) {
        viewModel.searchResults = results
        searchViewModel.results = results
    }

    public func setActiveBuffer(_ name: String) {
        viewModel.selectedBuffer = name
        bufferListViewModel.setActiveBuffer(name)
    }

    public func setActiveFile(_ path: String) {
        projectNavigatorViewModel.setActiveFile(path)
    }

    // MARK: - Actions to Emacs

    public func selectBuffer(_ bufferName: String) {
        onBufferSelect?(bufferName)
    }

    public func closeBuffer(_ bufferName: String) {
        onBufferClose?(bufferName)
    }

    public func selectFile(_ filePath: String) {
        onFileSelect?(filePath)
    }

    public func executeSearch(_ query: String) {
        viewModel.findStatus = .searching
        searchViewModel.status = .searching
        onSearchExecute?(query)
    }

    public func saveBuffer(_ bufferName: String) {
        onBufferSave?(bufferName)
    }

    // MARK: - Source Control Updates

    public func updateChangedFiles(_ files: [GitChangedFile]) {
        viewModel.changedFiles = files
        sourceControlViewModel.updateChangedFiles(files)
    }

    public func updateCommitHistory(_ commits: [GitCommitEntry]) {
        viewModel.commitHistory = commits
        sourceControlViewModel.updateCommitHistory(commits)
    }

    public func updateSearchStatus(resultCount: Int, fileCount: Int) {
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
