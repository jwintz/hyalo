// SearchViewModel.swift - Find navigator state with fuzzy matching
// Target: macOS 26 Tahoe with Liquid Glass design
// Split from NavigatorViewModel for granular updates (AUDIT.md #3)

import Foundation

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class SearchViewModel {
    // MARK: - State
    
    public var query: String = "" {
        didSet {
            if query.isEmpty {
                status = .none
                results = []
            }
        }
    }
    
    public var results: [SearchResult] = []
    public var status: FindStatus = .none
    public var resultCount: Int = 0
    public var fileCount: Int = 0
    
    /// Results grouped by file (moved from FindNavigatorView per AUDIT H4)
    public var groupedResults: [(file: String, results: [SearchResult])] {
        let dict = Dictionary(grouping: results, by: \.file)
        return dict.map { (file: $0.key, results: $0.value) }
            .sorted { $0.file < $1.file }
    }

    // MARK: - Dependencies
    
    public var onSearchExecute: ((String) -> Void)?
    public var onSearchResultSelect: ((String, Int, Int) -> Void)?
    
    // MARK: - Actions to Emacs
    
    public func executeSearch() {
        guard !query.isEmpty else { return }
        status = .searching
        onSearchExecute?(query)
    }
    
    public func selectResult(_ result: SearchResult) {
        onSearchResultSelect?(result.file, result.line, result.column)
    }
    
    // MARK: - Updates from Emacs
    
    public func updateResults(_ newResults: [SearchResult], resultCount: Int, fileCount: Int) {
        self.results = newResults
        self.resultCount = resultCount
        self.fileCount = fileCount
        self.status = resultCount > 0 ? .found : .noResults
    }
}
