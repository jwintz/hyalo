// SearchViewModel.swift - Find navigator state with fuzzy matching
// Target: macOS 26 Tahoe with Liquid Glass design
// Split from NavigatorViewModel for granular updates (AUDIT.md #3)

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
final class SearchViewModel {
    // MARK: - State
    
    var query: String = "" {
        didSet {
            if query.isEmpty {
                status = .none
                results = []
            }
        }
    }
    
    var results: [SearchResult] = []
    var status: FindStatus = .none
    var resultCount: Int = 0
    var fileCount: Int = 0
    
    // MARK: - Dependencies
    
    var onSearchExecute: ((String) -> Void)?
    var onSearchResultSelect: ((String, Int, Int) -> Void)?
    
    // MARK: - Actions to Emacs
    
    func executeSearch() {
        guard !query.isEmpty else { return }
        status = .searching
        onSearchExecute?(query)
    }
    
    func selectResult(_ result: SearchResult) {
        onSearchResultSelect?(result.file, result.line, result.column)
    }
    
    // MARK: - Updates from Emacs
    
    func updateResults(_ newResults: [SearchResult], resultCount: Int, fileCount: Int) {
        self.results = newResults
        self.resultCount = resultCount
        self.fileCount = fileCount
        self.status = resultCount > 0 ? .found : .noResults
    }
}
