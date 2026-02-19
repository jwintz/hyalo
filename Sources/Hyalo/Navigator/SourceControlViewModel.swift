// SourceControlViewModel.swift - Source control navigator state
// Target: macOS 26 Tahoe with Liquid Glass design
// Split from NavigatorViewModel for granular updates (AUDIT.md #3)

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
final class SourceControlViewModel {
    // MARK: - State
    
    var changedFiles: [GitChangedFile] = []
    var commitHistory: [GitCommitEntry] = []
    
    // MARK: - Updates from Emacs
    
    func updateChangedFiles(_ files: [GitChangedFile]) {
        changedFiles = files
    }
    
    func updateCommitHistory(_ commits: [GitCommitEntry]) {
        commitHistory = commits
    }
}
