// SourceControlViewModel.swift - Source control navigator state
// Target: macOS 26 Tahoe with Liquid Glass design
// Split from NavigatorViewModel for granular updates (AUDIT.md #3)

import Foundation

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class SourceControlViewModel {
    // MARK: - State
    
    public var changedFiles: [GitChangedFile] = []
    public var commitHistory: [GitCommitEntry] = []
    
    // MARK: - Updates from Emacs
    
    public func updateChangedFiles(_ files: [GitChangedFile]) {
        changedFiles = files
    }
    
    public func updateCommitHistory(_ commits: [GitCommitEntry]) {
        commitHistory = commits
    }
}
