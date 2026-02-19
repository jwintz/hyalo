// InspectorManager.swift - Shared inspector state singleton
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d's InspectorManager

import Foundation

// MARK: - File Info

@available(macOS 26.0, *)
struct FileInfo: Codable {
    var name: String
    var type: String
    var path: String
    var size: String
    var created: String
    var modified: String
    var permissions: String
    var encoding: String
    var lineEndings: String
    var indentStyle: String
    var indentWidth: Int
    var gitStatus: String
    var lastCommit: String?

    static var empty: FileInfo {
        FileInfo(
            name: "—", type: "—", path: "—", size: "—",
            created: "—", modified: "—", permissions: "—",
            encoding: "—", lineEndings: "—",
            indentStyle: "Spaces", indentWidth: 4,
            gitStatus: "—", lastCommit: nil
        )
    }
}

// MARK: - Commit

@available(macOS 26.0, *)
struct Commit: Identifiable, Codable, Hashable {
    var id: String { hash }
    var hash: String
    var fullHash: String?
    var message: String
    var author: String
    var authorEmail: String?
    var date: String
    var refs: [String]?
    var tag: String?
}

// MARK: - Inspector View Model

@available(macOS 26.0, *)
@MainActor
@Observable
final class InspectorViewModel {
    var selectedTab: InspectorTab? = .file
    var tabItems: [InspectorTab] = [.file, .history, .appearance]
    var fileInfo: FileInfo = .empty
    var commits: [Commit] = []
}

// MARK: - Inspector Manager

@available(macOS 26.0, *)
@MainActor
final class InspectorManager {
    static let shared = InspectorManager()

    let viewModel = InspectorViewModel()

    // Callbacks to Emacs (set by channel)
    var onCommitSelect: ((String) -> Void)?

    private init() {}

    func updateFileInfo(from data: Data) {
        do {
            let info = try JSONDecoder().decode(FileInfo.self, from: data)
            viewModel.fileInfo = info
        } catch {
            NSLog("[Hyalo] updateFileInfo decode error: %@", String(describing: error))
        }
    }

    func updateGitHistory(from data: Data) {
        do {
            let commits = try JSONDecoder().decode([Commit].self, from: data)
            viewModel.commits = commits
        } catch {
            NSLog("[Hyalo] updateGitHistory decode error: %@", String(describing: error))
            viewModel.commits = []
        }
    }
}
