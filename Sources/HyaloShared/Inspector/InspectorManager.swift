// InspectorManager.swift - Shared inspector state singleton
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d's InspectorManager

import Foundation

// MARK: - File Info

@available(macOS 26.0, iOS 26.0, *)
public struct FileInfo: Codable {
    public var name: String
    public var type: String
    public var path: String
    public var size: String
    public var created: String
    public var modified: String
    public var permissions: String
    public var encoding: String
    public var lineEndings: String
    public var indentStyle: String
    public var indentWidth: Int
    public var gitStatus: String
    public var lastCommit: String?

    public static var empty: FileInfo {
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

@available(macOS 26.0, iOS 26.0, *)
public struct Commit: Identifiable, Codable, Hashable {
    public var id: String { hash }
    public var hash: String
    public var fullHash: String?
    public var message: String
    public var author: String
    public var authorEmail: String?
    public var date: String
    public var refs: [String]?
    public var tag: String?
}

// MARK: - Inspector View Model

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class InspectorViewModel {
    public init() {}
    public var selectedTab: InspectorTab? = .file
    public var tabItems: [InspectorTab] = [.file, .history, .appearance]
    public var fileInfo: FileInfo = .empty
    public var commits: [Commit] = []
}

// MARK: - Inspector Manager

@available(macOS 26.0, iOS 26.0, *)
@MainActor
public final class InspectorManager {
    public static let shared = InspectorManager()

    public let viewModel = InspectorViewModel()

    // Callbacks to Emacs (set by channel)
    public var onCommitSelect: ((String) -> Void)?

    private init() {}

    public func updateFileInfo(from data: Data) {
        do {
            let info = try JSONDecoder().decode(FileInfo.self, from: data)
            viewModel.fileInfo = info
        } catch {
            NSLog("[Hyalo] updateFileInfo decode error: \(error)")
        }
    }

    public func updateGitHistory(from data: Data) {
        do {
            let commits = try JSONDecoder().decode([Commit].self, from: data)
            viewModel.commits = commits
        } catch {
            NSLog("[Hyalo] updateGitHistory decode error: \(error)")
            viewModel.commits = []
        }
    }
}
