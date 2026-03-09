// InspectorManager.swift - Shared inspector state singleton
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d's InspectorManager

import Foundation

// MARK: - File Info

@available(macOS 26.0, *)
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

// MARK: - Inspector View Model

@available(macOS 26.0, *)
@MainActor
@Observable
public final class InspectorViewModel {
    public init() {}
    public var selectedTab: InspectorTab? = .file
    public var tabItems: [InspectorTab] = [.file, .settings]
    public var fileInfo: FileInfo = .empty
}

// MARK: - Inspector Manager

@available(macOS 26.0, *)
@MainActor
public final class InspectorManager {
    public static let shared = InspectorManager()

    public let viewModel = InspectorViewModel()

    private init() {}

    public func updateFileInfo(from data: Data) {
        do {
            let info = try JSONDecoder().decode(FileInfo.self, from: data)
            viewModel.fileInfo = info
        } catch {
            NSLog("[Hyalo] updateFileInfo decode error: \(error)")
        }
    }
}
