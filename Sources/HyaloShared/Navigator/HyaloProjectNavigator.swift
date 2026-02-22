// HyaloProjectNavigator.swift - File tree navigator using ProjectNavigator package
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Wraps FileNavigator from mchakravarty/ProjectNavigator with Hyalo-specific
// labels (SF Symbol file icons, git status badges) and Emacs integration.

import SwiftUI
import Files
import ProjectNavigator

@available(macOS 26.0, iOS 26.0, *)
public struct HyaloProjectNavigator: View {
    @Bindable public var viewState: FileNavigatorViewState<HyaloFilePayload>
    public let gitStatus: [String: String]
    public let folderGitStatus: [UUID: String]
    public let onFileSelect: (String) -> Void
    public var filter: (String) -> Bool = { _ in true }

    public var body: some View {
        List(selection: $viewState.selection) {
            FileNavigator(
                name: nil as String?,
                item: .constant(viewState.fileTree.root),
                parent: .constant(nil),
                viewState: viewState,
                fileLabel: { cursor, $editedText, proxy in
                    fileLabel(cursor: cursor, proxy: proxy)
                },
                folderLabel: { cursor, $editedText, $folder in
                    folderLabel(cursor: cursor, folder: folder)
                }
            )
        }
        .listStyle(.sidebar)
        .scrollIndicators(.never)
        .environment(\.defaultMinListRowHeight, 22)
        .navigatorFilter(filter)
        .onChange(of: viewState.selection) { oldValue, newValue in
            guard let uuid = newValue else { return }
            if let proxy = viewState.fileTree.proxy(for: uuid).file {
                let path = proxy.contents.path
                onFileSelect(path)
            }
        }
    }

    // MARK: - File Label

    @ViewBuilder
    private func fileLabel(
        cursor: FileNavigatorCursor<HyaloFilePayload>,
        proxy: File<HyaloFilePayload>.Proxy
    ) -> some View {
        HStack(spacing: 0) {
            Label {
                Text(cursor.name)
                    .font(.system(size: HyaloDesign.FontSize.large))
            } icon: {
                Image(systemName: FileTreeIcons.icon(for: cursor.name))
                    .font(.system(size: HyaloDesign.IconSize.standard))
                    .foregroundStyle(.secondary)
            }
            Spacer()
            if let file = proxy.file, let status = gitStatus[file.contents.path] {
                GitStatusBadge(status: status)
            }
        }
        .contextMenu { pathContextMenu(path: proxy.file?.contents.path) }
    }

    // MARK: - Folder Label

    @ViewBuilder
    private func folderLabel(
        cursor: FileNavigatorCursor<HyaloFilePayload>,
        folder: ProxyFolder<HyaloFilePayload>
    ) -> some View {
        let isExpanded = viewState.expansions[folder.id] == true

        HStack(spacing: 0) {
            Label {
                Text(cursor.name)
                    .font(.system(size: HyaloDesign.FontSize.large))
            } icon: {
                Image(systemName: "folder.fill")
                    .font(.system(size: HyaloDesign.IconSize.standard))
                    .foregroundStyle(.secondary)
            }
            Spacer()
            if !isExpanded, let status = folderGitStatus[folder.id] {
                GitStatusBadge(status: status)
            }
        }
    }

    // MARK: - Context Menu

    @ViewBuilder
    private func pathContextMenu(path: String?) -> some View {
        if let path {
            Button("Reveal in Finder") {
                platformRevealInFinder([path])
            }
            Divider()
            Button("Copy Path") {
                platformCopyToClipboard(path)
            }
        }
    }
}
