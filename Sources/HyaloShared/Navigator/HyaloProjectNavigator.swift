// HyaloProjectNavigator.swift - Native file tree navigator
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Renders a collapsible file tree using native SwiftUI List with
// recursive DisclosureGroups. No external dependencies.

import SwiftUI

@available(macOS 26.0, *)
public struct HyaloProjectNavigator: View {
    @Bindable public var viewModel: ProjectNavigatorViewModel
    public let onFileSelect: (String) -> Void

    public var body: some View {
        List(selection: $viewModel.selection) {
            if let root = viewModel.displayRoot {
                ForEach(root.children ?? [], id: \.id) { child in
                    nodeView(child)
                }
            }
        }
        .listStyle(.sidebar)
        .scrollIndicators(.never)
        .environment(\.defaultMinListRowHeight, 22)
        .onChange(of: viewModel.selection) { _, newValue in
            guard let uuid = newValue else { return }
            if let root = viewModel.displayRoot, let node = findNode(id: uuid, in: root) {
                if !node.isDirectory {
                    onFileSelect(node.path)
                }
            }
        }
    }

    // MARK: - Node View

    private func nodeView(_ node: FileTreeNode) -> AnyView {
        if node.isDirectory {
            let isExpanded = Binding<Bool>(
                get: { viewModel.expansions.contains(node.id) },
                set: { newValue in
                    if newValue {
                        viewModel.expansions.insert(node.id)
                    } else {
                        viewModel.expansions.remove(node.id)
                    }
                }
            )
            return AnyView(
                DisclosureGroup(isExpanded: isExpanded) {
                    ForEach(node.children ?? [], id: \.id) { child in
                        nodeView(child)
                    }
                } label: {
                    folderLabel(node)
                }
                .tag(node.id)
            )
        } else {
            return AnyView(
                fileLabel(node)
                    .tag(node.id)
            )
        }
    }

    // MARK: - File Label

    @ViewBuilder
    private func fileLabel(_ node: FileTreeNode) -> some View {
        HStack(spacing: 0) {
            Label {
                Text(node.name)
                    .font(.system(size: HyaloDesign.FontSize.large))
            } icon: {
                Image(systemName: FileTreeIcons.icon(for: node.name))
                    .font(.system(size: HyaloDesign.IconSize.standard))
                    .foregroundStyle(.secondary)
            }
            Spacer()
        }
        .contextMenu { pathContextMenu(path: node.path) }
    }

    // MARK: - Folder Label

    @ViewBuilder
    private func folderLabel(_ node: FileTreeNode) -> some View {
        HStack(spacing: 0) {
            Label {
                Text(node.name)
                    .font(.system(size: HyaloDesign.FontSize.large))
            } icon: {
                Image(systemName: "folder.fill")
                    .font(.system(size: HyaloDesign.IconSize.standard))
                    .foregroundStyle(.secondary)
            }
            Spacer()
        }
    }

    // MARK: - Context Menu

    @ViewBuilder
    private func pathContextMenu(path: String) -> some View {
        Button("Reveal in Finder") {
            platformRevealInFinder([path])
        }
        Divider()
        Button("Copy Path") {
            platformCopyToClipboard(path)
        }
    }

    // MARK: - Tree Search

    private func findNode(id: UUID, in node: FileTreeNode) -> FileTreeNode? {
        if node.id == id { return node }
        guard let children = node.children else { return nil }
        for child in children {
            if let found = findNode(id: id, in: child) { return found }
        }
        return nil
    }
}
