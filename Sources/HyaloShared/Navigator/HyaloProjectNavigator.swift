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
                    ProjectNodeView(node: child, viewModel: viewModel)
                }
            }
        }
        .listStyle(.sidebar)
        .scrollIndicators(.never)
        .environment(\.defaultMinListRowHeight, 22)
        .onChange(of: viewModel.selection) { _, newValue in
            guard let selectedPath = newValue else { return }
            if let root = viewModel.displayRoot, let node = findNode(id: selectedPath, in: root) {
                if !node.isDirectory {
                    onFileSelect(node.path)
                }
            }
        }
    }

    // MARK: - Tree Search

    private func findNode(id: String, in node: FileTreeNode) -> FileTreeNode? {
        if node.id == id { return node }
        guard let children = node.children else { return nil }
        for child in children {
            if let found = findNode(id: id, in: child) { return found }
        }
        return nil
    }
}

// MARK: - Node View (concrete type, enables structural diffing)

@available(macOS 26.0, *)
private struct ProjectNodeView: View {
    let node: FileTreeNode
    @Bindable var viewModel: ProjectNavigatorViewModel

    var body: some View {
        if node.isDirectory {
            DisclosureGroup(isExpanded: expansionBinding) {
                ForEach(node.children ?? [], id: \.id) { child in
                    ProjectNodeView(node: child, viewModel: viewModel)
                }
            } label: {
                folderLabel
            }
            .tag(node.id)
        } else {
            fileLabel
                .tag(node.id)
        }
    }

    private var expansionBinding: Binding<Bool> {
        Binding<Bool>(
            get: { viewModel.expansions.contains(node.id) },
            set: { newValue in
                var t = Transaction()
                t.animation = nil
                withTransaction(t) {
                    if newValue {
                        viewModel.expansions.insert(node.id)
                    } else {
                        viewModel.expansions.remove(node.id)
                    }
                }
            }
        )
    }

    @ViewBuilder
    private var fileLabel: some View {
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

    @ViewBuilder
    private var folderLabel: some View {
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
}
