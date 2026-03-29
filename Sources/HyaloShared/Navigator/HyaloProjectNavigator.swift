// HyaloProjectNavigator.swift - Native file tree navigator
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Renders a collapsible file tree using a virtualized flat list.
// The tree is flattened to visible rows so List only materializes
// on-screen items, reducing WindowServer pressure for large projects.

import SwiftUI

@available(macOS 26.0, *)
public struct HyaloProjectNavigator: View {
    @Bindable public var viewModel: ProjectNavigatorViewModel
    public let onFileSelect: (String) -> Void

    fileprivate static let indentWidth: CGFloat = 16
    fileprivate static let chevronWidth: CGFloat = 16

    public var body: some View {
        List(selection: $viewModel.selection) {
            ForEach(viewModel.flattenedRows) { row in
                FlatRowView(row: row, viewModel: viewModel)
                    .tag(row.node.id)
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

// MARK: - Flat Row View (replaces recursive DisclosureGroup)

@available(macOS 26.0, *)
private struct FlatRowView: View {
    let row: FlatRow
    @Bindable var viewModel: ProjectNavigatorViewModel

    private var isExpanded: Bool {
        viewModel.expansions.contains(row.node.id)
    }

    var body: some View {
        HStack(spacing: 0) {
            // Indentation
            Spacer()
                .frame(width: CGFloat(row.depth) * HyaloProjectNavigator.indentWidth)

            if row.node.isDirectory {
                // Disclosure chevron
                Image(systemName: "chevron.right")
                    .font(.system(size: 10, weight: .semibold))
                    .foregroundStyle(.tertiary)
                    .rotationEffect(.degrees(isExpanded ? 90 : 0))
                    .frame(width: HyaloProjectNavigator.chevronWidth, height: 22)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        viewModel.toggleExpansion(row.node.id)
                    }

                Label {
                    Text(row.node.name)
                        .font(.system(size: HyaloDesign.FontSize.large))
                } icon: {
                    Image(systemName: "folder.fill")
                        .font(.system(size: HyaloDesign.IconSize.standard))
                        .foregroundStyle(.secondary)
                }
            } else {
                // Spacer matching chevron width for alignment
                Spacer()
                    .frame(width: HyaloProjectNavigator.chevronWidth)

                Label {
                    Text(row.node.name)
                        .font(.system(size: HyaloDesign.FontSize.large))
                } icon: {
                    Image(systemName: FileTreeIcons.icon(for: row.node.name))
                        .font(.system(size: HyaloDesign.IconSize.standard))
                        .foregroundStyle(.secondary)
                }
            }

            Spacer()
        }
        .contextMenu { pathContextMenu(path: row.node.path) }
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
