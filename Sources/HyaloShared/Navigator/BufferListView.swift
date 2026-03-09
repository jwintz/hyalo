// BufferListView.swift - Emacs buffer list view
// Target: macOS 26 Tahoe with Liquid Glass design
// Matches ProjectNavigatorView aesthetics: Label pattern, native selection,
// sidebar list style, same icon sizing and font tokens.

import SwiftUI

@available(macOS 26.0, *)
public struct BufferListView: View {
    @Environment(\.bufferListViewModel) private var envVM
    @State private var selection: String?
    @State private var hoveredBufferId: String?
    @Environment(\.colorTheme) private var theme

    private var viewModel: BufferListViewModel {
        envVM ?? NavigatorManager.shared.bufferListViewModel
    }

    public var body: some View {
        VStack(spacing: 0) {
            if viewModel.filteredBuffers.isEmpty {
                HyaloContentUnavailableView(
                    "No Buffers",
                    description: viewModel.filter.isEmpty
                        ? nil
                        : "No buffers matching \"\(viewModel.filter)\"",
                    systemImage: "doc.on.doc"
                )
            } else {
                List(viewModel.filteredBuffers, selection: $selection) { buffer in
                    bufferRow(buffer)
                        .tag(buffer.id)
                }
                .listStyle(.sidebar)
                .scrollIndicators(.never)
                .environment(\.defaultMinListRowHeight, 22)
                .animation(nil, value: viewModel.filteredBuffers)
            }
        }
        .onChange(of: selection) { _, newValue in
            guard let name = newValue else { return }
            viewModel.selectBuffer(name)
        }
        .onChange(of: viewModel.activeBuffer) { _, newValue in
            selection = newValue
        }
        .safeAreaInset(edge: .bottom, spacing: 0) {
            HStack(spacing: 5) {
                HyaloPaneTextField(
                    "Filter",
                    text: Bindable(viewModel).filter,
                    leadingAccessories: {
                        Image(systemName: "magnifyingglass")
                            .font(.system(size: 11))
                            .foregroundStyle(.secondary)
                            .padding(.leading, 8)
                            .frame(width: 26, height: 20)
                    },
                    clearable: true,
                    style: .plain
                )
            }
            .frame(height: 28, alignment: .center)
            .frame(maxWidth: .infinity)
            .overlay(alignment: .top) { Divider() }
        }
    }

    // MARK: - Buffer Row

    @ViewBuilder
    private func bufferRow(_ buffer: BufferInfo) -> some View {
        let isHovered = hoveredBufferId == buffer.id

        HStack(spacing: 0) {
            Label {
                Text(buffer.name)
                    .font(.system(size: HyaloDesign.FontSize.large))
            } icon: {
                Image(systemName: bufferIcon(buffer))
                    .font(.system(size: HyaloDesign.IconSize.standard))
                    .foregroundStyle(.secondary)
            }
            Spacer()
            if isHovered {
                Button {
                    viewModel.closeBuffer(buffer.name)
                } label: {
                    Image(systemName: "xmark")
                        .font(.system(size: 9, weight: .bold))
                        .foregroundStyle(.secondary)
                        .frame(width: 16, height: 16)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .accessibilityLabel("Close buffer: \(buffer.name)")
            } else if buffer.modified {
                Circle()
                    .fill(theme.accent)
                    .frame(width: 6, height: 6)
            }
        }
        .onHover { hovering in
            hoveredBufferId = hovering ? buffer.id : nil
        }
        .contextMenu { bufferContextMenu(buffer) }
        .accessibilityElement(children: .combine)
        .accessibilityLabel(buffer.name)
        .accessibilityValue(buffer.modified ? "Modified" : "")
    }

    // MARK: - Context Menu

    @ViewBuilder
    private func bufferContextMenu(_ buffer: BufferInfo) -> some View {
        if buffer.modified {
            Button("Save") {
                viewModel.saveBuffer(buffer.name)
            }
        }
        Button("Close") {
            viewModel.closeBuffer(buffer.name)
        }
        if let path = buffer.path, !path.isEmpty {
            Divider()
            Button("Reveal in Finder") {
                platformRevealInFinder([path])
            }
            Button("Copy Path") {
                platformCopyToClipboard(path)
            }
        }
    }

    // MARK: - Icon

    /// Derive icon from file extension when the buffer has a path,
    /// falling back to the Emacs-provided icon or a generic document.
    private func bufferIcon(_ buffer: BufferInfo) -> String {
        if let path = buffer.path, !path.isEmpty {
            let name = URL(fileURLWithPath: path).lastPathComponent
            return FileTreeIcons.icon(for: name)
        }
        return buffer.icon ?? "doc"
    }
}
