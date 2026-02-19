// OpenQuicklyView.swift - Fuzzy file search panel (Cmd+O)
// Target: macOS 26 Tahoe with Liquid Glass design
// Keyboard navigation handled by SearchPanel's NSEvent monitor.
// Selection state lives in viewModel (synced with NSEvent callbacks).

import SwiftUI

@available(macOS 26.0, *)
struct OpenQuicklyView: View {
    @Bindable var viewModel: OpenQuicklyViewModel
    let onClose: () -> Void
    let openFile: (OpenQuicklyItem) -> Void

    @FocusState private var isSearchFocused: Bool

    private var selectionBinding: Binding<OpenQuicklyItem?> {
        Binding(
            get: { viewModel.selectedItem },
            set: { viewModel.selectedItem = $0 }
        )
    }

    var body: some View {
        VStack(spacing: 0) {
            // Search field
            HStack(alignment: .center, spacing: 0) {
                Image(systemName: "magnifyingglass")
                    .font(.system(size: 18))
                    .foregroundColor(.secondary)
                    .padding(.leading, 1)
                    .padding(.trailing, 10)

                TextField("Open Quickly", text: $viewModel.searchText)
                    .font(.system(size: 20, weight: .light))
                    .textFieldStyle(.plain)
                    .focused($isSearchFocused)
            }
            .padding(.vertical, 12)
            .padding(.horizontal, 12)
            .foregroundColor(.primary.opacity(0.85))

            Divider()

            if viewModel.filteredItems.isEmpty && !viewModel.searchText.isEmpty {
                Text("No matching files")
                    .font(.system(size: 14))
                    .foregroundColor(.secondary)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else if !viewModel.filteredItems.isEmpty {
                List(viewModel.filteredItems, selection: selectionBinding) { item in
                    OpenQuicklyRow(item: item)
                        .tag(item)
                        .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                        .contentShape(Rectangle())
                        .onTapGesture {
                            commitSelection(item)
                        }
                }
                .id(viewModel.searchText)
                .listStyle(.plain)
                .scrollContentBackground(.hidden)
                .environment(\.defaultMinListRowHeight, 40)
            }
        }
        .background(EffectView(.sidebar, blendingMode: .behindWindow))
        .edgesIgnoringSafeArea(.vertical)
        .frame(minWidth: 680, minHeight: 400, maxHeight: .infinity)
        .onChange(of: viewModel.searchText) { _, _ in
            viewModel.filterItems()
        }
        .onAppear {
            DispatchQueue.main.async {
                isSearchFocused = true
            }
        }
    }

    private func commitSelection(_ item: OpenQuicklyItem) {
        openFile(item)
        viewModel.searchText = ""
        onClose()
    }
}

// MARK: - Open Quickly Row

@available(macOS 26.0, *)
struct OpenQuicklyRow: View {
    let item: OpenQuicklyItem

    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: item.icon)
                .font(.system(size: 16))
                .foregroundStyle(.secondary)
                .frame(width: 24, alignment: .center)

            VStack(alignment: .leading, spacing: 2) {
                Text(item.name)
                    .font(.system(size: 12, weight: .medium))
                    .foregroundStyle(.primary)
                    .lineLimit(1)

                Text(item.relativePath ?? item.path)
                    .font(.system(size: 10))
                    .foregroundStyle(.tertiary)
                    .lineLimit(1)
            }

            Spacer()
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
    }
}
