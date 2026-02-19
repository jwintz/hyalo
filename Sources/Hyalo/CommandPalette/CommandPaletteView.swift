// CommandPaletteView.swift - M-x style command palette (Cmd+P)
// Target: macOS 26 Tahoe with Liquid Glass design
// Keyboard navigation handled by SearchPanel's NSEvent monitor.
// Selection state lives in viewModel (synced with NSEvent callbacks).

import SwiftUI

@available(macOS 26.0, *)
struct CommandPaletteView: View {
    @Bindable var viewModel: CommandPaletteViewModel
    let onClose: () -> Void
    let executeCommand: (CommandItem) -> Void

    @FocusState private var isSearchFocused: Bool

    private var selectionBinding: Binding<CommandItem?> {
        Binding(
            get: { viewModel.selectedCommand },
            set: { viewModel.selectedCommand = $0 }
        )
    }

    var body: some View {
        VStack(spacing: 0) {
            // Search field with M-x prefix
            HStack(alignment: .center, spacing: 0) {
                Text("M-x ")
                    .font(.system(size: 16, design: .monospaced))
                    .foregroundColor(.secondary)
                    .padding(.leading, 12)

                TextField("", text: $viewModel.searchText)
                    .font(.system(size: 20, weight: .light))
                    .textFieldStyle(.plain)
                    .focused($isSearchFocused)
            }
            .padding(.vertical, 12)
            .foregroundColor(.primary.opacity(0.85))

            Divider()

            if viewModel.filteredCommands.isEmpty && !viewModel.searchText.isEmpty {
                Text("No matching commands")
                    .font(.system(size: 14))
                    .foregroundColor(.secondary)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                List(viewModel.filteredCommands, selection: selectionBinding) { command in
                    CommandRow(command: command)
                        .tag(command)
                        .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                        .contentShape(Rectangle())
                        .onTapGesture {
                            commitCommand(command)
                        }
                }
                .id(viewModel.searchText)
                .listStyle(.plain)
                .scrollContentBackground(.hidden)
                .environment(\.defaultMinListRowHeight, 36)
            }
        }
        .background(EffectView(.sidebar, blendingMode: .behindWindow))
        .edgesIgnoringSafeArea(.vertical)
        .frame(minWidth: 680, minHeight: 400, maxHeight: .infinity)
        .onChange(of: viewModel.searchText) { _, _ in
            viewModel.filterCommands()
        }
        .onAppear {
            DispatchQueue.main.async {
                isSearchFocused = true
            }
        }
    }

    func commitCommand(_ command: CommandItem) {
        executeCommand(command)
        viewModel.searchText = ""
        onClose()
    }
}

// MARK: - Command Row

@available(macOS 26.0, *)
struct CommandRow: View {
    let command: CommandItem

    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: command.icon)
                .font(.system(size: 13))
                .foregroundStyle(.secondary)
                .frame(width: 20, alignment: .center)

            VStack(alignment: .leading, spacing: 1) {
                Text(command.name)
                    .font(.system(size: 12, weight: .medium))
                    .foregroundStyle(.primary)
                    .lineLimit(1)

                if !command.description.isEmpty {
                    Text(command.description)
                        .font(.system(size: 10))
                        .foregroundStyle(.tertiary)
                        .lineLimit(1)
                }
            }

            Spacer()

            if let keybinding = command.keybinding {
                HStack(spacing: 2) {
                    ForEach(keybinding.split(separator: " "), id: \.self) { key in
                        Text(String(key))
                            .font(.system(size: 10, weight: .medium))
                            .padding(.horizontal, 4)
                            .padding(.vertical, 2)
                            .background(
                                RoundedRectangle(cornerRadius: 3)
                                    .fill(Color.primary.opacity(0.08))
                            )
                    }
                }
                .foregroundStyle(.secondary)
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
    }
}
