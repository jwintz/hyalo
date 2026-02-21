// BranchPickerView.swift - Git branch picker for toolbar
// Target: macOS 26 Tahoe with Liquid Glass design
// Two-line layout with project name

import SwiftUI

@available(macOS 26.0, *)
struct BranchPickerView: View {
    @Bindable var viewModel: ToolbarViewModel

    @Environment(\.controlActiveState)
    private var controlActive

    @State private var isHovering = false

    var body: some View {
        HStack(alignment: .center, spacing: 7) {
            Group {
                if !viewModel.currentBranch.isEmpty {
                    Image(systemName: "arrow.triangle.branch")
                } else {
                    Image(systemName: "folder.fill.badge.gearshape")
                }
            }
            .foregroundColor(controlActive == .inactive ? inactiveColor : .secondary)
            .font(.system(size: 14))
            .imageScale(.medium)
            .frame(width: 17, height: 17)

            VStack(alignment: .leading, spacing: 0) {
                Text(title)
                    .font(.headline)
                    .foregroundColor(controlActive == .inactive ? inactiveColor : .primary)
                    .lineLimit(1)
                    .frame(height: 16)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .help(title)

                Menu {
                    ForEach(viewModel.branches, id: \.self) { branch in
                        Button {
                            viewModel.onBranchSwitch?(branch)
                        } label: {
                            HStack {
                                Text(branch)
                                if branch == viewModel.currentBranch {
                                    Image(systemName: "checkmark")
                                }
                            }
                        }
                    }
                } label: {
                    Text(viewModel.currentBranch.isEmpty ? "No branch" : viewModel.currentBranch)
                        .font(.subheadline)
                        .foregroundColor(controlActive == .inactive ? inactiveColor : .gray)
                        .frame(height: 11)
                }
                .frame(maxWidth: .infinity, alignment: .leading) // Force menu label to be leading
                .menuIndicator(isHovering ? .visible : .hidden)
                .buttonStyle(.borderless)
                .padding(.leading, -3)
                .padding(.bottom, 2)
                .disabled(viewModel.currentBranch.isEmpty)
            }
        }
        .onHover { isHovering = $0 }
    }

    private var inactiveColor: Color {
        Color(nsColor: .disabledControlTextColor)
    }

    private var title: String {
        viewModel.projectName.isEmpty ? "Emacs" : viewModel.projectName
    }
}
