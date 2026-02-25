// BranchPickerView.swift - Git branch picker for toolbar
// Target: macOS 26 Tahoe with Liquid Glass design
// Two-line layout with project name

import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
public struct BranchPickerView: View {
    @Bindable public var viewModel: ToolbarViewModel

    public init(viewModel: ToolbarViewModel) {
        self.viewModel = viewModel
    }
    #if os(macOS)
    @Environment(\.controlActiveState)
    private var controlActive

    @State private var isHovering = false
#endif

    public var body: some View {
        HStack(alignment: .center, spacing: 7) {
            Group {
                if !viewModel.currentBranch.isEmpty {
                    Image(systemName: "arrow.triangle.branch")
                } else {
                    Image(systemName: "folder.fill.badge.gearshape")
                }
            }
            #if os(macOS)
            .foregroundColor(controlActive == .inactive ? inactiveColor : .secondary)
#else
            .foregroundColor(.secondary)
#endif
            .font(.system(size: 14))
            .imageScale(.medium)
            .frame(width: 17, height: 17)

            VStack(alignment: .leading, spacing: 0) {
                Text(title)
                    .font(.headline)
                    #if os(macOS)
                    .foregroundColor(controlActive == .inactive ? inactiveColor : .primary)
#else
                    .foregroundColor(.primary)
#endif
                    .lineLimit(1)
                    .frame(height: 16)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .help(title)

                if !viewModel.currentBranch.isEmpty {
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
                        Text(viewModel.currentBranch)
                            .font(.subheadline)
                            #if os(macOS)
                            .foregroundColor(controlActive == .inactive ? inactiveColor : .gray)
#else
                            .foregroundColor(.gray)
#endif
                            .frame(height: 11)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading) // Force menu label to be leading
                    #if os(macOS)
                    .menuIndicator(isHovering ? .visible : .hidden)
#else
                    .menuIndicator(.visible)
#endif
                    .buttonStyle(.borderless)
                    .padding(.leading, -3)
                    .padding(.bottom, 2)
                }
            }
        }
        #if os(macOS)
        .onHover { isHovering = $0 }
#endif
    }

    private var inactiveColor: Color {
        Color.secondary
    }

    private var title: String {
        viewModel.projectName.isEmpty ? "Emacs" : viewModel.projectName
    }
}
