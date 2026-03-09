// ProjectNavigatorView.swift - Project file tree navigator
// Target: macOS 26 Tahoe with Liquid Glass design
// Uses HyaloProjectNavigator backed by native FileManager-based file tree.

import SwiftUI

@available(macOS 26.0, *)
public struct ProjectNavigatorView: View {
    @Bindable public var viewModel: ProjectNavigatorViewModel

    public init(viewModel: ProjectNavigatorViewModel? = nil) {
        self.viewModel = viewModel ?? NavigatorManager.shared.projectNavigatorViewModel
    }

    public var body: some View {
        VStack(spacing: 0) {
            if viewModel.projectRoot != nil {
                HyaloProjectNavigator(
                    viewModel: viewModel,
                    onFileSelect: { path in
                        let active = viewModel.activeFilePath
                        let skip = (path == active)
                        guard !skip else { return }
                        viewModel.selectFile(path)
                    }
                )
                .id(viewModel.filterText)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                HyaloContentUnavailableView(
                    "No Project Open",
                    description: "Open a project to browse files",
                    systemImage: "folder"
                )
            }
        }
        .safeAreaInset(edge: .bottom, spacing: 0) {
            HStack(spacing: 5) {
                HyaloPaneTextField(
                    "Filter",
                    text: $viewModel.filterText,
                    leadingAccessories: {
                        Image(systemName: "magnifyingglass")
                            .font(.system(size: 11))
                            .foregroundStyle(.secondary)
                            .padding(.leading, 8)
                            .frame(width: 26, height: 20)
                    },
                    trailingAccessories: {
                        HStack(spacing: 2) {
                            Button {
                                viewModel.sortFoldersOnTop.toggle()
                            } label: {
                                Image(systemName: viewModel.sortFoldersOnTop
                                    ? "line.3.horizontal.decrease.circle.fill"
                                    : "line.3.horizontal.decrease.circle")
                                    .font(.system(size: 11))
                                    .foregroundStyle(viewModel.sortFoldersOnTop ? .primary : .secondary)
                            }
                            .buttonStyle(.plain)
                            .frame(width: 18, height: 20)
                            .help("Sort Folders on Top")
                            .accessibilityLabel("Sort Folders on Top")
                            .accessibilityValue(viewModel.sortFoldersOnTop ? "On" : "Off")
                            .accessibilityAddTraits(.isToggle)

                            Button {
                                viewModel.sourceControlFilter.toggle()
                            } label: {
                                Image(systemName: viewModel.sourceControlFilter
                                    ? "plusminus.circle.fill"
                                    : "plusminus.circle")
                                    .font(.system(size: 11))
                                    .foregroundStyle(viewModel.sourceControlFilter ? .primary : .secondary)
                            }
                            .buttonStyle(.plain)
                            .frame(width: 18, height: 20)
                            .help("Source Control Filter (Changes Only)")
                            .accessibilityLabel("Source Control Filter")
                            .accessibilityValue(viewModel.sourceControlFilter ? "On" : "Off")
                            .accessibilityAddTraits(.isToggle)
                        }
                        .padding(.trailing, 4)
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
}
