// FindNavigatorView.swift - Search navigator
// Target: macOS 26 Tahoe with Liquid Glass design
// Migrated from legacy NavigatorViewModel to SearchViewModel (AUDIT.md #3)

import SwiftUI

@available(macOS 26.0, *)
struct FindNavigatorView: View {
    @State private var searchQuery: String = ""
    @State private var replaceText: String = ""
    @State private var showReplace: Bool = false
    private var searchVM: SearchViewModel { NavigatorManager.shared.searchViewModel }

    /// Results grouped by file
    private var groupedResults: [(file: String, results: [SearchResult])] {
        let dict = Dictionary(grouping: searchVM.results, by: \.file)
        return dict.map { (file: $0.key, results: $0.value) }
            .sorted { $0.file < $1.file }
    }

    var body: some View {
        VStack(spacing: 0) {
            // Search form
            VStack(spacing: HyaloDesign.Spacing.tight) {
                HyaloPaneTextField(
                    "Search",
                    text: $searchQuery,
                    leadingAccessories: {
                        Image(systemName: "magnifyingglass")
                            .font(.system(size: 12))
                            .foregroundStyle(.tertiary)
                            .padding(.leading, 8)
                            .frame(width: 20, height: 20)
                    },
                    trailingAccessories: {
                        Divider().frame(height: 14)
                        Button {
                            showReplace.toggle()
                        } label: {
                            Image(systemName: "arrow.up.arrow.down")
                                .font(.system(size: 11, weight: .semibold))
                                .foregroundStyle(.secondary)
                        }
                        .buttonStyle(.plain)
                        .frame(width: 28, height: 20)
                    },
                    clearable: true,
                    onSubmit: {
                        NavigatorManager.shared.executeSearch(searchQuery)
                    }
                )

                if showReplace {
                    HyaloPaneTextField(
                        "Replace",
                        text: $replaceText,
                        leadingAccessories: {
                            Image(systemName: "arrow.triangle.2.circlepath")
                                .font(.system(size: 12))
                                .foregroundStyle(.tertiary)
                                .padding(.leading, 8)
                                .frame(width: 20, height: 20)
                        }
                    )
                }
            }
            .padding(.horizontal, HyaloDesign.Padding.compact)
            .padding(.vertical, 6)

            PanelDivider()

            // Status line
            if searchVM.status == .found {
                HStack {
                    Text("\(searchVM.resultCount) results in \(searchVM.fileCount) files")
                        .font(.system(size: HyaloDesign.FontSize.caption))
                        .foregroundStyle(.secondary)
                    Spacer()
                }
                .padding(.horizontal, HyaloDesign.Padding.compact)
                .padding(.vertical, 4)
                PanelDivider()
            }

            // Results
            switch searchVM.status {
            case .searching:
                VStack(spacing: 8) {
                    ProgressView()
                        .controlSize(.small)
                    Text("Searching...")
                        .font(.system(size: HyaloDesign.FontSize.caption))
                        .foregroundStyle(.tertiary)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)

            case .noResults:
                HyaloContentUnavailableView(
                    "No Results",
                    description: "No matches found for \"\(searchQuery)\"",
                    systemImage: "magnifyingglass"
                )

            case .found:
                List {
                    ForEach(groupedResults, id: \.file) { group in
                        Section {
                            ForEach(group.results) { result in
                                Button {
                                    searchVM.selectResult(result)
                                } label: {
                                    Text(result.text)
                                        .font(.system(size: HyaloDesign.FontSize.body, design: .monospaced))
                                        .lineLimit(1)
                                        .foregroundStyle(.primary)
                                }
                                .buttonStyle(.plain)
                            }
                        } header: {
                            HStack {
                                Text(URL(fileURLWithPath: group.file).lastPathComponent)
                                    .font(.system(size: HyaloDesign.FontSize.caption, weight: .semibold))
                                Spacer()
                                Text("\(group.results.count)")
                                    .font(.system(size: HyaloDesign.FontSize.small))
                                    .foregroundStyle(.tertiary)
                            }
                        }
                    }
                }
                .listStyle(.sidebar)
                .scrollIndicators(.never)
                .environment(\.defaultMinListRowHeight, 22)

            case .none:
                HyaloContentUnavailableView(
                    "Search",
                    description: "Enter a search term above",
                    systemImage: "magnifyingglass"
                )
            }
        }
    }
}
