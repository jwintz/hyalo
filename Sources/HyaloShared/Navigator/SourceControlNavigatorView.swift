// SourceControlNavigatorView.swift - Source control navigator
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import KelyphosKit

@available(macOS 26.0, iOS 26.0, *)
public struct SourceControlNavigatorView: View {
    @State private var selectedSegment: Int = 0
    @Environment(\.sourceControlViewModel) private var envSCVM
    @Environment(\.navigatorManager) private var envManager

    private var scVM: SourceControlViewModel { envSCVM ?? NavigatorManager.shared.sourceControlViewModel }

    public var body: some View {
        VStack(spacing: 0) {
            Picker("", selection: $selectedSegment) {
                Text("Changes").tag(0)
                Text("History").tag(1)
                Text("Repository").tag(2)
            }
            .pickerStyle(.segmented)
            .padding(.horizontal, HyaloDesign.Padding.compact)
            .padding(.vertical, 6)

            PanelDivider()

            switch selectedSegment {
            case 0: SourceControlChangesView(changedFiles: scVM.changedFiles)
            case 1: SourceControlHistoryView(commits: scVM.commitHistory)
            case 2:
                HyaloContentUnavailableView(
                    "Repository",
                    description: "Branch management available via magit",
                    systemImage: "arrow.triangle.branch"
                )
            default: EmptyView()
            }
        }
    }
}

// MARK: - Changes Tab

@available(macOS 26.0, iOS 26.0, *)
private struct SourceControlChangesView: View {
    let changedFiles: [GitChangedFile]
    @Environment(\.colorTheme) private var theme
    @Environment(\.navigatorManager) private var envManager

    var body: some View {
        if changedFiles.isEmpty {
            HyaloContentUnavailableView(
                "No Changes",
                description: "Working tree is clean",
                systemImage: "checkmark.circle"
            )
        } else {
            List(changedFiles) { file in
                HStack(spacing: 6) {
                    Image(systemName: FileTreeIcons.icon(for: file.fileName))
                        .foregroundStyle(.secondary)
                        .font(.system(size: HyaloDesign.FontSize.body))
                    Text(file.fileName)
                        .font(.system(size: HyaloDesign.FontSize.body))
                        .lineLimit(1)
                        .truncationMode(.middle)
                    Spacer()
                    Text(file.status)
                        .font(.system(size: HyaloDesign.FontSize.body, weight: .bold))
                        .foregroundStyle(statusColor(for: file.status))
                }
                .contentShape(Rectangle())
                .onTapGesture {
                    (envManager ?? NavigatorManager.shared).onChangedFileSelect?(file.filePath)
                    wakeEmacs()
                }
                .accessibilityElement(children: .combine)
                .accessibilityLabel("\(file.fileName), \(file.status)")
                .accessibilityAddTraits(.isButton)
                .contextMenu {
                    Button("Reveal in Finder") {
                        platformRevealInFinder([file.filePath])
                    }
                    Divider()
                    Button("Copy Path") {
                        platformCopyToClipboard(file.filePath)
                    }
                }
            }
            .listStyle(.sidebar)
            .scrollIndicators(.never)
            .environment(\.defaultMinListRowHeight, 22)
        }
    }

    private func statusColor(for status: String) -> Color {
        switch status {
        case "M": return theme.warning
        case "A": return theme.success
        case "D": return theme.error
        case "?": return .secondary
        case "R": return theme.link
        default: return .secondary
        }
    }
}

// MARK: - History Tab

@available(macOS 26.0, iOS 26.0, *)
private struct SourceControlHistoryView: View {
    let commits: [GitCommitEntry]
    @Environment(\.navigatorManager) private var envManager

    var body: some View {
        if commits.isEmpty {
            HyaloContentUnavailableView(
                "No History",
                description: "No commits found",
                systemImage: "clock.arrow.circlepath"
            )
        } else {
            List(commits) { commit in
                CommitHistoryRow(commit: commit)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        (envManager ?? NavigatorManager.shared).onCommitSelect?(commit.fullHash)
                        wakeEmacs()
                    }
                    .accessibilityElement(children: .combine)
                    .accessibilityLabel("Commit by \(commit.author): \(commit.message)")
                    .accessibilityAddTraits(.isButton)
                    .contextMenu {
                        Button("Copy Commit Message") {
                            platformCopyToClipboard(commit.message)
                        }
                        Button("Copy Identifier") {
                            platformCopyToClipboard(commit.fullHash)
                        }
                    }
            }
            .listStyle(.sidebar)
            .scrollIndicators(.never)
            .environment(\.defaultMinListRowHeight, 22)
        }
    }
}

// MARK: - Commit History Row

@available(macOS 26.0, iOS 26.0, *)
public struct CommitHistoryRow: View {
    public let commit: GitCommitEntry
    @Environment(\.colorTheme) private var theme

    /// Deterministic color from author email hash
    private var avatarColor: Color {
        let colors: [Color] = [
            .red, .orange, .yellow, .green, .mint,
            .teal, .cyan, .blue, .indigo, .purple,
            .pink, .brown
        ]
        let hash = abs(commit.authorEmail.hashValue)
        return colors[hash % colors.count]
    }

    public var body: some View {
        HStack(alignment: .top, spacing: 8) {
            // Avatar
            Image(systemName: "person.crop.circle.fill")
                .symbolRenderingMode(.hierarchical)
                .foregroundStyle(avatarColor)
                .font(.system(size: 24))
                .frame(width: 32, height: 32)

            // Content
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 4) {
                    Text(commit.author)
                        .fontWeight(.bold)
                        .font(.system(size: 11))
                        .lineLimit(1)

                    // Ref badges
                    ForEach(commit.refs, id: \.self) { ref in
                        Text(ref)
                            .font(.system(size: 10))
                            .padding(.horizontal, 4)
                            .padding(.vertical, 1)
                            .background(
                                RoundedRectangle(cornerRadius: 3)
                                    .fill(Color(platformColor: .quaternaryLabel))
                            )
                    }

                    // Tag badge
                    if !commit.tag.isEmpty {
                        Text(commit.tag)
                            .font(.system(size: 10))
                            .padding(.horizontal, 4)
                            .padding(.vertical, 1)
                            .background(
                                RoundedRectangle(cornerRadius: 3)
                                    .fill(theme.accent.opacity(0.2))
                            )
                    }
                }

                Text(commit.message)
                    .font(.system(size: 11))
                    .foregroundStyle(.secondary)
                    .lineLimit(2)
            }

            Spacer()

            // Hash + date
            VStack(alignment: .trailing, spacing: 4) {
                Text(commit.hash)
                    .font(.system(size: 10, design: .monospaced))
                    .padding(.horizontal, 4)
                    .padding(.vertical, 1)
                    .background(
                        RoundedRectangle(cornerRadius: 3)
                            .fill(Color(platformColor: .quaternaryLabel))
                    )

                Text(DateFormatting.relativeDate(from: commit.date))
                    .font(.system(size: 11))
                    .foregroundStyle(.secondary)
            }
        }
        .padding(.vertical, 4)
    }
}
