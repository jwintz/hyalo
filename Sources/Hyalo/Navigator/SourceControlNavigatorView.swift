// SourceControlNavigatorView.swift - Source control navigator
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import AppKit

@available(macOS 26.0, *)
struct SourceControlNavigatorView: View {
    @State private var selectedSegment: Int = 0
    private var viewModel: NavigatorViewModel { NavigatorManager.shared.viewModel }

    var body: some View {
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
            case 0: SourceControlChangesView(changedFiles: viewModel.changedFiles)
            case 1: SourceControlHistoryView(commits: viewModel.commitHistory)
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

@available(macOS 26.0, *)
private struct SourceControlChangesView: View {
    let changedFiles: [GitChangedFile]
    @Environment(\.colorTheme) private var theme

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
                    NavigatorManager.shared.onChangedFileSelect?(file.filePath)
                    HyaloModule.wakeEmacs()
                }
                .contextMenu {
                    Button("Reveal in Finder") {
                        NSWorkspace.shared.activateFileViewerSelecting(
                            [URL(fileURLWithPath: file.filePath)]
                        )
                    }
                    Divider()
                    Button("Copy Path") {
                        NSPasteboard.general.clearContents()
                        NSPasteboard.general.setString(file.filePath, forType: .string)
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

@available(macOS 26.0, *)
private struct SourceControlHistoryView: View {
    let commits: [GitCommitEntry]

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
                        NavigatorManager.shared.onCommitSelect?(commit.fullHash)
                        HyaloModule.wakeEmacs()
                    }
                    .contextMenu {
                        Button("Copy Commit Message") {
                            NSPasteboard.general.clearContents()
                            NSPasteboard.general.setString(commit.message, forType: .string)
                        }
                        Button("Copy Identifier") {
                            NSPasteboard.general.clearContents()
                            NSPasteboard.general.setString(commit.fullHash, forType: .string)
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

@available(macOS 26.0, *)
struct CommitHistoryRow: View {
    let commit: GitCommitEntry
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

    var body: some View {
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
                                    .fill(Color(nsColor: .quaternaryLabelColor))
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
                            .fill(Color(nsColor: .quaternaryLabelColor))
                    )

                Text(relativeDate(from: commit.date))
                    .font(.system(size: 11))
                    .foregroundStyle(.secondary)
            }
        }
        .padding(.vertical, 4)
    }

    /// Convert ISO date string to relative format
    private func relativeDate(from isoDate: String) -> String {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        if let date = formatter.date(from: isoDate) {
            let relative = RelativeDateTimeFormatter()
            relative.unitsStyle = .abbreviated
            return relative.localizedString(for: date, relativeTo: Date())
        }
        // Fallback: try without fractional seconds
        formatter.formatOptions = [.withInternetDateTime]
        if let date = formatter.date(from: isoDate) {
            let relative = RelativeDateTimeFormatter()
            relative.unitsStyle = .abbreviated
            return relative.localizedString(for: date, relativeTo: Date())
        }
        // Truncate to first 16 chars as fallback
        return String(isoDate.prefix(16))
    }
}
