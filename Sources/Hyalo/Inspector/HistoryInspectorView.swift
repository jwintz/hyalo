// HistoryInspectorView.swift - Git history inspector
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import AppKit

@available(macOS 26.0, *)
struct HistoryInspectorView: View {
    @Bindable var viewModel: InspectorViewModel

    var body: some View {
        if viewModel.commits.isEmpty {
            HyaloContentUnavailableView(
                "No History",
                description: "No git history for this file",
                systemImage: "clock.arrow.circlepath"
            )
        } else {
            List(viewModel.commits) { commit in
                InspectorCommitRow(commit: commit)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        let hash = commit.fullHash ?? commit.hash
                        InspectorManager.shared.onCommitSelect?(hash)
                        HyaloModule.wakeEmacs()
                    }
                    .contextMenu {

                        Button("Copy Commit Message") {
                            NSPasteboard.general.clearContents()
                            NSPasteboard.general.setString(commit.message, forType: .string)
                        }
                        Button("Copy Identifier") {
                            NSPasteboard.general.clearContents()
                            NSPasteboard.general.setString(
                                commit.fullHash ?? commit.hash,
                                forType: .string
                            )
                        }
                    }
            }
            .listStyle(.sidebar)
            .scrollIndicators(.never)
            .environment(\.defaultMinListRowHeight, 22)
        }
    }
}

// MARK: - Inspector Commit Row

@available(macOS 26.0, *)
private struct InspectorCommitRow: View {
    let commit: Commit
    @Environment(\.colorTheme) private var theme

    /// Deterministic color from author email hash
    private var avatarColor: Color {
        let colors: [Color] = [
            .red, .orange, .yellow, .green, .mint,
            .teal, .cyan, .blue, .indigo, .purple,
            .pink, .brown
        ]
        let hash = abs((commit.authorEmail ?? commit.author).hashValue)
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
                    if let refs = commit.refs {
                        ForEach(refs, id: \.self) { ref in
                            Text(ref)
                                .font(.system(size: 10))
                                .padding(.horizontal, 4)
                                .padding(.vertical, 1)
                                .background(
                                    RoundedRectangle(cornerRadius: 3)
                                        .fill(Color(nsColor: .quaternaryLabelColor))
                                )
                        }
                    }

                    // Tag badge
                    if let tag = commit.tag, !tag.isEmpty {
                        Text(tag)
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
                Text(String(commit.hash.prefix(8)))
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
        formatter.formatOptions = [.withInternetDateTime]
        if let date = formatter.date(from: isoDate) {
            let relative = RelativeDateTimeFormatter()
            relative.unitsStyle = .abbreviated
            return relative.localizedString(for: date, relativeTo: Date())
        }
        return String(isoDate.prefix(16))
    }
}
