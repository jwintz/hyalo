// DiagnosticsView.swift - Utility area diagnostics panel
// Target: macOS 26 Tahoe with Liquid Glass design
// Displays eglot/flymake diagnostics grouped by file.
// Clean SwiftUI design, no AppKit fallback.
//
// Theme-aware: uses system-adaptive colors for severity indicators
// so they remain readable in both light and dark Emacs themes.

import SwiftUI

@available(macOS 26.0, *)
struct DiagnosticsView: View {
    @Bindable var viewModel: DiagnosticsViewModel

    var body: some View {
        if viewModel.diagnostics.isEmpty {
            HyaloContentUnavailableView(
                "No Issues",
                description: "Diagnostics from eglot will appear here",
                systemImage: "checkmark.circle"
            )
        } else {
            VStack(spacing: 0) {
                diagnosticsSummary
                PanelDivider()
                diagnosticsList
            }
        }
    }

    // MARK: - Summary Bar

    private var diagnosticsSummary: some View {
        HStack(spacing: HyaloDesign.Spacing.standard) {
            if viewModel.errorCount > 0 {
                summaryPill(
                    count: viewModel.errorCount,
                    severity: .error
                )
            }
            if viewModel.warningCount > 0 {
                summaryPill(
                    count: viewModel.warningCount,
                    severity: .warning
                )
            }
            if viewModel.noteCount > 0 {
                summaryPill(
                    count: viewModel.noteCount,
                    severity: .note
                )
            }
            Spacer()
            Text("\(viewModel.diagnostics.count) issue\(viewModel.diagnostics.count == 1 ? "" : "s")")
                .font(.system(size: HyaloDesign.FontSize.small))
                .foregroundStyle(.tertiary)
        }
        .padding(.horizontal, HyaloDesign.Padding.horizontal)
        .padding(.vertical, 6)
    }

    private func summaryPill(count: Int, severity: DiagnosticSeverity) -> some View {
        HStack(spacing: 4) {
            Image(systemName: severity.systemImage)
                .font(.system(size: HyaloDesign.FontSize.small))
                .foregroundStyle(severityColor(severity))
            Text("\(count)")
                .font(.system(size: HyaloDesign.FontSize.caption).monospacedDigit())
                .foregroundStyle(.primary)
        }
        .padding(.horizontal, 6)
        .padding(.vertical, 2)
        .background {
            Capsule()
                .fill(severityColor(severity).opacity(0.12))
                .stroke(severityColor(severity).opacity(0.2), lineWidth: 0.5)
        }
    }

    // MARK: - Diagnostics List

    private var diagnosticsList: some View {
        ScrollView {
            LazyVStack(alignment: .leading, spacing: 0) {
                ForEach(viewModel.groupedByFile, id: \.file) { group in
                    fileSection(group.file, items: group.items)
                }
            }
        }
        .scrollContentBackground(.hidden)
    }

    // MARK: - File Section

    private func fileSection(_ file: String, items: [DiagnosticItem]) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            // File header
            HStack(spacing: 5) {
                Image(systemName: fileIcon(for: file))
                    .font(.system(size: HyaloDesign.FontSize.small))
                    .foregroundStyle(.tertiary)
                Text((file as NSString).lastPathComponent)
                    .font(.system(size: HyaloDesign.FontSize.caption, weight: .medium))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                Text("— \((file as NSString).deletingLastPathComponent.abbreviatingWithTildeInPath)")
                    .font(.system(size: HyaloDesign.FontSize.small))
                    .foregroundStyle(.quaternary)
                    .lineLimit(1)
                    .truncationMode(.middle)
                Spacer()
            }
            .padding(.horizontal, HyaloDesign.Padding.horizontal)
            .padding(.vertical, 4)
            .background {
                Color(nsColor: .separatorColor).opacity(0.08)
            }

            // Diagnostic rows
            ForEach(items) { item in
                diagnosticRow(item)
            }
        }
    }

    // MARK: - Diagnostic Row

    private func diagnosticRow(_ item: DiagnosticItem) -> some View {
        Button {
            viewModel.onNavigate?(item.file, item.line, item.column)
        } label: {
            HStack(alignment: .top, spacing: HyaloDesign.Spacing.compact) {
                // Severity indicator
                Image(systemName: item.severity.systemImage)
                    .font(.system(size: 10))
                    .foregroundStyle(severityColor(item.severity))
                    .frame(width: 14, alignment: .center)
                    .padding(.top, 2)

                // Message and location
                VStack(alignment: .leading, spacing: 2) {
                    Text(item.message)
                        .font(.system(size: HyaloDesign.FontSize.body))
                        .foregroundStyle(.primary)
                        .lineLimit(3)
                        .multilineTextAlignment(.leading)

                    HStack(spacing: 6) {
                        Label {
                            Text("Ln \(item.line)")
                                .font(.system(size: HyaloDesign.FontSize.small).monospacedDigit())
                        } icon: {
                            Image(systemName: "arrow.right")
                                .font(.system(size: 7))
                        }
                        .foregroundStyle(.tertiary)

                        if !item.source.isEmpty {
                            Text(item.source)
                                .font(.system(size: HyaloDesign.FontSize.small))
                                .foregroundStyle(.tertiary)
                                .padding(.horizontal, 4)
                                .padding(.vertical, 1)
                                .background {
                                    Capsule()
                                        .fill(Color(nsColor: .separatorColor).opacity(0.15))
                                }
                        }
                    }
                }

                Spacer(minLength: 0)
            }
            .padding(.horizontal, HyaloDesign.Padding.horizontal)
            .padding(.vertical, 4)
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
    }

    // MARK: - Helpers

    /// System-adaptive severity colors that remain readable in both
    /// light and dark themes. Uses SwiftUI semantic colors rather
    /// than theme-derived hex values for guaranteed contrast.
    private func severityColor(_ severity: DiagnosticSeverity) -> Color {
        switch severity {
        case .error: return .red
        case .warning: return .orange
        case .note: return .secondary
        }
    }

    private func fileIcon(for path: String) -> String {
        let ext = (path as NSString).pathExtension.lowercased()
        switch ext {
        case "swift": return "swift"
        case "el": return "doc.text"
        case "md": return "doc.plaintext"
        case "json": return "curlybraces"
        case "js", "ts", "tsx", "jsx": return "chevron.left.forwardslash.chevron.right"
        default: return "doc"
        }
    }
}

// MARK: - NSString Path Extension

private extension String {
    var abbreviatingWithTildeInPath: String {
        (self as NSString).abbreviatingWithTildeInPath
    }
}
