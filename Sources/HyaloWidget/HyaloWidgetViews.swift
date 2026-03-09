// HyaloWidgetViews.swift - SwiftUI views for the Hyalo desktop widget
// Target: macOS 26 Tahoe with Liquid Glass design
// Three widget families: systemSmall, systemMedium, systemLarge

import SwiftUI
import WidgetKit

// MARK: - Small Widget

/// Shows instance count and current buffer name.
@available(macOS 26.0, *)
struct HyaloWidgetSmallView: View {
    let data: HyaloWidgetData

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 6) {
                Image(nsImage: NSImage(byReferencingFile: "/Users/jwintz/Syntropment/hyalo-feedstock/icons/Emacs.icns").flatMap { $0 } ?? NSImage())
                    .font(.title2)
                    .foregroundStyle(.secondary)
                Text("\(data.instanceCount)")
                    .font(.system(.largeTitle, design: .rounded, weight: .bold))
                    .contentTransition(.numericText())
            }

            Text(data.instanceCount == 1 ? "instance" : "instances")
                .font(.caption)
                .foregroundStyle(.secondary)

            Spacer(minLength: 0)

            if let active = data.activeInstance {
                HStack(spacing: 4) {
                    Image(systemName: "doc.text")
                        .font(.caption2)
                        .foregroundStyle(.tertiary)
                    Text(active.currentBuffer)
                        .font(.caption)
                        .fontWeight(.medium)
                        .lineLimit(1)
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
    }
}

// MARK: - Medium Widget

/// Compact layout with all instance details.
@available(macOS 26.0, *)
struct HyaloWidgetMediumView: View {
    let data: HyaloWidgetData

    var body: some View {
        if let active = data.activeInstance {
            HStack(spacing: 16) {
                // Left: instance count + uptime
                VStack(spacing: 6) {
                    Text("\(data.instanceCount)")
                        .font(.system(.title, design: .rounded, weight: .bold))
                        .contentTransition(.numericText())
                    Text(data.instanceCount == 1 ? "instance" : "instances")
                        .font(.caption2)
                        .foregroundStyle(.secondary)

                    Divider().frame(width: 40)

                    Label(active.formattedUptime, systemImage: "clock")
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                    Label(String(format: "%.0f MB", active.memoryMB), systemImage: "memorychip")
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
                .frame(width: 80)

                Divider()

                // Right: active instance details
                VStack(alignment: .leading, spacing: 4) {
                    DetailRow(icon: "doc.text", label: active.currentBuffer)
                    DetailRow(icon: "folder", label: active.shortDirectory)
                    DetailRow(icon: "arrow.triangle.branch", label: active.gitBranch.isEmpty ? "—" : active.gitBranch)
                    DetailRow(icon: "chevron.left.forwardslash.chevron.right", label: active.prettyMode)
                    DetailRow(icon: "square.stack.3d.up", label: "\(active.bufferCount) buffers")
                }
                .frame(maxWidth: .infinity, alignment: .leading)
            }
            .padding()
        } else {
            noInstanceView
        }
    }
}

// MARK: - Large Widget

/// Expanded layout with recent files list.
@available(macOS 26.0, *)
struct HyaloWidgetLargeView: View {
    let data: HyaloWidgetData

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Header
            HStack {
                Image(nsImage: NSImage(byReferencingFile: "/Users/jwintz/Syntropment/hyalo-feedstock/icons/Emacs.icns").flatMap { $0 } ?? NSImage())
                    .font(.title3)
                    .foregroundStyle(.secondary)
                Text("\(data.instanceCount) Emacs \(data.instanceCount == 1 ? "Instance" : "Instances")")
                    .font(.headline)
                Spacer()
                Text(data.lastUpdated, style: .time)
                    .font(.caption2)
                    .foregroundStyle(.tertiary)
            }
            .padding(.bottom, 10)

            if data.instances.isEmpty {
                Spacer()
                noInstanceView
                Spacer()
            } else {
                ForEach(Array(data.instances.enumerated()), id: \.element.id) { index, instance in
                    if index > 0 {
                        Divider().padding(.vertical, 6)
                    }
                    instanceSection(instance)
                }
                Spacer(minLength: 0)
            }
        }
        .padding()
    }

    @ViewBuilder
    private func instanceSection(_ instance: HyaloInstanceData) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            // Instance header
            HStack {
                Text(instance.currentBuffer)
                    .font(.subheadline)
                    .fontWeight(.semibold)
                    .lineLimit(1)
                Spacer()
                HStack(spacing: 8) {
                    Label(instance.formattedUptime, systemImage: "clock")
                    Label(String(format: "%.0f MB", instance.memoryMB), systemImage: "memorychip")
                }
                .font(.caption2)
                .foregroundStyle(.secondary)
            }

            // Details grid
            HStack(spacing: 12) {
                DetailRow(icon: "folder", label: instance.shortDirectory)
                DetailRow(icon: "arrow.triangle.branch", label: instance.gitBranch.isEmpty ? "—" : instance.gitBranch)
                DetailRow(icon: "chevron.left.forwardslash.chevron.right", label: instance.prettyMode)
                DetailRow(icon: "square.stack.3d.up", label: "\(instance.bufferCount)")
            }
            .font(.caption)

            // Recent files
            if !instance.recentFiles.isEmpty {
                VStack(alignment: .leading, spacing: 2) {
                    Text("Recent")
                        .font(.caption2)
                        .foregroundStyle(.tertiary)
                        .padding(.top, 2)

                    ForEach(instance.recentFiles.prefix(5), id: \.self) { file in
                        HStack(spacing: 4) {
                            Image(systemName: "doc")
                                .font(.caption2)
                                .foregroundStyle(.tertiary)
                            Text(file)
                                .font(.caption)
                                .foregroundStyle(.secondary)
                                .lineLimit(1)
                        }
                    }
                }
            }
        }
    }
}

// MARK: - Extra Large Widget

/// Full dashboard with all instances, recent files, and system stats.
@available(macOS 26.0, *)
struct HyaloWidgetExtraLargeView: View {
    let data: HyaloWidgetData

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Header
            HStack {
                Image(systemName: "terminal")
                    .font(.title2)
                    .foregroundStyle(.secondary)
                VStack(alignment: .leading, spacing: 2) {
                    Text("Hyalo — Emacs Dashboard")
                        .font(.headline)
                    Text("\(data.instanceCount) \(data.instanceCount == 1 ? "instance" : "instances") running")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
                Spacer()
                Text(data.lastUpdated, style: .time)
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }
            .padding(.bottom, 12)

            if data.instances.isEmpty {
                Spacer()
                noInstanceView
                Spacer()
            } else {
                // Two-column grid for instances
                let columns = [GridItem(.flexible(), spacing: 16), GridItem(.flexible(), spacing: 16)]
                LazyVGrid(columns: columns, alignment: .leading, spacing: 12) {
                    ForEach(data.instances) { instance in
                        extraLargeInstanceCard(instance)
                    }
                }
                Spacer(minLength: 0)
            }
        }
        .padding()
    }

    @ViewBuilder
    private func extraLargeInstanceCard(_ instance: HyaloInstanceData) -> some View {
        VStack(alignment: .leading, spacing: 6) {
            // Instance header
            HStack {
                Text(instance.currentBuffer)
                    .font(.subheadline)
                    .fontWeight(.semibold)
                    .lineLimit(1)
                Spacer()
                Text("PID \(instance.pid)")
                    .font(.caption2)
                    .foregroundStyle(.tertiary)
            }

            // Stats row
            HStack(spacing: 12) {
                Label(instance.formattedUptime, systemImage: "clock")
                Label(String(format: "%.0f MB", instance.memoryMB), systemImage: "memorychip")
                Label("\(instance.bufferCount) buf", systemImage: "square.stack.3d.up")
            }
            .font(.caption2)
            .foregroundStyle(.secondary)

            Divider()

            // Details
            VStack(alignment: .leading, spacing: 3) {
                DetailRow(icon: "folder", label: instance.shortDirectory)
                DetailRow(icon: "arrow.triangle.branch", label: instance.gitBranch.isEmpty ? "—" : instance.gitBranch)
                DetailRow(icon: "chevron.left.forwardslash.chevron.right", label: instance.prettyMode)
            }
            .font(.caption)

            // Recent files
            if !instance.recentFiles.isEmpty {
                VStack(alignment: .leading, spacing: 2) {
                    Text("Recent")
                        .font(.caption2)
                        .foregroundStyle(.tertiary)
                        .padding(.top, 2)

                    ForEach(instance.recentFiles.prefix(8), id: \.self) { file in
                        HStack(spacing: 4) {
                            Image(systemName: "doc")
                                .font(.caption2)
                                .foregroundStyle(.tertiary)
                            Text(file)
                                .font(.caption)
                                .foregroundStyle(.secondary)
                                .lineLimit(1)
                        }
                    }
                }
            }
        }
        .padding(10)
        .background(.quaternary.opacity(0.3), in: RoundedRectangle(cornerRadius: 10))
    }
}

// MARK: - Shared Components

@available(macOS 26.0, *)
struct DetailRow: View {
    let icon: String
    let label: String

    var body: some View {
        HStack(spacing: 3) {
            Image(systemName: icon)
                .font(.caption2)
                .foregroundStyle(.tertiary)
                .frame(width: 12)
            Text(label)
                .lineLimit(1)
        }
    }
}

@available(macOS 26.0, *)
var noInstanceView: some View {
    VStack(spacing: 6) {
        Image(nsImage: NSImage(byReferencingFile: "/Users/jwintz/Syntropment/hyalo-feedstock/icons/Emacs.icns").flatMap { $0 } ?? NSImage())
            .font(.title)
            .foregroundStyle(.quaternary)
        Text("No Emacs instances")
            .font(.caption)
            .foregroundStyle(.secondary)
    }
    .frame(maxWidth: .infinity, maxHeight: .infinity)
}
