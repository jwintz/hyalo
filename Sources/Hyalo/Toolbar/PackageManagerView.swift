// PackageManagerView.swift - Package management toolbar item
// Target: macOS 26 Tahoe with Liquid Glass design
// Shows a shippingbox icon with badge count for upgradable packages.
// :vc packages are listed but do NOT count toward the badge.
// Upgradable packages are grouped by archive (MELPA, GNU ELPA, etc.).
// Popover lists packages with Refresh/Upgrade/List Packages buttons.

import SwiftUI

// MARK: - Package Status Model

@available(macOS 26.0, *)
enum PackageOperation: String, Codable {
    case idle
    case refreshing
    case upgrading
}

@available(macOS 26.0, *)
struct UpgradablePackage: Codable, Identifiable {
    let name: String
    let installed: String
    let available: String
    let archive: String

    var id: String { name }
}

@available(macOS 26.0, *)
struct VCPackage: Codable, Identifiable {
    let name: String
    let version: String

    var id: String { name }
}

@available(macOS 26.0, *)
struct PackageStatusPayload: Codable {
    let status: String
    let upgradable: [UpgradablePackage]
    let vcPackages: [VCPackage]
    let lastChecked: String?
}

// MARK: - Package Manager View

@available(macOS 26.0, *)
struct PackageManagerView: View {
    @Bindable var viewModel: ToolbarViewModel

    @Environment(\.controlActiveState)
    private var activeState

    @State private var isHovering = false
    @State private var showPopover = false

    /// Badge count: only archive-upgradable packages (not :vc)
    private var badgeCount: Int {
        viewModel.upgradablePackages.count
    }

    private var isActive: Bool {
        viewModel.packageOperation != .idle
    }

    var body: some View {
        Button {
            showPopover.toggle()
        } label: {
            ZStack(alignment: .topTrailing) {
                Group {
                    if isActive {
                        Image(systemName: "shippingbox")
                            .symbolEffect(.rotate, isActive: true)
                    } else {
                        Image(systemName: "shippingbox")
                    }
                }
                .font(.system(size: 14))
                .foregroundStyle(activeState == .inactive ? .tertiary : .primary)

                // Badge — only archive-upgradable packages
                if badgeCount > 0 && !isActive {
                    Text("\(badgeCount)")
                        .font(.system(size: 8, weight: .bold, design: .rounded))
                        .foregroundStyle(.white)
                        .padding(.horizontal, 3)
                        .padding(.vertical, 1)
                        .background(
                            Capsule()
                                .fill(.blue)
                        )
                        .offset(x: 6, y: -6)
                }
            }
        }
        .help(helpText)
        .onHover { isHovering = $0 }
        .popover(isPresented: $showPopover, arrowEdge: .bottom) {
            PackagePopoverContent(viewModel: viewModel)
        }
    }

    private var helpText: String {
        switch viewModel.packageOperation {
        case .refreshing:
            return "Refreshing package archives…"
        case .upgrading:
            return "Upgrading packages…"
        case .idle:
            if badgeCount > 0 {
                return "\(badgeCount) package\(badgeCount == 1 ? "" : "s") upgradable"
            }
            return "Packages up to date"
        }
    }
}

// MARK: - Package Popover Content

@available(macOS 26.0, *)
private struct PackagePopoverContent: View {
    @Bindable var viewModel: ToolbarViewModel

    @Environment(\.colorScheme)
    private var colorScheme

    private var isActive: Bool {
        viewModel.packageOperation != .idle
    }

    private var hasUpgradable: Bool {
        !viewModel.upgradablePackages.isEmpty
    }

    private var hasVC: Bool {
        !viewModel.vcPackages.isEmpty
    }

    /// Group upgradable packages by archive
    private var archiveGroups: [(archive: String, packages: [UpgradablePackage])] {
        let grouped = Dictionary(grouping: viewModel.upgradablePackages) { $0.archive }
        return grouped.sorted { $0.key < $1.key }
            .map { (archive: $0.key, packages: $0.value) }
    }

    /// Unique archive names from available packages (for "all up to date" sections)
    private var knownArchives: [String] {
        let archives = Set(viewModel.upgradablePackages.map(\.archive))
        // If no upgradable packages, we can't know archives — just show generic
        if archives.isEmpty { return ["Archives"] }
        return archives.sorted()
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            // Header
            HStack {
                Image(systemName: "shippingbox")
                    .symbolEffect(.rotate, isActive: isActive)
                    .foregroundStyle(.secondary)
                Text("Packages")
                    .font(.system(size: 13, weight: .semibold))
                Spacer()

                // Action buttons
                HStack(spacing: 8) {
                    // List Packages — opens Emacs *Packages* buffer
                    Button {
                        viewModel.onPackageList?()
                        HyaloModule.wakeEmacs()
                    } label: {
                        Image(systemName: "list.bullet.rectangle")
                            .font(.system(size: 11))
                    }
                    .buttonStyle(.borderless)
                    .help("List Packages")

                    // Refresh archives
                    Button {
                        viewModel.onPackageRefresh?()
                        HyaloModule.wakeEmacs()
                    } label: {
                        Image(systemName: "arrow.clockwise")
                            .font(.system(size: 11))
                    }
                    .buttonStyle(.borderless)
                    .disabled(isActive)
                    .help("Refresh package archives")

                    if hasUpgradable {
                        Button {
                            viewModel.onPackageUpgradeAll?()
                            HyaloModule.wakeEmacs()
                        } label: {
                            Text("Upgrade All")
                                .font(.system(size: 11, weight: .medium))
                        }
                        .buttonStyle(.bordered)
                        .controlSize(.small)
                        .disabled(isActive)
                    }
                }
            }

            // Status indicator
            if isActive {
                HStack(spacing: 6) {
                    ProgressView()
                        .controlSize(.small)
                    Text(viewModel.packageOperation == .refreshing
                         ? "Refreshing archives…"
                         : "Upgrading packages…")
                        .font(.system(size: 11))
                        .foregroundStyle(.secondary)
                }
            }

            Divider()

            ScrollView {
                LazyVStack(alignment: .leading, spacing: 4) {
                    // Archive sections (grouped by archive, only upgradable packages)
                    if hasUpgradable {
                        ForEach(archiveGroups, id: \.archive) { group in
                            sectionHeader(
                                group.archive.uppercased(),
                                count: group.packages.count
                            )
                            ForEach(group.packages) { pkg in
                                UpgradablePackageRow(
                                    package: pkg,
                                    isActive: isActive,
                                    onUpgrade: {
                                        viewModel.onPackageUpgradeSingle?(pkg.name)
                                        HyaloModule.wakeEmacs()
                                    }
                                )
                            }
                        }
                    } else if !isActive {
                        // No upgradable packages — show "all up to date"
                        sectionHeader("ARCHIVES", count: 0)
                        HStack {
                            Image(systemName: "checkmark.circle.fill")
                                .foregroundStyle(.green)
                                .font(.system(size: 11))
                            Text("All packages are up to date")
                                .font(.system(size: 12))
                                .foregroundStyle(.secondary)
                        }
                        .padding(.vertical, 4)
                        .padding(.horizontal, 6)
                    }

                    // VC packages section
                    if hasVC {
                        if hasUpgradable || !isActive {
                            Divider()
                                .padding(.vertical, 2)
                        }
                        sectionHeader("VERSION CONTROL", count: viewModel.vcPackages.count)
                        ForEach(viewModel.vcPackages) { pkg in
                            VCPackageRow(
                                package: pkg,
                                isActive: isActive,
                                onUpgrade: {
                                    viewModel.onPackageUpgradeSingle?(pkg.name)
                                    HyaloModule.wakeEmacs()
                                }
                            )
                        }
                    }
                }
            }
            .frame(maxHeight: 350)

            // Last checked footer
            Divider()
            if let lastChecked = viewModel.lastChecked {
                Text("Last checked: \(lastChecked.formatted(date: .omitted, time: .shortened))")
                    .font(.system(size: 10))
                    .foregroundStyle(.tertiary)
            } else {
                Text("Last checked: Never")
                    .font(.system(size: 10))
                    .foregroundStyle(.tertiary)
            }
        }
        .padding(12)
        .frame(width: 340)
    }

    private func sectionHeader(_ title: String, count: Int) -> some View {
        HStack {
            Text(title)
                .font(.system(size: 10, weight: .medium))
                .foregroundStyle(.secondary)
                .textCase(.uppercase)
            if count > 0 {
                Text("(\(count))")
                    .font(.system(size: 10))
                    .foregroundStyle(.tertiary)
            }
        }
        .padding(.top, 2)
        .padding(.bottom, 1)
    }
}

// MARK: - Upgradable Package Row

@available(macOS 26.0, *)
private struct UpgradablePackageRow: View {
    let package: UpgradablePackage
    let isActive: Bool
    let onUpgrade: () -> Void

    @State private var isHovering = false

    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                Text(package.name)
                    .font(.system(size: 12, weight: .medium))
                HStack(spacing: 4) {
                    Text(package.installed)
                        .font(.system(size: 10, design: .monospaced))
                        .foregroundStyle(.secondary)
                    Image(systemName: "arrow.right")
                        .font(.system(size: 8))
                        .foregroundStyle(.tertiary)
                    Text(package.available)
                        .font(.system(size: 10, design: .monospaced))
                        .foregroundStyle(.blue)
                }
            }

            Spacer()

            if isHovering && !isActive {
                Button {
                    onUpgrade()
                } label: {
                    Image(systemName: "arrow.up.circle")
                        .font(.system(size: 14))
                        .foregroundStyle(.blue)
                }
                .buttonStyle(.borderless)
                .help("Upgrade \(package.name)")
                .transition(.opacity)
            }
        }
        .padding(.vertical, 4)
        .padding(.horizontal, 6)
        .background {
            RoundedRectangle(cornerRadius: 4)
                .fill(.primary.opacity(isHovering ? 0.04 : 0))
        }
        .onHover { isHovering = $0 }
        .animation(.easeInOut(duration: 0.15), value: isHovering)
    }
}

// MARK: - VC Package Row

@available(macOS 26.0, *)
private struct VCPackageRow: View {
    let package: VCPackage
    let isActive: Bool
    let onUpgrade: () -> Void

    @State private var isHovering = false

    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                Text(package.name)
                    .font(.system(size: 12, weight: .medium))
                HStack(spacing: 4) {
                    Text(package.version)
                        .font(.system(size: 10, design: .monospaced))
                        .foregroundStyle(.secondary)
                    Image(systemName: "arrow.triangle.branch")
                        .font(.system(size: 8))
                        .foregroundStyle(.tertiary)
                    Text("vc")
                        .font(.system(size: 10, design: .monospaced))
                        .foregroundStyle(.purple)
                }
            }

            Spacer()

            if isHovering && !isActive {
                Button {
                    onUpgrade()
                } label: {
                    Image(systemName: "arrow.up.circle")
                        .font(.system(size: 14))
                        .foregroundStyle(.purple)
                }
                .buttonStyle(.borderless)
                .help("Upgrade \(package.name)")
                .transition(.opacity)
            }
        }
        .padding(.vertical, 4)
        .padding(.horizontal, 6)
        .background {
            RoundedRectangle(cornerRadius: 4)
                .fill(.primary.opacity(isHovering ? 0.04 : 0))
        }
        .onHover { isHovering = $0 }
        .animation(.easeInOut(duration: 0.15), value: isHovering)
    }
}
