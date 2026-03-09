// PackageManagerView.swift - Package management toolbar item
// Target: macOS 26 Tahoe with Liquid Glass design
// Shows a shippingbox icon with badge count for upgradable packages.
// :vc packages are listed but do NOT count toward the badge.
// Upgradable packages are grouped by archive (MELPA, GNU ELPA, etc.).
// Popover lists packages with Refresh/Upgrade/List Packages buttons.

import SwiftUI

// MARK: - Package Status Model

@available(macOS 26.0, *)
public enum PackageOperation: String, Codable {
    case idle
    case refreshing
    case upgrading
}

@available(macOS 26.0, *)
public struct UpgradablePackage: Codable, Identifiable {
    public let name: String
    public let installed: String
    public let available: String
    public let archive: String

    public var id: String { name }
}

@available(macOS 26.0, *)
public struct VCPackage: Codable, Identifiable {
    public let name: String
    public let version: String

    public var id: String { name }
}

@available(macOS 26.0, *)
public struct PackageStatusPayload: Codable {
    public let status: String
    public let upgradable: [UpgradablePackage]
    public let vcPackages: [VCPackage]
    public let lastChecked: String?
}

// MARK: - Package Manager View

@available(macOS 26.0, *)
public struct PackageManagerView: View {
    @Bindable public var viewModel: ToolbarViewModel

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

    public var body: some View {
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
        // .bordered gives the Liquid Glass single-action toolbar button look in
        // macOS 26 without wrapping in a ControlGroup (which bridges to
        // NSToolbarItemGroup and causes compression/collapse under space pressure).
        .buttonStyle(.bordered)
        .help(helpText)
        .onHover { isHovering = $0 }
        .popover(isPresented: $showPopover, arrowEdge: .bottom) {
            PackagePopoverContent(viewModel: viewModel)
        }
    }

    public init(viewModel: ToolbarViewModel) {
        self.viewModel = viewModel
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

// MARK: - Content Height Preference Key

@available(macOS 26.0, *)
private struct ContentHeightKey: PreferenceKey {
    static var defaultValue: CGFloat = 0
    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = max(value, nextValue())
    }
}

// MARK: - Package Popover Content

@available(macOS 26.0, *)
private struct PackagePopoverContent: View {
    @Bindable var viewModel: ToolbarViewModel

    @Environment(\.colorScheme)
    private var colorScheme

    /// Measured content height of the package list.
    @State private var contentHeight: CGFloat = 0

    /// Window height for computing max list height.
    @State private var windowHeight: CGFloat = 600

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
                        wakeEmacs()
                    } label: {
                        Image(systemName: "list.bullet.rectangle")
                            .font(.system(size: 11))
                    }
                    .buttonStyle(.borderless)
                    .help("List Packages")

                    // Refresh archives
                    Button {
                        viewModel.onPackageRefresh?()
                        wakeEmacs()
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
                            wakeEmacs()
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
                                        wakeEmacs()
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
                                    wakeEmacs()
                                }
                            )
                        }
                    }
                }
                .background(
                    GeometryReader { geo in
                        Color.clear.preference(
                            key: ContentHeightKey.self,
                            value: geo.size.height)
                    }
                )
            }
            .frame(height: min(contentHeight, windowHeight * 0.7))
            .onPreferenceChange(ContentHeightKey.self) { contentHeight = $0 }

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
        .onAppear {
            if let window = NSApp.mainWindow {
                windowHeight = window.frame.height
            }
        }
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
