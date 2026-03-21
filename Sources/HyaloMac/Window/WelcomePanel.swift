// WelcomePanel.swift - Floating NSPanel hosting KelyphosWelcomeView
// Target: macOS 26 Tahoe

import AppKit
import SwiftUI
import KelyphosKit
import HyaloShared

@available(macOS 26.0, *)
private final class WelcomeProjectPanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }

    override var acceptsMouseMovedEvents: Bool {
        get { true }
        set { super.acceptsMouseMovedEvents = newValue }
    }
}

/// Creates and manages the welcome panel shown when Emacs launches without file arguments.
@available(macOS 26.0, *)
@MainActor
final class WelcomePanel {

    private var panel: NSPanel?
    private let welcomeState: WelcomeState
    private let shellState: KelyphosShellState

    init(welcomeState: WelcomeState, shellState: KelyphosShellState) {
        self.welcomeState = welcomeState
        self.shellState = shellState
    }

    func show() {
        if panel != nil { return }

        let contentView = WelcomePanelContentView(
            welcomeState: welcomeState,
            shellState: shellState
        )

        let hostView = NSHostingView(rootView: contentView)
        hostView.frame = NSRect(x: 0, y: 0, width: 740, height: 432)

        let newPanel = WelcomeProjectPanel(
            contentRect: NSRect(x: 0, y: 0, width: 740, height: 432),
            styleMask: [.titled, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        newPanel.titleVisibility = .hidden
        newPanel.titlebarAppearsTransparent = true
        newPanel.isOpaque = false
        newPanel.backgroundColor = .clear
        newPanel.isMovableByWindowBackground = true
        newPanel.isFloatingPanel = false
        newPanel.becomesKeyOnlyIfNeeded = false
        newPanel.acceptsMouseMovedEvents = true
        newPanel.level = .normal
        newPanel.contentView = hostView

        // Hide standard window buttons
        newPanel.standardWindowButton(.closeButton)?.isHidden = true
        newPanel.standardWindowButton(.miniaturizeButton)?.isHidden = true
        newPanel.standardWindowButton(.zoomButton)?.isHidden = true

        // Center on screen
        newPanel.center()
        newPanel.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)

        panel = newPanel
        welcomeState.isVisible = true
    }

    func dismiss() {
        panel?.orderOut(nil)
        panel = nil
        welcomeState.isVisible = false
    }
}

// MARK: - Content View

@available(macOS 26.0, *)
private struct WelcomePanelContentView: View {
    var welcomeState: WelcomeState
    var shellState: KelyphosShellState

    var body: some View {
        KelyphosWelcomeView(
            title: "Hyalo",
            icon: logoImage,
            state: shellState,
            actions: [
                KelyphosWelcomeAction(systemImage: "folder", title: "Open Project") {
                    openProjectViaPanel()
                },
                KelyphosWelcomeAction(systemImage: "doc", title: "New File") {
                    welcomeState.onProjectSelected?("__new_file__")
                },
            ],
            recents: {
                WelcomeProjectListView(welcomeState: welcomeState)
            }
        )
    }

    private var logoImage: Image? {
        let dir = HyaloModule.baseLispDir
        guard !dir.isEmpty else { return nil }
        let path = dir + "/hyalo-splash.svg"
        guard FileManager.default.fileExists(atPath: path),
              var svgText = try? String(contentsOfFile: path, encoding: .utf8) else { return nil }

        // Recolor for light mode (same logic as LoadingView)
        let isDark = NSApp.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        if !isDark {
            svgText = svgText.replacingOccurrences(of: "fill=\"#f9f9f9\"", with: "fill=\"#1a1a1a\"")
            svgText = svgText.replacingOccurrences(of: "fill=\"#F9F9F9\"", with: "fill=\"#1a1a1a\"")
        }

        guard let data = svgText.data(using: .utf8),
              let nsImage = NSImage(data: data) else { return nil }
        nsImage.size = NSSize(width: 96, height: 96)
        return Image(nsImage: nsImage)
    }

    private func openProjectViaPanel() {
        let panel = NSOpenPanel()
        panel.canChooseFiles = false
        panel.canChooseDirectories = true
        panel.allowsMultipleSelection = false
        if panel.runModal() == .OK, let url = panel.url {
            welcomeState.onProjectSelected?(url.path)
        }
    }
}

// MARK: - Project List

@available(macOS 26.0, *)
private struct WelcomeProjectListView: View {
    var welcomeState: WelcomeState

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            Text("Recent Projects")
                .font(.system(size: 11, weight: .semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 16)
                .padding(.top, 12)
                .padding(.bottom, 8)

            if welcomeState.projects.isEmpty {
                Spacer()
                Text("No known projects")
                    .font(.system(size: 12))
                    .foregroundStyle(.tertiary)
                    .frame(maxWidth: .infinity)
                Spacer()
            } else {
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 0) {
                        ForEach(welcomeState.projects) { project in
                            ProjectRow(project: project) {
                                welcomeState.onProjectSelected?(project.path)
                            }
                        }
                    }
                }
            }
        }
    }
}

@available(macOS 26.0, *)
private struct ProjectRow: View {
    let project: WelcomeState.Project
    let action: () -> Void

    @State private var isHovered = false

    var body: some View {
        Button(action: action) {
            VStack(alignment: .leading, spacing: 2) {
                Text(project.name)
                    .font(.system(size: 13))
                    .foregroundStyle(.primary)
                    .lineLimit(1)
                Text(abbreviatePath(project.path))
                    .font(.system(size: 10))
                    .foregroundStyle(.tertiary)
                    .lineLimit(1)
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 6)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(isHovered ? Color.primary.opacity(0.08) : .clear, in: RoundedRectangle(cornerRadius: 6))
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .onHover { isHovered = $0 }
    }

    private func abbreviatePath(_ path: String) -> String {
        let home = NSHomeDirectory()
        if path.hasPrefix(home) {
            return "~" + path.dropFirst(home.count)
        }
        return path
    }
}
