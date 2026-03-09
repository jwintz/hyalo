// UtilityAreaTab.swift - Utility area tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import KelyphosKit

@available(macOS 26.0, *)
public enum UtilityAreaTab: String, CaseIterable, KelyphosPanel {
    case terminal
    case diagnostics

    public var id: String { rawValue }

    public var title: String {
        switch self {
        case .terminal: return "Terminal"
        case .diagnostics: return "Diagnostics"
        }
    }

    public var systemImage: String {
        switch self {
        case .terminal: return "terminal"
        case .diagnostics: return "exclamationmark.triangle"
        }
    }

    public var body: some View {
        switch self {
        case .terminal: TerminalTabBody()
        case .diagnostics: DiagnosticsTabBody()
        }
    }
}

// MARK: - Terminal Content Environment Key

/// Injected by platform-specific code (HyaloMac) to provide
/// the terminal view into the utility area tab body.
@available(macOS 26.0, *)
private struct TerminalContentKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: AnyView = AnyView(EmptyView())
}

@available(macOS 26.0, *)
public extension EnvironmentValues {
    var terminalContent: AnyView {
        get { self[TerminalContentKey.self] }
        set { self[TerminalContentKey.self] = newValue }
    }
}

// MARK: - Tab Bodies (read view models from environment)

@available(macOS 26.0, *)
private struct TerminalTabBody: View {
    @Environment(\.terminalContent) private var content
    @Environment(HyaloWorkspaceState.self) private var workspace

    var body: some View {
        content
            .padding(.horizontal, workspace.horizontalMargin)
    }
}

@available(macOS 26.0, *)
private struct DiagnosticsTabBody: View {
    @Environment(\.utilityAreaViewModel) private var vm

    var body: some View {
        if let vm {
            DiagnosticsView(viewModel: vm.diagnosticsViewModel)
        }
    }
}
