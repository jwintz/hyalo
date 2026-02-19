// UtilityAreaViewModel.swift - Utility area state model
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
final class UtilityAreaViewModel {
    var isVisible: Bool = false
    var isMaximized: Bool = false
    var height: CGFloat = 260
    var selectedTab: UtilityAreaTab? = .terminal

    let diagnosticsViewModel = DiagnosticsViewModel()

    /// Per-frame terminal holder.  Each window controller gets its own
    /// UtilityAreaViewModel and therefore its own terminal instance.
    let terminalHolder = UtilityAreaTerminalHolder()
}
