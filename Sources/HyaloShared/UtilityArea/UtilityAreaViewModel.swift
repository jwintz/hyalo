// UtilityAreaViewModel.swift - Utility area state model
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class UtilityAreaViewModel {
    public var isVisible: Bool = false
    public var isMaximized: Bool = false
    public var height: CGFloat = 260
    public var selectedTab: UtilityAreaTab? = .terminal

    public let diagnosticsViewModel = DiagnosticsViewModel()

    public init() {}
}
