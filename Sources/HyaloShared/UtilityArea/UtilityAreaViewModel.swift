// UtilityAreaViewModel.swift - Utility area state model
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
public final class UtilityAreaViewModel {
    public let diagnosticsViewModel = DiagnosticsViewModel()

    public init() {}
}
