// StatusBarManager.swift - Shared status bar state singleton
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

@available(macOS 26.0, *)
@MainActor
final class StatusBarManager {
    static let shared = StatusBarManager()
    let viewModel = StatusBarViewModel()

    private init() {}

    func updateStatus(
        line: Int,
        column: Int,
        mode: String,
        encoding: String?,
        lineEnding: String?,
        indentStyle: String?,
        indentWidth: Int?,
        fileType: String?,
        fileSize: String?,
        minorModes: [String]?,
        modelineLHS: String?,
        modelineRHS: String?
    ) {
        viewModel.line = line
        viewModel.column = column
        viewModel.mode = mode
        if let encoding { viewModel.encoding = encoding }
        if let lineEnding { viewModel.lineEnding = lineEnding }
        if let indentStyle { viewModel.indentStyle = indentStyle }
        if let indentWidth { viewModel.indentWidth = indentWidth }
        if let fileType { viewModel.fileType = fileType }
        if let fileSize { viewModel.fileSize = fileSize }
        if let minorModes { viewModel.minorModes = minorModes }
        if let modelineLHS { viewModel.modelineLHS = modelineLHS }
        if let modelineRHS { viewModel.modelineRHS = modelineRHS }
    }
}
