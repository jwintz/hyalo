// UtilityAreaTerminalViewiOS.swift - UIViewRepresentable wrapping SwiftTerm's iOS TerminalView
// Target: iOS 26 with Liquid Glass design
//
// Display-only terminal stub — no PTY on iOS, so no startProcess() call.
// Uses @Observable TerminalPalette for appearance-aware theming.

#if canImport(UIKit)
import UIKit
import SwiftUI
import SwiftTerm
import HyaloShared
import OSLog

@available(iOS 26.0, *)
private let logger = Logger(subsystem: "hyalo", category: "UtilityAreaTerminalViewiOS")

@available(iOS 26.0, *)
struct UtilityAreaTerminalViewiOS: UIViewRepresentable {
    /// The palette to use for theming. Use `.shared` for the global palette.
    @Bindable var palette: TerminalPalette

    init(palette: TerminalPalette = .shared) {
        self.palette = palette
    }

    func makeUIView(context: Context) -> TerminalView {
        logger.info("🔧 UtilityAreaTerminalViewiOS.makeUIView called")
        logger.info("   - palette isDark: \(palette.isDark)")
        logger.info("   - palette version: \(palette.version)")

        let tv = TerminalView(frame: .zero)
        tv.optionAsMetaKey = true
        tv.terminalDelegate = context.coordinator

        // Apply palette colors
        logger.info("🎨 Applying palette in makeUIView")
        tv.applyPalette(palette)

        return tv
    }

    func updateUIView(_ uiView: TerminalView, context: Context) {
        logger.info("🔧 UtilityAreaTerminalViewiOS.updateUIView called")
        logger.info("   - palette isDark: \(palette.isDark)")
        logger.info("   - palette version: \(palette.version)")
        // Reapply palette when version or appearance changes
        uiView.applyPalette(palette)
    }

    func makeCoordinator() -> Coordinator {
        Coordinator()
    }

    class Coordinator: NSObject, TerminalViewDelegate {
        func sizeChanged(source: TerminalView, newCols: Int, newRows: Int) {
            // No-op: display-only terminal
        }

        func setTerminalTitle(source: TerminalView, title: String) {
            // No-op: display-only terminal
        }

        func hostCurrentDirectoryUpdate(source: TerminalView, directory: String?) {
            // No-op: display-only terminal
        }

        func send(source: TerminalView, data: ArraySlice<UInt8>) {
            // No-op: display-only terminal
        }

        func scrolled(source: TerminalView, position: Double) {
            // No-op: display-only terminal
        }

        func requestOpenLink(source: TerminalView, link: String, params: [String: String]) {
            // No-op: display-only terminal
        }

        func bell(source: TerminalView) {
            // No-op: display-only terminal
        }

        func clipboardCopy(source: TerminalView, content: Data) {
            // No-op: display-only terminal
        }

        func iTermContent(source: TerminalView, content: ArraySlice<UInt8>) {
            // No-op: display-only terminal
        }

        func rangeChanged(source: TerminalView, startY: Int, endY: Int) {
            // No-op: display-only terminal
        }
    }
}
#endif
