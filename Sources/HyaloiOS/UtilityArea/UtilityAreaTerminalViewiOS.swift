// UtilityAreaTerminalViewiOS.swift - UIViewRepresentable wrapping SwiftTerm's iOS TerminalView
// Display-only terminal stub — no PTY on iOS, so no startProcess() call.

#if canImport(UIKit)
import UIKit
import SwiftUI
import SwiftTerm

@available(iOS 26.0, *)
struct UtilityAreaTerminalViewiOS: UIViewRepresentable {
    func makeUIView(context: Context) -> TerminalView {
        let tv = TerminalView(frame: .zero)
        tv.optionAsMetaKey = true
        tv.terminalDelegate = context.coordinator
        return tv
    }

    func updateUIView(_ uiView: TerminalView, context: Context) {
        // No updates needed for display-only terminal
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
