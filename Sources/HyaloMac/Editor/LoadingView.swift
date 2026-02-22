// LoadingView.swift - Startup proxy content shown in a separate NSWindow
// Target: macOS 26 Tahoe with Liquid Glass design
//
// The proxy window is created by decorateWindow() the moment the Emacs window
// is decorated.  The Emacs window itself stays invisible (visibility . nil)
// until hyalo-loading-done is called at the end of hyalo-window--post-setup,
// at which point the proxy window closes and the Emacs window appears.
//
// Messages are pushed from Elisp via hyalo-set-loading-message.

import AppKit
import SwiftUI

// MARK: - Observable Loading State

/// Lightweight observable state shared between LoadingView instances.
/// Decoupled from HyaloWorkspaceState so the proxy window can exist
/// before the main workspace is fully initialized.
@available(macOS 26.0, *)
@MainActor
@Observable
final class LoadingState {
    var message: String = ""
    var lispDir: String = ""
}

// MARK: - View

@available(macOS 26.0, *)
struct LoadingView: View {
    var state: LoadingState

    @State private var logoImage: NSImage? = nil
    @Environment(\.colorScheme) private var colorScheme

    var body: some View {
        VStack(spacing: 20) {
            Spacer()

            logoContent
                .frame(width: 96, height: 96)

            Text("Hyalo")
                .font(.system(size: 22, weight: .thin, design: .rounded))
                .tracking(8)
                .foregroundStyle(.primary)

            // Empty string kept as single space to hold layout height
            Text(state.message.isEmpty ? " " : state.message)
                .font(.system(size: 11, design: .monospaced))
                .foregroundStyle(.secondary)
                .animation(.default, value: state.message)
                .frame(height: 16)

            Spacer()
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .onAppear { loadLogo(from: state.lispDir) }
        .onChange(of: state.lispDir) { _, newDir in loadLogo(from: newDir) }
        .onChange(of: colorScheme) { _, _ in loadLogo(from: state.lispDir) }
    }

    // MARK: - Logo

    @ViewBuilder
    private var logoContent: some View {
        if let logo = logoImage {
            Image(nsImage: logo)
                .resizable()
                .scaledToFit()
        } else {
            Image(systemName: "terminal")
                .font(.system(size: 56, weight: .ultraLight))
                .symbolRenderingMode(.hierarchical)
        }
    }

    private func loadLogo(from dir: String) {
        guard !dir.isEmpty else { return }
        let path = dir + "/hyalo-splash.svg"
        guard FileManager.default.fileExists(atPath: path) else { return }
        guard var svgText = try? String(contentsOfFile: path, encoding: .utf8) else { return }

        // The SVG uses near-white fills (#f9f9f9) designed for dark backgrounds.
        // Recolor to a dark neutral in light mode so the logo stays legible.
        if colorScheme == .light {
            svgText = svgText.replacingOccurrences(of: "fill=\"#f9f9f9\"", with: "fill=\"#1a1a1a\"")
            svgText = svgText.replacingOccurrences(of: "fill=\"#F9F9F9\"", with: "fill=\"#1a1a1a\"")
        }

        guard let data = svgText.data(using: .utf8),
              let img = NSImage(data: data) else { return }
        img.size = NSSize(width: 96, height: 96)
        logoImage = img
    }
}
