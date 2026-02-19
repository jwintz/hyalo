// PanelDivider.swift - Divider component for panel sections
// Target: macOS 26 Tahoe with Liquid Glass design
// Opacity 0.65 dark / 0.13 light

import SwiftUI

@available(macOS 26.0, *)
struct PanelDivider: View {
    @Environment(\.colorScheme)
    private var colorScheme

    var body: some View {
        Divider()
            .overlay(
                Color.black.opacity(colorScheme == .dark ? 0.65 : 0.13)
            )
    }
}
