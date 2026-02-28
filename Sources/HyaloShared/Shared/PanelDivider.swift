// PanelDivider.swift - Divider component for panel sections
// Target: macOS 26 Tahoe with Liquid Glass design
// Opacity 0.65 dark / 0.13 light

import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
struct PanelDivider: View {
    var body: some View {
        Divider()
            .overlay(
                Color(platformColor: .separator)
            )
    }
}
