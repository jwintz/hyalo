// InspectorSection.swift - Reusable inspector section header
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
struct InspectorSection<Content: View>: View {
    let label: String
    @ViewBuilder let content: Content

    init(_ label: String, @ViewBuilder content: () -> Content) {
        self.label = label
        self.content = content()
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 11) {
            Text(label)
                .foregroundColor(.secondary)
                .fontWeight(.bold)
                .font(.system(size: 12))
                .padding(.horizontal, 12)

            VStack(alignment: .trailing, spacing: 5) {
                content
                Divider()
            }
        }
    }
}
