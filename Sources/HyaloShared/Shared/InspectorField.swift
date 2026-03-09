// InspectorField.swift - Reusable inspector key-value field
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
struct InspectorField<Content: View>: View {
    let label: String
    @ViewBuilder let content: Content

    init(_ label: String, @ViewBuilder content: () -> Content) {
        self.label = label
        self.content = content()
    }

    var body: some View {
        HStack(alignment: .top, spacing: 5) {
            Text(label)
                .foregroundColor(.primary)
                .fontWeight(.regular)
                .font(.system(size: 10))
                .padding(.top, 3)
                .frame(maxWidth: 72, alignment: .trailing)

            VStack(alignment: .leading) {
                content
            }
            .font(.system(size: 10))
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .padding(.horizontal, 12)
    }
}
