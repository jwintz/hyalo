import SwiftUI

struct HyaloContentUnavailableView<Actions: View>: View {
    let label: String
    let description: String?
    let systemImage: String?
    let actions: Actions?

    init(
        _ label: String,
        description: String? = nil,
        systemImage: String? = nil
    ) where Actions == EmptyView {
        self.label = label
        self.description = description
        self.systemImage = systemImage
        self.actions = nil
    }

    init(
        _ label: String,
        description: String? = nil,
        systemImage: String? = nil,
        @ViewBuilder actions: () -> Actions
    ) {
        self.label = label
        self.description = description
        self.systemImage = systemImage
        self.actions = actions()
    }

    var body: some View {
        VStack(spacing: 14) {
            if let systemImage {
                Image(systemName: systemImage)
                    .font(.system(size: 28))
                    .foregroundStyle(.tertiary)
                    .padding(.bottom, 8)
            }

            VStack(spacing: 5) {
                Text(label)
                    .font(.system(size: 16.5, weight: systemImage != nil ? .bold : .regular))

                if let description {
                    Text(description)
                        .font(.system(size: 10))
                        .multilineTextAlignment(.center)
                }
            }

            if let actions {
                HStack { actions }
            }
        }
        .foregroundStyle(.secondary)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .contentShape(Rectangle())
    }
}
