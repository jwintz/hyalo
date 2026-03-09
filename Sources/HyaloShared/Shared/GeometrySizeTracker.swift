import SwiftUI

struct GeometrySizeTracker: View {
    let onChange: (CGFloat) -> Void
    let dimension: Dimension
    let onUpdate: (() -> Void)?

    enum Dimension {
        case width
        case height
    }

    init(dimension: Dimension = .width, onChange: @escaping (CGFloat) -> Void, onUpdate: (() -> Void)? = nil) {
        self.dimension = dimension
        self.onChange = onChange
        self.onUpdate = onUpdate
    }

    init(dimension: Dimension = .width, onChange: @escaping (CGFloat) -> Void) {
        self.init(dimension: dimension, onChange: onChange, onUpdate: nil)
    }

    var body: some View {
        GeometryReader { geometry in
            Color.clear
                .onAppear {
                    let size = dimension == .width ? geometry.size.width : geometry.size.height
                    onChange(size)
                }
                .onChange(of: dimension == .width ? geometry.size.width : geometry.size.height) { _, newSize in
                    onChange(newSize)
                    onUpdate?()
                }
        }
    }
}
