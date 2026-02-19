// FooterPattern.swift - Footer pattern rendering
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
struct FooterPatternView: View {
    let pattern: FooterPattern
    let alpha: CGFloat

    var body: some View {
        if pattern != .none {
            Canvas { context, size in
                drawPattern(context: context, size: size)
            }
            .opacity(alpha)
            .allowsHitTesting(false)
        }
    }

    private func drawPattern(context: GraphicsContext, size: CGSize) {
        let color = Color.primary.opacity(0.05)
        let spacing: CGFloat = 20

        switch pattern {
        case .hideout:
            for x in stride(from: 0, through: size.width, by: spacing) {
                for y in stride(from: 0, through: size.height, by: spacing) {
                    let rect = CGRect(x: x, y: y, width: 4, height: 4)
                    context.fill(Path(ellipseIn: rect), with: .color(color))
                }
            }
        case .topography:
            for y in stride(from: 0, through: size.height, by: spacing) {
                var path = Path()
                path.move(to: CGPoint(x: 0, y: y))
                for x in stride(from: 0, through: size.width, by: 10) {
                    let offset = sin(x / 30 + y / 20) * 5
                    path.addLine(to: CGPoint(x: x, y: y + offset))
                }
                context.stroke(path, with: .color(color), lineWidth: 0.5)
            }
        case .circuitBoard:
            for x in stride(from: 0, through: size.width, by: spacing * 2) {
                for y in stride(from: 0, through: size.height, by: spacing * 2) {
                    var path = Path()
                    path.move(to: CGPoint(x: x, y: y))
                    path.addLine(to: CGPoint(x: x + spacing, y: y))
                    path.addLine(to: CGPoint(x: x + spacing, y: y + spacing))
                    context.stroke(path, with: .color(color), lineWidth: 0.5)
                }
            }
        case .none:
            break
        }
    }
}
