import SwiftUI

struct DropdownItemStyleModifier: ViewModifier {
    @State private var isHovering = false

    func body(content: Content) -> some View {
        content
            .padding(.vertical, 4)
            .padding(.horizontal, 8)
            .background(
                isHovering
                    ? AnyView(RoundedRectangle(cornerRadius: 6).fill(Color.accentColor))
                    : AnyView(Color.clear)
            )
            .foregroundStyle(isHovering ? Color.white : Color.primary)
            .clipShape(RoundedRectangle(cornerRadius: 6))
            .onHover { isHovering = $0 }
    }
}

extension View {
    func dropdownItemStyle() -> some View {
        modifier(DropdownItemStyleModifier())
    }
}
