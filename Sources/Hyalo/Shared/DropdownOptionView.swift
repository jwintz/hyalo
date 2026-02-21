// DropdownOptionView.swift - Reusable row in an instant-popover dropdown menu
// Target: macOS 26 Tahoe
//
// Shows an optional checkmark (for the currently selected item), a label,
// and a trailing spacer.  Applies `dropdownItemStyle()` for hover highlight.

import SwiftUI

@available(macOS 26.0, *)
struct DropdownOptionView: View {
    let label: String
    let isChecked: Bool
    let action: () -> Void

    var body: some View {
        HStack(spacing: 5) {
            // Checkmark column — fixed width keeps all labels aligned
            Group {
                if isChecked {
                    Image(systemName: "checkmark")
                        .fontWeight(.bold)
                        .imageScale(.small)
                } else {
                    Color.clear
                }
            }
            .frame(width: 10)

            Text(label)
            Spacer()
        }
        .dropdownItemStyle()
        .onTapGesture(perform: action)
        .accessibilityElement()
        .accessibilityLabel(label)
        .accessibilityAddTraits(isChecked ? [.isSelected] : [])
        .accessibilityAction(.default, action)
    }
}
