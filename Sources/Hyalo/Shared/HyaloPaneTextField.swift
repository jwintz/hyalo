// HyaloPaneTextField.swift - Search/filter text field
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
struct HyaloPaneTextField<LeadingAccessories: View, TrailingAccessories: View>: View {
    enum Style {
        case bordered
        case plain
    }

    @Environment(\.colorScheme) var colorScheme
    @Environment(\.controlActiveState) private var controlActive

    @FocusState private var isFocused: Bool

    var label: String
    @Binding private var text: String
    let axis: Axis
    let leadingAccessories: LeadingAccessories?
    let trailingAccessories: TrailingAccessories?
    var clearable: Bool
    var onClear: (() -> Void)
    var hasValue: Bool
    var onSubmit: (() -> Void)?
    var style: Style

    init(
        _ label: String,
        text: Binding<String>,
        axis: Axis? = .horizontal,
        @ViewBuilder leadingAccessories: () -> LeadingAccessories? = { EmptyView() },
        @ViewBuilder trailingAccessories: () -> TrailingAccessories? = { EmptyView() },
        clearable: Bool? = false,
        onClear: (() -> Void)? = {},
        hasValue: Bool? = false,
        onSubmit: (() -> Void)? = nil,
        style: Style = .bordered
    ) {
        self.label = label
        _text = text
        self.axis = axis ?? .horizontal
        self.leadingAccessories = leadingAccessories()
        self.trailingAccessories = trailingAccessories()
        self.clearable = clearable ?? false
        self.onClear = onClear ?? {}
        self.hasValue = hasValue ?? false
        self.onSubmit = onSubmit
        self.style = style
    }

    @ViewBuilder
    func selectionBackground(_ isFocused: Bool = false) -> some View {
        if controlActive != .inactive || !text.isEmpty || hasValue {
            if isFocused || !text.isEmpty || hasValue {
                Color(.textBackgroundColor)
            } else {
                if colorScheme == .light {
                    Color.black.opacity(0.06)
                } else {
                    Color.white.opacity(0.24)
                }
            }
        } else {
            if colorScheme == .light {
                Color.clear
            } else {
                Color.white.opacity(0.14)
            }
        }
    }

    var body: some View {
        HStack(alignment: .center, spacing: 0) {
            if let leading = leadingAccessories {
                leading
                    .frame(height: 20)
            }
            
            ZStack(alignment: .leading) {
                if text.isEmpty {
                    Text(label)
                        .font(.system(size: 11))
                        .foregroundStyle(.secondary.opacity(0.8))
                        .padding(.horizontal, style == .plain ? 0 : 8)
                        .allowsHitTesting(false)
                }
                
                TextField("", text: $text, axis: axis)
                    .textFieldStyle(.plain)
                    .focused($isFocused)
                    .font(.system(size: 11))
                    .padding(.horizontal, style == .plain ? 0 : 8)
                    .frame(maxWidth: .infinity)
                    .frame(height: 22)
                    .foregroundStyle(.primary)
                    .onSubmit { onSubmit?() }
            }
            
            if clearable {
                Button {
                    text = ""
                    onClear()
                } label: {
                    Image(systemName: "xmark.circle.fill")
                        .font(.system(size: 11, weight: .semibold))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
                .frame(width: 20, height: 20)
                .opacity(text.isEmpty ? 0 : 1)
                .disabled(text.isEmpty)
            }
            if let trailing = trailingAccessories {
                trailing
            }
        }
        .frame(height: 22) // Stabilize container height
        .background {
            if style == .bordered {
                selectionBackground(isFocused)
                    .clipShape(RoundedRectangle(cornerRadius: 6))
            }
        }
        .overlay {
            if style == .bordered {
                RoundedRectangle(cornerRadius: 6)
                    .stroke(
                        isFocused || !text.isEmpty || hasValue ? .tertiary : .quaternary,
                        lineWidth: 1.25
                    )
                    .allowsHitTesting(false)
            }
        }
        .onTapGesture {
            isFocused = true
        }
    }
}
