// MinibufferView.swift - Native Swift overlay for Emacs minibuffer
// Prompt + input (read-only) on top, candidate list on bottom.
// Text editing happens in the Emacs minibuffer; this is a display-only overlay.

import SwiftUI

@available(macOS 26.0, *)
public struct MinibufferView: View {
    @Bindable public var viewModel: MinibufferViewModel

    @ViewBuilder private var panelBackground: some View {
        Color.clear.glassEffect(in: RoundedRectangle(cornerRadius: 12))
    }

    public init(viewModel: MinibufferViewModel) {
        self.viewModel = viewModel
    }

    private var hasCandidates: Bool {
        !viewModel.candidates.isEmpty || viewModel.totalCandidates > 0
    }

    public var body: some View {
        VStack(spacing: 0) {
            // Prompt + input (read-only mirror of Emacs minibuffer)
            HStack(alignment: .center, spacing: 0) {
                if !viewModel.prompt.isEmpty {
                    Text(viewModel.prompt)
                        .font(.system(size: 14, design: .monospaced))
                        .foregroundStyle(.secondary)
                        .padding(.leading, 12)
                        .lineLimit(1)
                }

                // Blinking cursor + input text in monospace
                HStack(spacing: 0) {
                    Text(viewModel.input)
                    Cursor()
                }
                .font(.system(size: 14, design: .monospaced))
                .frame(maxWidth: .infinity, alignment: .leading)
                .accessibilityLabel("Minibuffer input")
            }
            .padding(.vertical, 10)
            .foregroundColor(.primary.opacity(0.85))

            Divider()

            // Candidate count
            if viewModel.totalCandidates > 0 {
                HStack {
                    Text("\(viewModel.totalCandidates) candidates")
                        .font(.system(size: 10))
                        .foregroundStyle(.quaternary)
                    Spacer()
                }
                .padding(.horizontal, 12)
                .padding(.vertical, 3)
            }

            // Candidate list
            if viewModel.candidates.isEmpty && !viewModel.input.isEmpty && !viewModel.historyMode {
                Text("No matching candidates")
                    .font(.system(size: 13))
                    .foregroundStyle(.secondary)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else if !viewModel.candidates.isEmpty {
                ScrollViewReader { proxy in
                    List(Array(viewModel.candidates.enumerated()), id: \.element.id) { index, candidate in
                        MinibufferCandidateRow(
                            candidate: candidate,
                            isSelected: index == viewModel.selectedIndex,
                            annotationColumnChars: viewModel.annotationColumnChars
                        )
                        .id(candidate.id)
                        .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                        .listRowSeparator(.hidden)
                        .contentShape(Rectangle())
                        .onTapGesture {
                            viewModel.onCandidateSelected?(index)
                        }
                    }
                    .listStyle(.plain)
                    .scrollContentBackground(.hidden)
                    .environment(\.defaultMinListRowHeight, 28)
                    .onChange(of: viewModel.selectedIndex) { _, newIndex in
                        if newIndex >= 0 && newIndex < viewModel.candidates.count {
                            withAnimation(.easeOut(duration: 0.1)) {
                                proxy.scrollTo(viewModel.candidates[newIndex].id, anchor: .center)
                            }
                        }
                    }
                }
            }
        }
        .background(panelBackground)
        .edgesIgnoringSafeArea(.vertical)
        .frame(
            minWidth: 680,
            minHeight: hasCandidates ? 400 : nil,
            maxHeight: .infinity,
            alignment: .top
        )
    }
}

// MARK: - Blinking Cursor

@available(macOS 26.0, *)
private struct Cursor: View {
    var body: some View {
        Rectangle()
            .fill(Color.accentColor)
            .frame(width: 1.5, height: 16)
    }
}

// MARK: - Candidate Row

@available(macOS 26.0, *)
struct MinibufferCandidateRow: View, Equatable {
    let candidate: MinibufferCandidate
    let isSelected: Bool
    let annotationColumnChars: Int

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.candidate == rhs.candidate
            && lhs.isSelected == rhs.isSelected
            && lhs.annotationColumnChars == rhs.annotationColumnChars
    }

    // Approximate width per character for 10pt monospaced system font (annotation)
    private static let annotationCharWidth: CGFloat = 6.02

    private var annotationColumnWidth: CGFloat {
        CGFloat(annotationColumnChars) * Self.annotationCharWidth
    }

    /// Build an AttributedString with match ranges highlighted in accent color.
    /// Falls back to a plain AttributedString when no ranges are available.
    private func attributedCandidateText() -> AttributedString {
        var str = AttributedString(candidate.text)
        let baseWeight: Font.Weight = isSelected ? .semibold : .regular
        let baseColor = Color.primary.opacity(isSelected ? 1.0 : 0.85)
        str.font = .system(size: 12, weight: baseWeight, design: .monospaced)
        str.foregroundColor = baseColor

        guard let ranges = candidate.matchRanges, !ranges.isEmpty else { return str }

        let text = candidate.text
        for range in ranges {
            guard range.count == 2 else { continue }
            let s = range[0], e = range[1]
            guard s >= 0, s < e, e <= text.count else { continue }
            let startIdx = text.index(text.startIndex, offsetBy: s)
            let endIdx = text.index(text.startIndex, offsetBy: e)
            if let attrRange = Range(startIdx..<endIdx, in: str) {
                str[attrRange].font = .system(size: 12, weight: .semibold, design: .monospaced)
                str[attrRange].foregroundColor = Color.accentColor
            }
        }
        return str
    }

    var body: some View {
        HStack(spacing: 0) {
            // Candidate text — fills available space, elides if needed
            Text(attributedCandidateText())
                .lineLimit(1)
                .truncationMode(.middle)
                .frame(maxWidth: .infinity, alignment: .leading)

            // Marginalia annotation — fixed width for vertical column alignment.
            // Marginalia pads each field (mode, size, perms) to fixed widths,
            // so a fixed annotation frame width aligns the internal columns.
            if !candidate.annotation.isEmpty {
                Text(candidate.annotation)
                    .font(.system(size: 10, design: .monospaced))
                    .foregroundStyle(isSelected ? .secondary : .tertiary)
                    .lineLimit(1)
                    .frame(width: annotationColumnWidth, alignment: .leading)
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 4)
        .background(
            RoundedRectangle(cornerRadius: 4)
                .fill(isSelected ? Color.accentColor.opacity(0.18) : Color.clear)
                .padding(.horizontal, 4)
        )
    }
}
