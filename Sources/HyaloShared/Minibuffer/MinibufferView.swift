// MinibufferView.swift - Native Swift panel for Emacs minibuffer
// Prompt + TextField on top, candidate list on bottom.
// Keyboard navigation handled by SearchPanel's NSEvent monitor (macOS).

import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
public struct MinibufferView: View {
    @Bindable public var viewModel: MinibufferViewModel

    @FocusState private var isInputFocused: Bool

    @ViewBuilder private var panelBackground: some View {
#if os(macOS)
        EffectView(.sidebar, blendingMode: .behindWindow)
#else
        EffectView()
#endif
    }

    public init(viewModel: MinibufferViewModel) {
        self.viewModel = viewModel
    }

    private var hasCandidates: Bool {
        !viewModel.candidates.isEmpty || viewModel.totalCandidates > 0
    }

    public var body: some View {
        VStack(spacing: 0) {
            // Prompt + input
            HStack(alignment: .center, spacing: 0) {
                if !viewModel.prompt.isEmpty {
                    Text(viewModel.prompt)
                        .font(.system(size: 14, design: .monospaced))
                        .foregroundStyle(.secondary)
                        .padding(.leading, 12)
                        .lineLimit(1)
                }

                TextField("", text: $viewModel.input)
                    .font(.system(size: 18, weight: .light))
                    .textFieldStyle(.plain)
                    .focused($isInputFocused)
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
            maxHeight: .infinity
        )
        .onChange(of: viewModel.input) { _, newValue in
            guard viewModel.shouldFireInputCallback else { return }
            viewModel.onInputChanged?(newValue)
        }
        .onAppear {
            DispatchQueue.main.async {
                isInputFocused = true
            }
        }
    }
}

// MARK: - Candidate Row

@available(macOS 26.0, iOS 26.0, *)
struct MinibufferCandidateRow: View {
    let candidate: MinibufferCandidate
    let isSelected: Bool
    let annotationColumnChars: Int

    // Approximate width per character for 10pt monospaced system font (annotation)
    private static let annotationCharWidth: CGFloat = 6.02

    private var annotationColumnWidth: CGFloat {
        CGFloat(annotationColumnChars) * Self.annotationCharWidth
    }

    var body: some View {
        HStack(spacing: 0) {
            // Candidate text — fills available space, elides if needed
            Text(candidate.text)
                .font(.system(size: 12, weight: isSelected ? .semibold : .regular, design: .monospaced))
                .foregroundStyle(.primary)
                .opacity(isSelected ? 1.0 : 0.85)
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
