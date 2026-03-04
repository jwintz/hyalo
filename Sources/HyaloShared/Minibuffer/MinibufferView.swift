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

    public var body: some View {
        VStack(spacing: 0) {
            // Prompt + input
            HStack(alignment: .center, spacing: 0) {
                if !viewModel.prompt.isEmpty {
                    Text(viewModel.prompt)
                        .font(.system(size: 16, design: .monospaced))
                        .foregroundColor(.secondary)
                        .padding(.leading, 12)
                        .lineLimit(1)
                }

                TextField("", text: $viewModel.input)
                    .font(.system(size: 20, weight: .light))
                    .textFieldStyle(.plain)
                    .focused($isInputFocused)
                    .accessibilityLabel("Minibuffer input")
            }
            .padding(.vertical, 12)
            .foregroundColor(.primary.opacity(0.85))

            Divider()

            // Candidate list
            if viewModel.candidates.isEmpty && !viewModel.input.isEmpty {
                Text("No matching candidates")
                    .font(.system(size: 14))
                    .foregroundColor(.secondary)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else if !viewModel.candidates.isEmpty {
                ScrollViewReader { proxy in
                    List(Array(viewModel.candidates.enumerated()), id: \.element.id) { index, candidate in
                        MinibufferCandidateRow(
                            candidate: candidate,
                            isSelected: index == viewModel.selectedIndex
                        )
                        .id(candidate.id)
                        .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                        .contentShape(Rectangle())
                        .onTapGesture {
                            viewModel.onCandidateSelected?(index)
                        }
                    }
                    .listStyle(.plain)
                    .scrollContentBackground(.hidden)
                    .environment(\.defaultMinListRowHeight, 32)
                    .onChange(of: viewModel.selectedIndex) { _, newIndex in
                        if newIndex >= 0 && newIndex < viewModel.candidates.count {
                            withAnimation {
                                proxy.scrollTo(viewModel.candidates[newIndex].id, anchor: .center)
                            }
                        }
                    }
                }
            }
        }
        .background(panelBackground)
        .edgesIgnoringSafeArea(.vertical)
        .frame(minWidth: 680, minHeight: 400, maxHeight: .infinity)
        .onChange(of: viewModel.input) { _, newValue in
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

    var body: some View {
        HStack(spacing: 8) {
            Text(candidate.text)
                .font(.system(size: 12, weight: isSelected ? .medium : .regular))
                .foregroundStyle(.primary)
                .lineLimit(1)

            Spacer()

            if !candidate.annotation.isEmpty {
                Text(candidate.annotation)
                    .font(.system(size: 10))
                    .foregroundStyle(.tertiary)
                    .lineLimit(1)
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 5)
        .background(isSelected ? Color.accentColor.opacity(0.15) : Color.clear)
    }
}
