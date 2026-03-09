// FileInspectorView.swift - File metadata inspector
// Target: macOS 26 Tahoe with Liquid Glass design
// Uses Form with .formStyle(.grouped).

import SwiftUI

@available(macOS 26.0, *)
struct FileInspectorView: View {
    @Bindable var viewModel: InspectorViewModel

    var body: some View {
        Form {
            Section("Identity and Type") {
                LabeledContent("Name") {
                    HStack(spacing: 4) {
                        Image(systemName: FileTreeIcons.icon(for: viewModel.fileInfo.name))
                            .foregroundStyle(.secondary)
                            .font(.system(size: 11))
                        Text(viewModel.fileInfo.name)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                }
                LabeledContent("Type") {
                    Text(viewModel.fileInfo.type)
                        .lineLimit(1)
                }
            }

            Section("Location") {
                LabeledContent("Path") {
                    Text(viewModel.fileInfo.path)
                        .foregroundStyle(.secondary)
                        .lineLimit(2)
                        .truncationMode(.middle)
                        .font(.system(size: 11))
                }
            }

            Section("Attributes") {
                LabeledContent("Size") {
                    Text(viewModel.fileInfo.size)
                }
                LabeledContent("Created") {
                    Text(viewModel.fileInfo.created)
                }
                LabeledContent("Modified") {
                    Text(viewModel.fileInfo.modified)
                }
                LabeledContent("Permissions") {
                    Text(viewModel.fileInfo.permissions)
                }
            }

            Section("Text Settings") {
                LabeledContent("Encoding") {
                    Text(viewModel.fileInfo.encoding)
                }
                LabeledContent("Line Endings") {
                    Text(viewModel.fileInfo.lineEndings)
                }
                LabeledContent("Indentation") {
                    Text("\(viewModel.fileInfo.indentStyle): \(viewModel.fileInfo.indentWidth)")
                }
            }

            Section("Source Control") {
                LabeledContent("Status") {
                    Text(viewModel.fileInfo.gitStatus)
                }
                if let lastCommit = viewModel.fileInfo.lastCommit {
                    LabeledContent("Last Commit") {
                        Text(lastCommit)
                            .lineLimit(2)
                    }
                }
            }
        }
        .formStyle(.grouped)
        .scrollContentBackground(.hidden)
        .font(.system(size: 12))
    }
}
