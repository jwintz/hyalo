// NavigatorComponents.swift - Shared navigator UI components
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Components used across navigator views: git status badges, file icons,
// highlighted text for filter matches.

import SwiftUI

// MARK: - Git Status Badge

@available(macOS 26.0, *)
struct GitStatusBadge: View {
    let status: String
    @Environment(\.colorTheme) private var theme

    var body: some View {
        let displayStatus = status == "?" ? "A" : status
        Text(displayStatus)
            .font(.system(size: 11, weight: .bold))
            .foregroundStyle(gitStatusColor)
    }

    private var gitStatusColor: Color {
        switch status {
        case "M": return theme.warning
        case "A": return theme.success
        case "D": return theme.error
        case "?": return theme.success
        case "R": return theme.link
        default: return .secondary
        }
    }
}

// MARK: - Highlighted Text Component

@available(macOS 26.0, *)
struct HighlightedText: View {
    let text: String
    let highlight: String
    let isActive: Bool
    @Environment(\.colorTheme) private var theme

    var body: some View {
        if highlight.isEmpty {
            Text(text)
                .foregroundColor(isActive ? theme.accent : .primary)
        } else {
            highlightedText
        }
    }

    @ViewBuilder
    private var highlightedText: some View {
        if let range = text.range(of: highlight, options: .caseInsensitive) {
            let before = String(text[text.startIndex..<range.lowerBound])
            let match = String(text[range])
            let after = String(text[range.upperBound...])

            HStack(spacing: 0) {
                if !before.isEmpty {
                    Text(before)
                        .foregroundStyle(.secondary)
                }
                Text(match)
                    .fontWeight(.bold)
                    .foregroundStyle(isActive ? theme.accent : .primary)
                if !after.isEmpty {
                    Text(after)
                        .foregroundStyle(.secondary)
                }
            }
        } else {
            Text(text)
                .foregroundStyle(.secondary)
        }
    }
}

// MARK: - File Icon Mapping (monochrome SF Symbols)

/// Maps file extensions to monochrome SF Symbol names.
enum FileTreeIcons {
    static let rootIcon = "folder.fill.badge.gearshape"

    static func icon(for fileName: String) -> String {
        let ext = (fileName as NSString).pathExtension.lowercased()
        switch ext {
        case "swift":           return "swift"
        case "el":              return "doc.text"
        case "json", "yml", "yaml": return "curlybraces"
        case "md", "txt":       return "doc.plaintext"
        case "html", "htm":     return "chevron.left.forwardslash.chevron.right"
        case "css":             return "curlybraces"
        case "js", "mjs":       return "doc.text"
        case "jsx", "tsx":      return "atom"
        case "ts":              return "doc.text"
        case "py":              return "doc.text"
        case "rb":              return "doc.text"
        case "rs":              return "doc.text"
        case "go":              return "doc.text"
        case "c", "h", "cpp", "hpp", "m", "mm":
                                return "doc.text"
        case "java", "kt":     return "doc.text"
        case "sh", "bash", "zsh": return "terminal"
        case "png", "jpg", "jpeg", "gif", "svg", "ico", "webp":
                                return "photo"
        case "pdf":             return "doc.text"
        case "mp3", "wav", "aif", "mid":
                                return "speaker.wave.2"
        case "mp4", "mov", "avi":
                                return "film"
        case "plist":           return "tablecells"
        case "xcconfig":        return "gearshape.2"
        case "lock":            return "lock.doc"
        case "gitignore":       return "arrow.triangle.branch"
        case "env":             return "gearshape"
        case "rtf":             return "doc.richtext"
        case "strings":         return "text.quote"
        default:                return "doc"
        }
    }
}
