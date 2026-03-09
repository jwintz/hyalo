// DiagnosticsViewModel.swift - Diagnostic data model and view model
// Target: macOS 26 Tahoe with Liquid Glass design
// Data source: eglot + flymake diagnostics pushed from Emacs

import Foundation

// MARK: - Diagnostic Severity

public enum DiagnosticSeverity: String, Codable, Comparable {
    case error = "error"
    case warning = "warning"
    case note = "note"

    public var sortOrder: Int {
        switch self {
        case .error: return 0
        case .warning: return 1
        case .note: return 2
        }
    }

    public var systemImage: String {
        switch self {
        case .error: return "xmark.circle.fill"
        case .warning: return "exclamationmark.triangle.fill"
        case .note: return "info.circle.fill"
        }
    }

    public static func < (lhs: DiagnosticSeverity, rhs: DiagnosticSeverity) -> Bool {
        lhs.sortOrder < rhs.sortOrder
    }
}

// MARK: - Diagnostic Item

public struct DiagnosticItem: Codable, Identifiable, Hashable {
    public let id: String
    public let file: String
    public let line: Int
    public let column: Int
    public let severity: DiagnosticSeverity
    public let message: String
    public let source: String

    /// Short file name for display
    public var fileName: String {
        (file as NSString).lastPathComponent
    }

    /// Location string for display: "line:column"
    public var location: String {
        "\(line):\(column)"
    }
}

// MARK: - Diagnostics View Model

@available(macOS 26.0, *)
@MainActor
@Observable
public final class DiagnosticsViewModel {
    public var diagnostics: [DiagnosticItem] = []

    public var errorCount: Int {
        diagnostics.filter { $0.severity == .error }.count
    }

    public var warningCount: Int {
        diagnostics.filter { $0.severity == .warning }.count
    }

    public var noteCount: Int {
        diagnostics.filter { $0.severity == .note }.count
    }

    /// Diagnostics grouped by file, sorted by severity then line
    public var groupedByFile: [(file: String, items: [DiagnosticItem])] {
        let dict = Dictionary(grouping: diagnostics, by: \.file)
        return dict.map { (file: $0.key, items: $0.value.sorted { a, b in
            if a.severity != b.severity { return a.severity < b.severity }
            return a.line < b.line
        }) }
        .sorted { a, b in
            // Files with errors sort first, then warnings, then notes
            let aMax = a.items.first?.severity ?? .note
            let bMax = b.items.first?.severity ?? .note
            if aMax != bMax { return aMax < bMax }
            return a.file < b.file
        }
    }

    /// Navigation callback: file, line, column
    public var onNavigate: ((String, Int, Int) -> Void)?

    public func update(from data: Data) {
        do {
            let items = try JSONDecoder().decode([DiagnosticItem].self, from: data)
            diagnostics = items
        } catch {
        }
    }
}
