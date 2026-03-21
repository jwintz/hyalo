// GodModeState.swift - God-mode state enumeration
// Target: macOS 26 Tahoe with Liquid Glass design

/// Represents the current god-mode editing state.
///
/// God-mode translates single-key presses into modified key sequences.
/// The state determines which modifier is applied to the next key:
/// - `inactive`: god-mode is off, normal editing
/// - `control`: default god-mode state, keys produce C- commands
/// - `literal`: after SPC, next key is unmodified
/// - `meta`: after g, next key produces M- command
/// - `controlMeta`: after G, next key produces C-M- command
/// - `digitArgument`: numeric prefix argument in progress
/// - `universalArgument`: after u, equivalent to C-u prefix
@available(macOS 26.0, *)
public enum GodModeState: String, Sendable {
    case inactive
    case control
    case literal
    case meta
    case controlMeta = "control-meta"
    case digitArgument = "digit-argument"
    case universalArgument = "universal-argument"

    /// Short label for the toolbar pill.
    public var label: String {
        switch self {
        case .inactive:          return ""
        case .control:           return "C"
        case .literal:           return "SPC"
        case .meta:              return "M"
        case .controlMeta:       return "CM"
        case .digitArgument:     return "##"
        case .universalArgument: return "u"
        }
    }

    /// SF Symbol for the toolbar pill.
    public var icon: String {
        switch self {
        case .inactive:          return "command.square"
        case .control:           return "command.square.fill"
        case .literal:           return "character.cursor.ibeam"
        case .meta:              return "option"
        case .controlMeta:       return "command.square.fill"
        case .digitArgument:     return "number.square"
        case .universalArgument: return "u.square.fill"
        }
    }

    /// Whether this state represents an active god-mode modifier.
    public var isActive: Bool {
        self != .inactive
    }
}
