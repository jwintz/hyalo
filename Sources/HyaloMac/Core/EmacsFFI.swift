// EmacsFFI.swift - Emacs C interop: dlsym wrappers and window finders
// Target: macOS 26 Tahoe

import AppKit
import EmacsSwiftModule

// C functions in Emacs binary — resolved via dlsym since Hyalo is
// loaded as a dynamic module into the Emacs process.

// Sets fringe alpha override and forces full redisplay of all NS frames.
let ns_set_fringe_alpha_override: (@convention(c) (Double) -> Void)? = {
    guard let sym = dlsym(dlopen(nil, RTLD_LAZY), "ns_set_fringe_alpha_override") else {
        return nil
    }
    return unsafeBitCast(sym, to: (@convention(c) (Double) -> Void).self)
}()

// Forces full redisplay of all NS frames (renders into IOSurface
// and flushes to VRAM).  Used when changes must be visible during
// event tracking (e.g. appearance mode switch via picker click).
let ns_force_redisplay: (@convention(c) () -> Void)? = {
    guard let sym = dlsym(dlopen(nil, RTLD_LAZY), "ns_force_redisplay") else {
        return nil
    }
    return unsafeBitCast(sym, to: (@convention(c) () -> Void).self)
}()

// Wakes the Emacs event loop by posting an NSApplicationDefined
// event.  Causes [NSApp run] to return and process pending I/O,
// including pipe process data from Swift channels.
let ns_wake_emacs: (@convention(c) () -> Void)? = {
    guard let sym = dlsym(dlopen(nil, RTLD_LAZY), "ns_wake_emacs") else {
        return nil
    }
    return unsafeBitCast(sym, to: (@convention(c) () -> Void).self)
}()

/// Find the main Emacs window (not child-frames).
/// Child-frames are positioned off-screen at x < -5000.
func findEmacsWindow() -> NSWindow? {
    func isOnScreen(_ window: NSWindow) -> Bool {
        window.frame.origin.x > -5000
    }

    if let window = NSApp.mainWindow, isOnScreen(window) { return window }
    if let window = NSApp.keyWindow, isOnScreen(window) { return window }

    for window in NSApp.windows where window.isVisible && !window.isMiniaturized && isOnScreen(window) {
        let className = String(describing: type(of: window))
        if className.contains("EmacsWindow") { return window }
    }

    for window in NSApp.windows where isOnScreen(window) {
        let className = String(describing: type(of: window))
        if className.contains("EmacsWindow") { return window }
    }

    return nil
}

/// Find all on-screen EmacsWindows (not child-frames).
func findAllEmacsWindows() -> [NSWindow] {
    NSApp.windows.filter { window in
        window.frame.origin.x > -5000
        && String(describing: type(of: window)).contains("EmacsWindow")
    }
}

/// Find the first undecorated EmacsWindow (not yet in the reverse map).
@available(macOS 26.0, *)
func findUndecoratedEmacsWindow() -> NSWindow? {
    for window in findAllEmacsWindows() {
        if HyaloModule.windowToFrameId[ObjectIdentifier(window)] == nil {
            return window
        }
    }
    return nil
}

/// Extract the Emacs NSView from the window's content view hierarchy.
func extractEmacsView(from contentView: NSView) -> NSView? {
    let className = String(describing: type(of: contentView))
    if className.contains("EmacsView") { return contentView }

    for subview in contentView.subviews {
        let subClassName = String(describing: type(of: subview))
        if subClassName.contains("EmacsView") || subview.acceptsFirstResponder {
            return subview
        }
        if let found = extractEmacsView(from: subview) {
            return found
        }
    }

    return nil
}

// MARK: - Module Factory

func createModule() -> Module {
    HyaloModule()
}

// MARK: - NSColor Hex Extension

extension NSColor {
    convenience init?(hexString: String) {
        var hex = hexString.trimmingCharacters(in: .whitespacesAndNewlines)
        if hex.hasPrefix("#") { hex.removeFirst() }
        guard hex.count == 6 else { return nil }
        var rgb: UInt64 = 0
        guard Scanner(string: hex).scanHexInt64(&rgb) else { return nil }
        self.init(
            red: CGFloat((rgb >> 16) & 0xFF) / 255.0,
            green: CGFloat((rgb >> 8) & 0xFF) / 255.0,
            blue: CGFloat(rgb & 0xFF) / 255.0,
            alpha: 1.0
        )
    }
}
