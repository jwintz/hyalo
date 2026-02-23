#if canImport(UIKit)
// EmacsCInterop.swift - C function declarations for libemacs on iOS
// These symbols are resolved at link time from libemacs.a

import Foundation

// MARK: - Emacs Entry Point

/// Initialize Emacs with command-line arguments and an optional portable dump file.
/// Called on a dedicated background thread with a 64MB stack.
@_silgen_name("ios_emacs_init")
func ios_emacs_init(
    _ argc: Int32,
    _ argv: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>,
    _ dump_file: UnsafePointer<CChar>?
) -> Int32

/// Get the build fingerprint for pdmp file matching.
@_silgen_name("ios_get_fingerprint")
func ios_get_fingerprint() -> UnsafeMutablePointer<CChar>?

// MARK: - Bootstrap Callbacks

/// Notify that bootstrap has started.
@_silgen_name("ios_notify_bootstrap_start")
func ios_notify_bootstrap_start()

/// Report file loading progress during bootstrap.
@_silgen_name("ios_report_load_progress")
func ios_report_load_progress(_ filename: UnsafePointer<CChar>)

/// Report that bootstrap is complete.
@_silgen_name("ios_report_bootstrap_complete")
func ios_report_bootstrap_complete()
// MARK: - Event Loop

/// Signal to Emacs that an event is available, waking the event loop.
@_silgen_name("ios_signal_event_available")
func ios_signal_event_available()

// MARK: - UIWindow Setup


// MARK: - Window System Initialization

/// Enable iOS window system to bypass tty check.
/// Must be set to true before ios_emacs_init to enable iOS window system.
@_silgen_name("ios_init_gui")
var ios_init_gui: CBool

@_silgen_name("ios_set_main_window")
func ios_set_main_window(_ window: AnyObject)
#endif // canImport(UIKit)
