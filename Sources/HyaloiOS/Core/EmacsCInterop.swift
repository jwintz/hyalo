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
