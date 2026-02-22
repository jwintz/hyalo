// EmacsLifecycle.swift - Emacs thread management for iOS
// Manages the background Emacs thread and environment setup.

import Foundation
import SwiftUI

/// Emacs lifecycle state machine.
@available(iOS 26.0, *)
@MainActor
@Observable
final class EmacsLifecycle {

    enum State: Sendable {
        case idle
        case starting
        case bootstrapping(progress: String)
        case running
        case failed(String)
    }

    private(set) var state: State = .idle

    private var emacsThread: Thread?

    // MARK: - Environment Setup

    /// Set environment variables that Emacs expects on iOS.
    func setupEnvironment() {
        let docsPath = NSSearchPathForDirectoriesInDomains(
            .documentDirectory, .userDomainMask, true
        )[0]
        let cachesPath = NSSearchPathForDirectoriesInDomains(
            .cachesDirectory, .userDomainMask, true
        )[0]
        let bundlePath = Bundle.main.bundlePath

        setenv("HOME", docsPath, 1)
        setenv("XDG_CONFIG_HOME", "\(docsPath)/emacs", 1)
        setenv("EMACSLOADPATH", "\(bundlePath)/lisp", 1)
        setenv("EMACSDATA", "\(cachesPath)/emacs/data", 1)
        setenv("EMACSDOC", "\(bundlePath)/etc", 1)
    }

    // MARK: - Thread Management

    /// Start Emacs on a dedicated background thread.
    func start() {
        guard case .idle = state else { return }
        state = .starting

        setupEnvironment()

        let bundlePath = Bundle.main.bundlePath
        let docsPath = NSSearchPathForDirectoriesInDomains(
            .documentDirectory, .userDomainMask, true
        )[0]

        emacsThread = Thread { [weak self] in
            // Resolve portable dump file
            let dumpFile: String? = {
                guard let fp = ios_get_fingerprint() else { return nil }
                let fingerprint = String(cString: fp)
                let path = "\(docsPath)/emacs-\(fingerprint).pdmp"
                if FileManager.default.fileExists(atPath: path) {
                    return path
                }
                // Fall back to bundled dump
                let bundled = "\(bundlePath)/bootstrap-emacs.pdmp"
                if FileManager.default.fileExists(atPath: bundled) {
                    return bundled
                }
                return nil
            }()

            // Build argv
            var args: [UnsafeMutablePointer<CChar>?] = [
                strdup("emacs"),
                strdup("--init-directory"),
                strdup("\(bundlePath)/init"),
                strdup("--debug-init"),
                nil
            ]

            let result: Int32
            if let dumpFile {
                result = dumpFile.withCString { dump in
                    ios_emacs_init(Int32(args.count - 1), &args, dump)
                }
            } else {
                result = ios_emacs_init(Int32(args.count - 1), &args, nil)
            }

            // Free argv
            for arg in args { free(arg) }

            DispatchQueue.main.async {
                if result == 0 {
                    self?.state = .running
                } else {
                    self?.state = .failed("Emacs init returned \(result)")
                }
            }
        }

        emacsThread?.stackSize = 64 * 1024 * 1024 // 64MB
        emacsThread?.qualityOfService = .userInitiated
        emacsThread?.name = "emacs-main"
        emacsThread?.start()
    }
}
