// Module+Profiling.swift - OSSignposter integration for Instruments
// Target: macOS 26 Tahoe
//
// Exposes hyalo-signpost-begin/end to elisp so init phases appear
// as signpost intervals in Instruments Time Profiler.

import os
import EmacsSwiftModule

private let signposter = OSSignposter(subsystem: "hyalo", category: "init")
private var activeIntervals: [String: OSSignpostIntervalState] = [:]

extension HyaloModule {

    func setupProfilingBindings(_ env: EmacsSwiftModule.Environment) throws {

        try env.defun("hyalo-signpost-begin",
            with: "Begin an OSSignposter interval with LABEL."
        ) { (_: EmacsSwiftModule.Environment, label: String) -> Bool in
            let id = signposter.makeSignpostID()
            let state = signposter.beginInterval("init-phase", id: id, "\(label)")
            activeIntervals[label] = state
            return true
        }

        try env.defun("hyalo-signpost-end",
            with: "End the OSSignposter interval for LABEL."
        ) { (_: EmacsSwiftModule.Environment, label: String) -> Bool in
            if let state = activeIntervals.removeValue(forKey: label) {
                signposter.endInterval("init-phase", state, "\(label)")
            }
            return true
        }
    }
}
