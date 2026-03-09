// UtilityAreaViewModelExtension.swift - macOS-only terminalHolder on UtilityAreaViewModel
// Target: macOS 26 Tahoe with Liquid Glass design
//
// UtilityAreaViewModel is a final @Observable class defined in HyaloShared.
// Stored properties cannot be added via Swift extension on a final class.
// We use objc_setAssociatedObject to attach the holder as a side-table value.

import Foundation
import ObjectiveC
import HyaloShared

private nonisolated(unsafe) var terminalHolderKey: UInt8 = 0

@available(macOS 26.0, *)
extension UtilityAreaViewModel {
    @MainActor
    var terminalHolder: UtilityAreaTerminalHolder {
        if let existing = objc_getAssociatedObject(self, &terminalHolderKey) as? UtilityAreaTerminalHolder {
            return existing
        }
        let holder = UtilityAreaTerminalHolder()
        objc_setAssociatedObject(self, &terminalHolderKey, holder, .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
        return holder
    }
}
