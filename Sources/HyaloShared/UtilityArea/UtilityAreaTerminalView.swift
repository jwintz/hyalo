// UtilityAreaTerminalView.swift - Terminal view stub for iOS
// macOS implementation lives in HyaloMac/UtilityArea/UtilityAreaTerminalView.swift
// and HyaloMac/Inspector/InspectorTerminalView.swift.

import SwiftUI

#if os(iOS)
@available(iOS 26.0, *)
public struct UtilityAreaTerminalView: View {
    public init() {}
    public var body: some View { EmptyView() }
}

@available(iOS 26.0, *)
public class UtilityAreaTerminalHolder: ObservableObject {
    public init() {}
}
#endif
