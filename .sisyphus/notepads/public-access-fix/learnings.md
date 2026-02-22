## Learnings from public access fix
- Made HyaloShared types public to satisfy HyaloMac imports for cross-module visibility:
  - CommandPaletteViewModel, OpenQuicklyViewModel: public classes with public initializers
  - CommandPaletteView, OpenQuicklyView: public SwiftUI views
  - NavigatorManager: public final class
- Resolved Swift visibility issues by adding explicit public init() for public classes that lacked explicit initializers
- Verified HyaloShared builds in isolation via: swift build --target HyaloShared
- Next steps: attempt full project build to ensure no cross-module access regressions; address further visibility across additional MacOS/Kit modules if errors persist

Evidence: HyaloShared target builds successfully; see the build log for target HyaloShared.
