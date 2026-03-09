# Hyalo Widget Setup

The desktop widget lives in `Sources/HyaloWidget/` and shows live Emacs instance status. Because WidgetKit extensions must be embedded in an app bundle, the widget **cannot** be a standalone SPM target — it requires an Xcode app extension target.

## Files Created

| File | Purpose |
|------|---------|
| `HyaloWidgetData.swift` | `HyaloWidgetData` / `HyaloInstanceData` models + App Groups read/write |
| `HyaloTimelineProvider.swift` | `TimelineProvider` that reads from shared container |
| `HyaloWidgetViews.swift` | SwiftUI views for small / medium / large families |
| `HyaloWidget.swift` | `@main` widget bundle entry point + previews |
| `HyaloWidgetDataPublisher.swift` | Main app helper that publishes Emacs state to the widget |

## Project Configuration Required

### 1. Add App Group Entitlement

Both the main app target and widget extension must share the App Group:

```
group.org.gnu.hyalo
```

Add to both targets' `.entitlements` files:
```xml
<key>com.apple.security.application-groups</key>
<array>
    <string>group.org.gnu.hyalo</string>
</array>
```

### 2. Create Widget Extension Target

In `iOS/project.yml` (XcodeGen), add:

```yaml
targets:
  HyaloWidget:
    type: appex
    platform: macOS
    deploymentTarget: "26.0"
    sources:
      - path: ../Sources/HyaloWidget
    settings:
      base:
        PRODUCT_BUNDLE_IDENTIFIER: org.gnu.hyalo.widget
        INFOPLIST_KEY_NSExtension_NSExtensionPointIdentifier: com.apple.widgetkit-extension
        GENERATE_INFOPLIST_FILE: YES
        SWIFT_VERSION: 6.0
        LD_RUNPATH_SEARCH_PATHS: "$(inherited) @executable_path/../Frameworks @executable_path/../../../../Frameworks"
    dependencies:
      - sdk: WidgetKit.framework
      - sdk: SwiftUI.framework
    entitlements:
      path: HyaloWidget.entitlements
      properties:
        com.apple.security.application-groups:
          - group.org.gnu.hyalo
```

And embed the widget in the main app target:

```yaml
targets:
  HyaloApp:
    dependencies:
      - target: HyaloWidget
        embed: true
```

### 3. Wire Up the Publisher (Main App)

In the main app's initialization (e.g., after Emacs channel setup), start the publisher:

```swift
HyaloWidgetDataPublisher.shared.start()
```

When Emacs channels push state updates, update the metadata:

```swift
let publisher = HyaloWidgetDataPublisher.shared
publisher.instanceMetadata["frame-1"] = .init(
    pid: emacsProcess.processIdentifier,
    startDate: launchDate,
    bufferCount: 12,
    currentBuffer: "init.el",
    currentDirectory: "/Users/jwintz/Syntropment/hyalo",
    gitBranch: "main",
    majorMode: "emacs-lisp-mode",
    recentFiles: ["init.el", "Package.swift", "README.md"]
)
```

The publisher auto-publishes every 15 seconds. Call `publish()` manually after significant state changes for immediate widget updates.
