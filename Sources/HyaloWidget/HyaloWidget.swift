// HyaloWidget.swift - WidgetKit entry point for the Hyalo desktop widget
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Configuration:
// This widget extension requires an app extension target in the Xcode project.
// It cannot be built as a standalone SPM target because WidgetKit extensions
// must be embedded in an app bundle.
//
// See WIDGET_SETUP.md for project configuration instructions.

import SwiftUI
import WidgetKit

// MARK: - Widget Definition

@available(macOS 26.0, *)
struct HyaloWidget: Widget {
    let kind = "org.gnu.hyalo.widget"

    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: HyaloTimelineProvider()) { entry in
            HyaloWidgetEntryView(entry: entry)
                .containerBackground(.ultraThinMaterial, for: .widget)
        }
        .configurationDisplayName("Emacs Status")
        .description("Live status of running Emacs instances.")
        .supportedFamilies([.systemSmall, .systemMedium, .systemLarge, .systemExtraLarge])
    }
}

// MARK: - Entry View (family dispatch)

@available(macOS 26.0, *)
struct HyaloWidgetEntryView: View {
    @Environment(\.widgetFamily) var family
    let entry: HyaloWidgetEntry

    var body: some View {
        switch family {
        case .systemSmall:
            HyaloWidgetSmallView(data: entry.data)
        case .systemMedium:
            HyaloWidgetMediumView(data: entry.data)
        case .systemLarge:
            HyaloWidgetLargeView(data: entry.data)
        case .systemExtraLarge:
            HyaloWidgetExtraLargeView(data: entry.data)
        default:
            HyaloWidgetMediumView(data: entry.data)
        }
    }
}

// MARK: - Widget Bundle

@available(macOS 26.0, *)
@main
struct HyaloWidgetBundle: WidgetBundle {
    var body: some Widget {
        HyaloWidget()
    }
}

// MARK: - Previews

#if DEBUG
@available(macOS 26.0, *)
#Preview("Small", as: .systemSmall) {
    HyaloWidget()
} timeline: {
    HyaloWidgetEntry(date: .now, data: .previewMultiple)
}

@available(macOS 26.0, *)
#Preview("Medium", as: .systemMedium) {
    HyaloWidget()
} timeline: {
    HyaloWidgetEntry(date: .now, data: .previewMultiple)
}

@available(macOS 26.0, *)
#Preview("Large", as: .systemLarge) {
    HyaloWidget()
} timeline: {
    HyaloWidgetEntry(date: .now, data: .previewMultiple)
}

@available(macOS 26.0, *)
#Preview("Extra Large", as: .systemExtraLarge) {
    HyaloWidget()
} timeline: {
    HyaloWidgetEntry(date: .now, data: .previewMultiple)
}

@available(macOS 26.0, *)
#Preview("Empty", as: .systemMedium) {
    HyaloWidget()
} timeline: {
    HyaloWidgetEntry(date: .now, data: HyaloWidgetData())
}
#endif
