// HyaloTimelineProvider.swift - WidgetKit timeline provider for Hyalo
// Target: macOS 26 Tahoe
// Reads Emacs instance data from App Groups shared container

import WidgetKit

// MARK: - Timeline Entry

struct HyaloWidgetEntry: TimelineEntry {
    let date: Date
    let data: HyaloWidgetData
}

// MARK: - Timeline Provider

struct HyaloTimelineProvider: TimelineProvider {

    func placeholder(in context: Context) -> HyaloWidgetEntry {
        HyaloWidgetEntry(date: .now, data: .placeholder)
    }

    func getSnapshot(in context: Context, completion: @escaping (HyaloWidgetEntry) -> Void) {
        let data = context.isPreview ? .previewMultiple : HyaloWidgetStore.read()
        completion(HyaloWidgetEntry(date: .now, data: data))
    }

    func getTimeline(in context: Context, completion: @escaping (Timeline<HyaloWidgetEntry>) -> Void) {
        let data = HyaloWidgetStore.read()
        let entry = HyaloWidgetEntry(date: .now, data: data)
        // Refresh every 30 seconds to keep process stats current
        let nextUpdate = Calendar.current.date(byAdding: .second, value: 30, to: .now)!
        let timeline = Timeline(entries: [entry], policy: .after(nextUpdate))
        completion(timeline)
    }
}
