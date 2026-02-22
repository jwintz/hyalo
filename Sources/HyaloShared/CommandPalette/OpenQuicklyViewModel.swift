// OpenQuicklyViewModel.swift - View model for Open Quickly
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d

import Foundation
import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
@Observable
public final class OpenQuicklyViewModel {
    public init() { }
    public var searchText: String = ""
    public var selectedItem: OpenQuicklyItem?
    public var items: [OpenQuicklyItem] = []
    public var filteredItems: [OpenQuicklyItem] = []

    public var onSelect: ((OpenQuicklyItem) -> Void)?
    public var onClose: (() -> Void)?

    // MARK: - Data Updates

    public func updateItems(_ newItems: [OpenQuicklyItem]) {
        items = newItems
        filterItems()
    }

    public func updateItems(from jsonData: Data) {
        do {
            let decoded = try JSONDecoder().decode([OpenQuicklyItem].self, from: jsonData)
            updateItems(decoded)
        } catch {
            NSLog("[Hyalo] Failed to decode Open Quickly items: \(error)")
        }
    }

    // MARK: - Filtering

    public func filterItems() {
        if searchText.isEmpty {
            filteredItems = items
            selectedItem = items.first
        } else {
            filteredItems = FuzzyMatcher.filter(query: searchText, items: items)
            selectedItem = filteredItems.first
        }
    }

    // MARK: - Selection

    public func selectItem(_ item: OpenQuicklyItem) {
        onSelect?(item)
    }

    public func selectCurrent() {
        if let item = selectedItem {
            selectItem(item)
        } else if let first = filteredItems.first {
            selectItem(first)
        }
    }

    // MARK: - Navigation

    public func selectNext() {
        guard let current = selectedItem,
              let index = filteredItems.firstIndex(where: { $0.id == current.id }),
              index < filteredItems.count - 1 else {
            selectedItem = filteredItems.first
            return
        }
        selectedItem = filteredItems[index + 1]
    }

    public func selectPrevious() {
        guard let current = selectedItem,
              let index = filteredItems.firstIndex(where: { $0.id == current.id }),
              index > 0 else {
            return
        }
        selectedItem = filteredItems[index - 1]
    }
}
