// OpenQuicklyViewModel.swift - View model for Open Quickly
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d

import Foundation
import SwiftUI

@available(macOS 26.0, *)
@Observable
final class OpenQuicklyViewModel {
    var searchText: String = ""
    var selectedItem: OpenQuicklyItem?
    var items: [OpenQuicklyItem] = []
    var filteredItems: [OpenQuicklyItem] = []

    var onSelect: ((OpenQuicklyItem) -> Void)?
    var onClose: (() -> Void)?

    // MARK: - Data Updates

    func updateItems(_ newItems: [OpenQuicklyItem]) {
        items = newItems
        filterItems()
    }

    func updateItems(from jsonData: Data) {
        do {
            let decoded = try JSONDecoder().decode([OpenQuicklyItem].self, from: jsonData)
            updateItems(decoded)
        } catch {
            NSLog("[Hyalo] Failed to decode Open Quickly items: \(error)")
        }
    }

    // MARK: - Filtering

    func filterItems() {
        if searchText.isEmpty {
            filteredItems = items
            selectedItem = items.first
        } else {
            filteredItems = FuzzyMatcher.filter(query: searchText, items: items)
            selectedItem = filteredItems.first
        }
    }

    // MARK: - Selection

    func selectItem(_ item: OpenQuicklyItem) {
        onSelect?(item)
    }

    func selectCurrent() {
        if let item = selectedItem {
            selectItem(item)
        } else if let first = filteredItems.first {
            selectItem(first)
        }
    }

    // MARK: - Navigation

    func selectNext() {
        guard let current = selectedItem,
              let index = filteredItems.firstIndex(where: { $0.id == current.id }),
              index < filteredItems.count - 1 else {
            selectedItem = filteredItems.first
            return
        }
        selectedItem = filteredItems[index + 1]
    }

    func selectPrevious() {
        guard let current = selectedItem,
              let index = filteredItems.firstIndex(where: { $0.id == current.id }),
              index > 0 else {
            return
        }
        selectedItem = filteredItems[index - 1]
    }
}
