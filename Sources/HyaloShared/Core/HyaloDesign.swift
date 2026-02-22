import SwiftUI

enum HyaloDesign {
    enum CornerRadius {
        static let capsule: CGFloat = 14
        static let glass: CGFloat = 18
        static let content: CGFloat = 14
        static let menu: CGFloat = 12
        static let small: CGFloat = 6
    }

    enum Padding {
        static let horizontal: CGFloat = 12
        static let outer: CGFloat = 16
        static let sidebar: CGFloat = 14
        static let compact: CGFloat = 8
        static let section: CGFloat = 16
    }

    enum Height {
        static let modeLine: CGFloat = 24
        static let headerLine: CGFloat = 22
        static let toolbar: CGFloat = 28
        static let tabBar: CGFloat = 27
        static let statusBar: CGFloat = 28
    }

    enum Width {
        static let sidebarToggle: CGFloat = 47
        static let sidebarMin: CGFloat = 200
        static let sidebarIdeal: CGFloat = 280
        static let sidebarMax: CGFloat = 400
        static let inspectorMin: CGFloat = 300
        static let inspectorIdeal: CGFloat = 400
        static let inspectorMax: CGFloat = 500
    }

    enum Spacing {
        static let tight: CGFloat = 4
        static let compact: CGFloat = 8
        static let standard: CGFloat = 12
        static let comfortable: CGFloat = 16
        static let generous: CGFloat = 24
    }

    enum FontSize {
        static let small: CGFloat = 9
        static let caption: CGFloat = 10
        static let body: CGFloat = 11
        static let emphasized: CGFloat = 12
        static let large: CGFloat = 13
    }

    enum IconSize {
        static let small: CGFloat = 10
        static let medium: CGFloat = 12
        static let standard: CGFloat = 14
        static let large: CGFloat = 28
    }

    enum Animation {
        static let instant: Double = 0.0
        static let quick: Double = 0.1
        static let standard: Double = 0.25
        static let slow: Double = 0.3
    }
}
