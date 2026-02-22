import SwiftUI

public enum HyaloDesign {
    public enum CornerRadius {
        public static let capsule: CGFloat = 14
        public static let glass: CGFloat = 18
        public static let content: CGFloat = 14
        public static let menu: CGFloat = 12
        public static let small: CGFloat = 6
    }

    public enum Padding {
        public static let horizontal: CGFloat = 12
        public static let outer: CGFloat = 16
        public static let sidebar: CGFloat = 14
        public static let compact: CGFloat = 8
        public static let section: CGFloat = 16
    }

    public enum Height {
        public static let modeLine: CGFloat = 24
        public static let headerLine: CGFloat = 22
        public static let toolbar: CGFloat = 28
        public static let tabBar: CGFloat = 27
        public static let statusBar: CGFloat = 28
    }

    public enum Width {
        public static let sidebarToggle: CGFloat = 47
        public static let sidebarMin: CGFloat = 200
        public static let sidebarIdeal: CGFloat = 280
        public static let sidebarMax: CGFloat = 400
        public static let inspectorMin: CGFloat = 300
        public static let inspectorIdeal: CGFloat = 400
        public static let inspectorMax: CGFloat = 500
    }

    public enum Spacing {
        public static let tight: CGFloat = 4
        public static let compact: CGFloat = 8
        public static let standard: CGFloat = 12
        public static let comfortable: CGFloat = 16
        public static let generous: CGFloat = 24
    }

    public enum FontSize {
        public static let small: CGFloat = 9
        public static let caption: CGFloat = 10
        public static let body: CGFloat = 11
        public static let emphasized: CGFloat = 12
        public static let large: CGFloat = 13
    }

    public enum IconSize {
        public static let small: CGFloat = 10
        public static let medium: CGFloat = 12
        public static let standard: CGFloat = 14
        public static let large: CGFloat = 28
    }

    public enum Animation {
        public static let instant: Double = 0.0
        public static let quick: Double = 0.1
        public static let standard: Double = 0.25
        public static let slow: Double = 0.3
    }
}
