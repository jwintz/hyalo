// DateFormatting.swift - Shared date formatting utilities
// Target: macOS 26 Tahoe / iOS 26

import Foundation

public enum DateFormatting {
    /// Cached ISO 8601 formatter (with fractional seconds)
    private static let iso8601: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return f
    }()

    /// Cached ISO 8601 formatter (without fractional seconds, fallback)
    private static let iso8601NoFrac: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime]
        return f
    }()

    /// Cached ISO 8601 formatter for full date/time/timezone (package timestamps)
    private static let iso8601Full: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [
            .withFullDate, .withTime,
            .withDashSeparatorInDate, .withColonSeparatorInTime,
            .withTimeZone
        ]
        return f
    }()

    /// Cached relative formatter
    private static let relative: RelativeDateTimeFormatter = {
        let f = RelativeDateTimeFormatter()
        f.unitsStyle = .abbreviated
        return f
    }()

    /// Parse an ISO 8601 date string, trying fractional seconds first.
    public static func parseISO8601(_ isoDate: String) -> Date? {
        iso8601.date(from: isoDate) ?? iso8601NoFrac.date(from: isoDate)
    }

    /// Parse a full ISO 8601 date string (with timezone, no fractional seconds).
    public static func parseISO8601Full(_ isoDate: String) -> Date? {
        iso8601Full.date(from: isoDate)
    }

    /// Convert an ISO date string to an abbreviated relative format (e.g. "2h ago").
    public static func relativeDate(from isoDate: String) -> String {
        if let date = parseISO8601(isoDate) {
            return relative.localizedString(for: date, relativeTo: Date())
        }
        return String(isoDate.prefix(16))
    }
}
