import Testing
@testable import HyaloShared

@Suite("DateFormatting Tests")
struct DateFormattingTests {

    @Test("Parse standard ISO 8601 date")
    func parseISO8601Standard() {
        let date = DateFormatting.parseISO8601("2025-01-15T10:30:00Z")
        #expect(date != nil)
    }

    @Test("Parse ISO 8601 with fractional seconds")
    func parseISO8601Fractional() {
        let date = DateFormatting.parseISO8601("2025-01-15T10:30:00.123Z")
        #expect(date != nil)
    }

    @Test("Parse returns nil for invalid string")
    func parseISO8601Invalid() {
        let date = DateFormatting.parseISO8601("not-a-date")
        #expect(date == nil)
    }

    @Test("Parse full ISO 8601 format")
    func parseISO8601Full() {
        let date = DateFormatting.parseISO8601Full("2025-01-15T10:30:00+0000")
        #expect(date != nil)
    }

    @Test("Relative date returns non-empty string for valid date")
    func relativeDateValid() {
        let result = DateFormatting.relativeDate(from: "2025-01-15T10:30:00Z")
        #expect(!result.isEmpty)
    }

    @Test("Relative date falls back for invalid date")
    func relativeDateFallback() {
        let result = DateFormatting.relativeDate(from: "not-a-date-but-long-enough")
        #expect(result == "not-a-date-but-l")
    }

    @Test("Relative date truncates short invalid input")
    func relativeDateShortFallback() {
        let result = DateFormatting.relativeDate(from: "short")
        #expect(result == "short")
    }
}
