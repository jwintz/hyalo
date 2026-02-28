import Testing
@testable import HyaloShared

@Suite("FuzzyMatcher Tests")
struct FuzzyMatcherTests {

    @Test("Exact match returns true")
    func exactMatch() {
        #expect(FuzzyMatcher.matches(query: "hello", text: "hello"))
    }

    @Test("Subsequence match returns true")
    func subsequenceMatch() {
        #expect(FuzzyMatcher.matches(query: "hlo", text: "hello"))
    }

    @Test("Case insensitive match")
    func caseInsensitive() {
        #expect(FuzzyMatcher.matches(query: "HLO", text: "hello"))
    }

    @Test("Non-matching query returns false")
    func noMatch() {
        #expect(!FuzzyMatcher.matches(query: "xyz", text: "hello"))
    }

    @Test("Empty query matches anything")
    func emptyQuery() {
        #expect(FuzzyMatcher.matches(query: "", text: "hello"))
    }

    @Test("Exact match scores highest")
    func exactMatchScoreHighest() {
        let exactScore = FuzzyMatcher.score(query: "hello", text: "hello")
        let partialScore = FuzzyMatcher.score(query: "hlo", text: "hello")
        #expect(exactScore > partialScore)
    }

    @Test("Prefix match scores higher than non-prefix")
    func prefixScoresHigher() {
        let prefixScore = FuzzyMatcher.score(query: "hel", text: "hello")
        let midScore = FuzzyMatcher.score(query: "ell", text: "hello")
        #expect(prefixScore > midScore)
    }

    @Test("Non-matching query scores zero")
    func nonMatchScoreZero() {
        let score = FuzzyMatcher.score(query: "xyz", text: "hello")
        #expect(score == 0)
    }

    @Test("Filter sorts by score descending")
    func filterSortsByScore() {
        struct Item: FuzzyMatchable {
            let name: String
            var fuzzyMatchText: String { name }
        }
        let items = [
            Item(name: "application"),
            Item(name: "app"),
            Item(name: "approximate"),
        ]
        let filtered = FuzzyMatcher.filter(query: "app", items: items)
        #expect(filtered.count == 3)
        #expect(filtered[0].name == "app")
    }

    @Test("Filter excludes non-matching items")
    func filterExcludes() {
        struct Item: FuzzyMatchable {
            let name: String
            var fuzzyMatchText: String { name }
        }
        let items = [
            Item(name: "apple"),
            Item(name: "banana"),
            Item(name: "cherry"),
        ]
        let filtered = FuzzyMatcher.filter(query: "app", items: items)
        #expect(filtered.count == 1)
        #expect(filtered[0].name == "apple")
    }
}
