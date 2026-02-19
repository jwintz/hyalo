// FuzzyMatcher.swift - Subsequence fuzzy matching with scoring
// Target: macOS 26 Tahoe with Liquid Glass design
// Proper fuzzy matcher: contiguous runs, word boundaries, prefix bonus.

import Foundation

// MARK: - Fuzzy Matchable Protocol

protocol FuzzyMatchable {
    var fuzzyMatchText: String { get }
}

// MARK: - Fuzzy Matcher

enum FuzzyMatcher {

    /// Returns true if all characters in query appear in text in order.
    static func matches(query: String, text: String) -> Bool {
        let queryLower = query.lowercased()
        let textLower = text.lowercased()

        var queryIndex = queryLower.startIndex
        var textIndex = textLower.startIndex

        while queryIndex < queryLower.endIndex && textIndex < textLower.endIndex {
            if queryLower[queryIndex] == textLower[textIndex] {
                queryIndex = queryLower.index(after: queryIndex)
            }
            textIndex = textLower.index(after: textIndex)
        }

        return queryIndex == queryLower.endIndex
    }

    /// Score a fuzzy match (higher = better). Returns 0 if no match.
    ///
    /// Scoring criteria:
    /// - Exact match: 10000
    /// - Prefix match: 5000 + length bonus
    /// - Substring match: 2000 + position bonus (earlier is better)
    /// - Subsequence match: sum of per-character bonuses:
    ///   - Contiguous run bonus: +20 per consecutive match
    ///   - Word boundary bonus: +30 (char after `-`, `_`, `/`, space, or start)
    ///   - Case match bonus: +5 (exact case)
    ///   - Early position bonus: +10 * (1 - position / textLength)
    static func score(query: String, text: String) -> Double {
        if query.isEmpty { return 1.0 }

        let queryChars = Array(query.lowercased())
        let textChars = Array(text.lowercased())
        let originalTextChars = Array(text)
        let originalQueryChars = Array(query)

        let qLen = queryChars.count
        let tLen = textChars.count

        guard tLen > 0 else { return 0.0 }

        // Exact match
        if queryChars == textChars { return 10000.0 }

        // Prefix match
        if tLen >= qLen && Array(textChars.prefix(qLen)) == queryChars {
            return 5000.0 + Double(qLen) / Double(tLen) * 100.0
        }

        // Substring match — find earliest contiguous occurrence
        if let substringStart = findSubstring(queryChars, in: textChars) {
            let positionBonus = Double(tLen - substringStart) / Double(tLen) * 100.0
            return 2000.0 + positionBonus
        }

        // Subsequence match with detailed scoring
        var totalScore: Double = 0.0
        var qi = 0
        var lastMatchIndex = -1
        var consecutiveCount = 0

        // Separators that create word boundaries
        let separators: Set<Character> = ["-", "_", "/", " ", "."]

        for ti in 0..<tLen {
            guard qi < qLen else { break }
            if textChars[ti] == queryChars[qi] {
                var charScore: Double = 10.0

                // Contiguous run bonus
                if lastMatchIndex == ti - 1 {
                    consecutiveCount += 1
                    charScore += Double(consecutiveCount) * 20.0
                } else {
                    consecutiveCount = 0
                }

                // Word boundary bonus: first char or after separator
                if ti == 0 || (ti > 0 && separators.contains(Character(String(originalTextChars[ti - 1])))) {
                    charScore += 30.0
                }

                // Case match bonus
                if qi < originalQueryChars.count && ti < originalTextChars.count
                    && originalQueryChars[qi] == originalTextChars[ti] {
                    charScore += 5.0
                }

                // Early position bonus
                charScore += 10.0 * (1.0 - Double(ti) / Double(tLen))

                totalScore += charScore
                lastMatchIndex = ti
                qi += 1
            }
        }

        // Did not match all query characters
        if qi < qLen { return 0.0 }

        // Normalize: penalize long texts relative to query length
        let lengthPenalty = Double(qLen) / Double(tLen)
        totalScore *= (0.5 + 0.5 * lengthPenalty)

        return totalScore
    }

    /// Find the start index of `needle` as a contiguous substring in `haystack`.
    private static func findSubstring(_ needle: [Character], in haystack: [Character]) -> Int? {
        let nLen = needle.count
        let hLen = haystack.count
        guard hLen >= nLen else { return nil }
        for i in 0...(hLen - nLen) {
            var found = true
            for j in 0..<nLen {
                if haystack[i + j] != needle[j] {
                    found = false
                    break
                }
            }
            if found { return i }
        }
        return nil
    }

    /// Filter and sort items using fuzzy matching.
    static func filter<T: FuzzyMatchable>(query: String, items: [T]) -> [T] {
        if query.isEmpty { return items }

        let scored = items.compactMap { item -> (T, Double)? in
            let s = score(query: query, text: item.fuzzyMatchText)
            guard s > 0 else { return nil }
            return (item, s)
        }

        return scored.sorted { $0.1 > $1.1 }.map(\.0)
    }
}
