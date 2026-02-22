# Swift Module Documentation Template

A Swift module doc page lives under `vault/4.swift/N.module-name.md`. It follows Apple documentation style — inspired by [developer.apple.com/documentation/swiftui/navigationsplitview](https://developer.apple.com/documentation/swiftui/navigationsplitview).

## Frontmatter

```yaml
---
title: ModuleName
description: One-line description
navigation:
  icon: i-lucide-<icon>
order: N
---
```

## Page Structure

```markdown
## Overview

1–3 sentences describing the module's role in the architecture. Mention key types and their relationships.

## Key Types

### `TypeName`
`struct` | `class` | `enum` | `protocol` | `@Observable class`

Description of what the type does and when it is used.

**Properties**

| Property | Type | Description |
|----------|------|-------------|
| `propertyName` | `String` | What it holds |

**Key Methods**

- `methodName(_:)` — Description of what the method does
- `setup()` — Called once during window controller initialization

**Example**

```swift
// Example showing primary usage
let vm = SomeViewModel()
vm.setup()
```

## Design Notes

Key architectural constraints, patterns, or decisions specific to this module.

- Reference `AGENTS.md` rules where relevant (e.g., background color rules)
- Document the single-source-of-truth pattern where applicable
- Note `@available(macOS 26.0, *)` requirements
```

## Rules

- Use `swift` as the fenced code block language
- Document only public/internal types, not private implementation details
- For `@Observable` classes: note that they use `@Bindable` injection, not `@EnvironmentObject`
- Mention `wakeEmacs()` calls if the type triggers Emacs side effects
- Keep method lists short — prefer a table or bullet list over full function signatures
- Base all descriptions on actual source files; do not invent behavior
- Design Notes section is optional — only include if there are non-obvious constraints
