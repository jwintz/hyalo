---
title: Changelog
description: Release history and notable changes to Hyalo
navigation:
  icon: i-lucide-history
  order: 99
order: 99
tags:
  - changelog
  - releases
  - hyalo
---

All notable changes to Hyalo are documented here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

:changelog-versions{:versions='[{"title":"Unreleased","description":"Preparing v0.1.0"}]'}

---

## Unreleased

### Fixed

- Branch picker showing "emacs" on startup — push current directory folder name when outside git repo
- Branch picker showing stale project name on startup — clear cached project roots in `hyalo-status-setup`
- `hyalo-environment--log` to use elog only — remove redundant `message` call
- Environment pill not showing on startup — push initial state after channel setup
- AttributeGraph cycle in environment pill — remove unused `@Bindable` from `BreadcrumbContent` view
- Pixi environment icon to use `puzzlepiece.extension.fill` SF Symbol
- Dark mode rendering green instead of violet in web header logo

### Added

- Native macOS `NavigationSplitView` layout with Navigator, Editor, Inspector, and UtilityArea panels
- Liquid Glass design system targeting macOS 26 Tahoe
- Bidirectional Swift ↔ Emacs Lisp async channel architecture
- `NavigatorAreaView` with three tabs: Buffers, Files, Source Control
- `InspectorAreaView` with four tabs: File, History, Appearance, Terminal
- `UtilityAreaView` with Terminal (SwiftTerm) and Diagnostics tabs
- `NSToolbar` with tracking separators, expanding activity pill, branch picker, package manager
- `CommandPalette` (Cmd+P) and Open Quickly (Cmd+O) as `NSPanel`-based overlays
- `EditorTabBarView` driven by `tab-line-mode` as single source of truth
- Native compilation activity viewer via `hyalo-compile.el`
- Package manager toolbar integration via `hyalo-package.el`
- 14 focused init modules: bootstrap, core, emacs, appearance, editing, completion, modes, tools, help, header, markdown, hyalo, agents, tengwar
- `iota-dimmer` for inactive buffer dimming
- Keycast integration broadcasting keypresses to toolbar
- Tengwar script rendering via overlays (optional)
- `exec-path-from-shell` leveraging `.zshrc` and `.zshenv`
- AOT native compilation of Emacs Lisp for fast subsequent startups

### Changed

- Buffer/tab sync uses `tab-line-mode` as single source of truth; `hyalo-sync--push` reads `tab-line-tabs-fixed-window-buffers` from `window-buffer-change-functions`
- `EditorTabViewModel` and `BufferListViewModel` remove all time-based guards — hook fires after switch is complete
- All Swift view models (Navigator, BufferList, EditorTab) remove local state mutation on click — Emacs is the single source of truth
- Loading proxy window only shown when bootstrap is needed (`.local/` or `.local/elpa/` missing)

### Fixed

- `:after hyalo` in `use-package` forms silently prevented `:config` blocks from executing; removed from all local Hyalo packages in `init-hyalo.el`
- Feedback loop in buffer sync: `first-change-hook` fired in `json-encode` temp buffers; added buffer-name guard and `hyalo-sync--inhibit` reentrancy guard
- Editor tab click reverted to previous buffer: missing `wakeEmacs()` on tab bar callbacks caused 1–3 second delay; added to all tab actions
- Slow startup in home directory: `hyalo-navigator--get-project-root` no longer falls back to `default-directory` when no git root is found
- Editor tab bar requiring two clicks to switch: time-based selection guard now stays active for full 100ms window
