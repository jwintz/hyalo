---
title: Settings
description: App settings — placeholder for future native settings UI
navigation:
  icon: i-lucide-settings-2
order: 11
tags:
  - swift
  - module
  - settings
---

## Overview

The Settings module is a reserved placeholder for a future native settings UI. As of the current implementation, all user-configurable preferences are managed through two surfaces:

- **Inspector Appearance tab** (`InspectorAppearanceView`) — editor tint colour, background opacity, vibrancy material
- **UserDefaults** — persisted directly by the relevant views (e.g., `@AppStorage` bindings in `InspectorAppearanceView`, `ToolbarManager`)

There are no Swift source files in this module yet.

## Planned Settings Surface

When implemented, the Settings module will provide an `NSSettingsScene`-based settings window (macOS 13+ API) with the following panes:

| Pane | Contents |
|------|----------|
| General | Default project directory, startup behaviour, font size |
| Appearance | Theme, tint colour, vibrancy material, opacity |
| Keybindings | Browsable key map mirroring `describe-bindings` |
| Accounts | Git credentials, package registry tokens |
| Advanced | Debug flags, channel logging, module reload |

## Current Preference Storage

Preferences currently stored in `UserDefaults`:

| Key | Type | Default | Owner |
|-----|------|---------|-------|
| `hyalo.vibrancyMaterial` | `Int` | `0` (sidebar) | `InspectorAppearanceView` |
| `hyalo.showKeycast` | `Bool` | `false` | `ToolbarManager` |
| `hyalo.showBranchPicker` | `Bool` | `true` | `ToolbarManager` |
| `hyalo.sidebarWidth` | `Double` | `240` | `HyaloWorkspaceState` |
| `hyalo.inspectorWidth` | `Double` | `260` | `HyaloWorkspaceState` |

## Design Notes

- Do not implement settings views that duplicate Emacs Customise functionality. Swift settings should cover only native UI concerns (appearance, window layout, credentials).
- All Emacs-side preferences (keybindings, package config, mode behaviour) remain in Emacs Lisp and are edited through the normal Emacs Customise interface.
- When this module is implemented, migrate `@AppStorage` bindings from individual views into a single `HyaloSettings` observable model to centralise preference access.
