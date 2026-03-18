---
title: Hyalo
description: IDE shell around Emacs for macOS — Liquid Glass, bidirectional Swift channels, curated default configuration
icon: i-lucide-monitor
order: 0
navigation:
  title: Home
  icon: i-lucide-home
  order: 0
---

Hyalo is an IDE shell around Emacs for macOS, built as a dynamic module (`.dylib`) loaded by Emacs at startup. Native Liquid Glass surfaces, bidirectional Swift/Emacs Lisp channels, and 14 init modules — batteries included.

## Requirements

- macOS 26+ with Xcode 17+
- Swift 6.2 or later
- Emacs 30.1+ compiled with `--with-modules` — from [[../hyalo-feedstock|hyalo-feedstock]]

## Getting Emacs

The fastest way to get a compatible Emacs is via the [jwintz Pixi channel](https://prefix.dev/channels/jwintz/packages/emacs):

```bash
pixi global install emacs --channel https://prefix.dev/channels/jwintz --channel conda-forge
```

This installs a pre-built, codesigned `Emacs.app` with `--with-modules` and `--with-native-compilation` already enabled. Alternatively, build from source with [[../hyalo-feedstock/vault/1.guide/1.quickstart|hyalo-feedstock]].

## Quick Start

```bash
emacs --init-directory /path/to/hyalo
```

This launches Emacs with Hyalo's modular init system. The Swift module loads automatically and the IDE shell appears.

## Build

```bash
swift build                    # debug build
pixi run build-release         # release build
pixi run package               # assemble Hyalo.app bundle
pixi run dmg                   # create DMG installer
```

## Documentation

- [[features/index|Features]]
- [[features/appearance|Appearance]]
- [[features/architecture|Architecture]]
- [[features/native-compilation|Native Compilation]]
- [[init/index|Init Modules]]
- [[swift/index|Swift Source]]
- [[lisp/index|Emacs Lisp Source]]
- [[Changelog]]
