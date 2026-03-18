---
title: Hyalo
description: A macOS IDE shell around Emacs — native Liquid Glass design, bidirectional Swift channels, and a carefully crafted default configuration.
navigation: false
---

::u-page-hero
---
title: Hyalo
description: An IDE shell around Emacs for macOS. Native Liquid Glass surfaces, bidirectional Swift channels, and 14 curated init modules — batteries included.
links:
  - label: Get Started
    to: /home
    icon: i-lucide-arrow-right
    color: neutral
    size: xl
  - label: View on GitHub
    to: https://github.com/jwintz/hyalo
    icon: simple-icons-github
    color: neutral
    variant: outline
    size: xl
---
::

::u-page-grid{class="lg:grid-cols-3 max-w-(--ui-container) mx-auto px-4 pb-24"}

:::u-page-card
---
spotlight: true
class: col-span-3 lg:col-span-1
to: /features/architecture
icon: i-lucide-git-branch
---
#title
Swift inside Emacs

#description
The `.dylib` is loaded into the Emacs process — no XPC, no IPC, no sockets. Swift and Emacs share the same heap. Bidirectional async channels connect them.
:::

:::u-page-card
---
spotlight: true
class: col-span-3 lg:col-span-1
to: /features/appearance
icon: i-lucide-sparkles
---
#title
Liquid Glass

#description
Panels use real-time blur and vibrancy. NSVisualEffectView materials, tracking separators, and an expanding toolbar activity pill — all on macOS 26 Tahoe.
:::

:::u-page-card
---
spotlight: true
class: col-span-3 lg:col-span-1
to: /features/native-compilation
icon: i-lucide-zap
---
#title
Native Compilation

#description
Built on Emacs with `--with-native-compilation`. Every `.el` file AOT-compiled to native code via libgccjit. Zero startup latency, no JIT pauses.
:::

:::u-page-card
---
spotlight: true
class: col-span-3 lg:col-span-1
to: /init/index
icon: i-lucide-layers
---
#title
14 Init Modules

#description
Completion, editing, tools, AI, appearance, markdown — all wired up. Each module owns exactly one concern. Nothing fights you.
:::

:::u-page-card
---
spotlight: true
class: col-span-3 lg:col-span-1
to: /features/swift-side
icon: i-lucide-layout-panel-left
---
#title
IDE Chrome

#description
Navigator, Editor, Inspector, Utility, Toolbar, StatusBar, CommandPalette — all native SwiftUI. The macOS app shell Emacs never had.
:::

:::u-page-card
---
spotlight: true
class: col-span-3 lg:col-span-1
to: /features/init-system
icon: i-lucide-puzzle
---
#title
Modular Config

#description
Start with the default config or fork it. Each `init-*.el` module is self-contained. Add, remove, or replace modules without touching the rest.
:::

::
