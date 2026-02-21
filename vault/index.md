---
title: Hyalo
description: A macOS IDE shell around Emacs — native Liquid Glass design, bidirectional Swift channels, and a carefully crafted default configuration.
navigation: false
---

::rellax-hero
---
label: macOS · Emacs · SwiftUI
title: Hyalo
subtitle: An IDE shell around Emacs.
tagline: Native macOS integration. Liquid Glass surfaces. Infinitely extensible.
image: /_raw/Assets/Hyalo-Landing-3.jpg
cta:
  - label: Get Started
    to: /features
    icon: i-lucide-arrow-right
  - label: GitHub
    to: https://github.com/jwintz/hyalo
    icon: simple-icons-github
    variant: outline
---
::

::highlight-strip
---
items:
  - label: Liquid Glass
    anchor: liquid-glass
  - label: Architecture
    anchor: architecture
  - label: Native Compilation
    anchor: native-compilation
  - label: Configuration
    anchor: configuration
  - label: Design System
    anchor: design-system
  - label: Explore
    anchor: explore
---
::

::highlights-gallery
---
items:
  - title: Liquid Glass
    description: Panels with real-time blur and vibrancy. Every surface adapts to your content.
    image: /_raw/Assets/Hyalo-Landing-2.jpg
    anchor: liquid-glass
  - title: Swift + Emacs
    description: One process. Shared heap memory. Zero serialization overhead.
    image: /_raw/Assets/Hyalo-Landing-3.jpg
    anchor: architecture
  - title: Native Compilation
    description: AOT via libgccjit. Every .el file compiles to native code.
    image: /_raw/Assets/Hyalo-Landing-1.jpg
    anchor: native-compilation
  - title: 14 Init Modules
    description: Batteries included. Completion, tools, editing, AI — nothing you fight.
    image: /_raw/Assets/Hyalo-Landing-2.jpg
    anchor: configuration
  - title: Design System
    description: SF Fonts. SF Symbols. Curated theme stack. System colors everywhere.
    image: /_raw/Assets/Hyalo-Landing-1.jpg
    anchor: design-system
---
::

<!-- ═══════════════════════════════════════════════ -->
<!-- LIQUID GLASS                                    -->
<!-- ═══════════════════════════════════════════════ -->

::section-intro{id="liquid-glass"}
---
label: Design
title: Liquid Glass.
accent: Every surface, reimagined.
body: Hyalo targets macOS 26 Tahoe. Panels use real-time blur and vibrancy materials. The toolbar uses tracking separators and an expanding activity pill. Every surface reads the system accent color and adapts to your content.
---
::

::feature-strip
---
label: Vibrancy
heading: Panels that breathe.
accent: Content that shines through.
body: The navigator, inspector, and utility area use NSVisualEffectView materials. Sidebar backgrounds refract the content behind them. Toolbar items float over a translucent title bar. The result is a workspace that feels weightless — a glass shell around your code.
image: /_raw/Assets/Hyalo-Landing-2.jpg
to: /features/appearance
toLinkLabel: See the design system
---
::

<!-- ═══════════════════════════════════════════════ -->
<!-- ARCHITECTURE                                    -->
<!-- ═══════════════════════════════════════════════ -->

::section-intro{id="architecture"}
---
label: Architecture
title: One process.
accent: No boundaries.
body: A Swift dynamic module loaded directly into the Emacs process via module_init. Swift and Emacs share heap memory. No XPC, no socket, no serialization. NavigationSplitView, NSToolbar, SwiftTerm, and SF Symbols sit alongside your buffers.
---
::

::stat-callout
---
image: /_raw/Assets/Hyalo-Landing-1.jpg
stats:
  - value: "67"
    label: Swift source files
  - value: "12"
    label: modules
  - value: "0"
    label: IPC overhead
caption: A bidirectional channel system lets any package push structured data to the UI without polling.
---
::

::feature-strip
---
label: Channels
heading: Bidirectional communication.
accent: Emacs Lisp meets Swift.
body: The channel architecture bridges Emacs Lisp and SwiftUI. Push diagnostics from Eglot to the utility area. Sync buffer metadata to the inspector. Update the toolbar activity pill from a compilation process. All through typed channel messages — no polling, no timers.
image: /_raw/Assets/Hyalo-Landing-3.jpg
flip: true
to: /features/architecture
toLinkLabel: Explore the architecture
---
::

<!-- ═══════════════════════════════════════════════ -->
<!-- NATIVE COMPILATION                              -->
<!-- ═══════════════════════════════════════════════ -->

::section-intro{id="native-compilation"}
---
label: Performance
title: Near-zero startup.
accent: Native speed, always.
body: Hyalo compiles every Emacs Lisp file ahead-of-time via libgccjit. The feedstock at hyalo-feedstock delivers the exact binary — with-modules and with-native-compilation enabled by default.
---
::

::image-mosaic
---
images:
  - src: /_raw/Assets/Hyalo-Landing-1.jpg
    alt: Code editor with syntax highlighting
  - src: /_raw/Assets/Hyalo-Landing-3.jpg
    alt: Development workstation
  - src: /_raw/Assets/Hyalo-Landing-2.jpg
    alt: Clean macOS desktop
caption: Every .el file compiles to native code. The result is an editor that launches in milliseconds and never stutters.
---
::

<!-- ═══════════════════════════════════════════════ -->
<!-- CONFIGURATION                                   -->
<!-- ═══════════════════════════════════════════════ -->

::section-intro{id="configuration"}
---
label: Configuration
title: 14 init modules.
accent: A foundation you extend, not fight.
body: Completion with Vertico, Consult, and Corfu. Tools with Magit and Eglot. Editing with god-mode and Avy. AI with Copilot. Each module is a focused use-package block — load order handled for you, keybindings via general.el.
---
::

::feature-strip
---
label: Modules
heading: Batteries included.
accent: Nothing missing, nothing excess.
body: Bootstrap handles GC tuning, package archives, and exec-path-from-shell. Core wires general and which-key. Emacs configures cursor, startup, recentf, and saveplace. Appearance brings curated themes and SF Mono. Every module is independent, documented, and overridable.
image: /_raw/Assets/Hyalo-Landing-1.jpg
to: /init
toLinkLabel: Browse init modules
---
::

<!-- ═══════════════════════════════════════════════ -->
<!-- DESIGN SYSTEM                                   -->
<!-- ═══════════════════════════════════════════════ -->

::section-intro{id="design-system"}
---
label: Appearance
title: System-native design.
accent: From fonts to frames.
---
::

::feature-strip
---
label: Themes
heading: A curated theme stack.
accent: Light and dark, always in sync.
body: modus-themes as the foundation. nano-themes for the default light and dark pair. ef-themes for variety. Per-element transparency via iota-dimmer. Theme-appearance synchronization — loading a dark theme switches the Swift window to dark mode. The inspector panel lets you override everything.
image: /_raw/Assets/Hyalo-Landing-2.jpg
flip: true
to: /features/appearance
toLinkLabel: See the design system
---
::

::stat-callout
---
image: /_raw/Assets/Hyalo-Landing-3.jpg
stats:
  - value: "14"
    label: init modules
  - value: "20"
    label: lisp modules
  - value: "12"
    label: Swift modules
caption: Every layer documented, explorable, and overridable.
---
::

<!-- ═══════════════════════════════════════════════ -->
<!-- EXPLORE                                         -->
<!-- ═══════════════════════════════════════════════ -->

::more-grid{id="explore"}
---
heading: Explore every layer.
items:
  - icon: i-lucide-monitor
    title: Window Management
    description: NSSplitViewController three-panel layout. Navigator, editor, inspector. Drag-to-resize with persistent divider positions.
    to: /swift/window
  - icon: i-lucide-layout-panel-left
    title: Navigator
    description: ProjectNavigator file tree with UUID-based identity, expansion state, and editable labels. Buffers, project, and SCM views.
    to: /swift/navigator
  - icon: i-lucide-panel-right
    title: Inspector
    description: Live document metadata, font controls, and appearance overrides. All rendered in SwiftUI inside the Emacs process.
    to: /swift/inspector
  - icon: i-lucide-terminal
    title: Utility Area
    description: SwiftTerm terminal integration. Diagnostics from Eglot and Flymake. Compilation output. All in a collapsible bottom panel.
    to: /swift/utilityarea
  - icon: i-lucide-command
    title: Command Palette
    description: NSPanel-based command palette and open-quickly dialogs. Cmd+P for commands, Cmd+O for files.
    to: /swift/commandpalette
  - icon: i-lucide-settings
    title: Toolbar
    description: NSToolbar with tracking separators, expanding activity pill, and branch picker. Native macOS toolbar behavior.
    to: /swift/toolbar
  - icon: i-lucide-sparkles
    title: Appearance Module
    description: SF Mono faces, modus-themes, nano-themes, ef-themes, lin, and iota-dimmer. System accent color integration.
    to: /features/appearance
  - icon: i-lucide-zap
    title: Native Compilation
    description: AOT via libgccjit. hyalo-feedstock builds Emacs 30.1+ with --with-modules and --with-native-compilation.
    to: /features/native-compilation
  - icon: i-lucide-git-branch
    title: Source Control
    description: Magit integration with status pushed to the toolbar branch picker via the channel system.
    to: /lisp/hyalo-source-control
  - icon: i-lucide-message-square
    title: Channels
    description: Bidirectional Swift-Emacs Lisp communication. Typed messages, no polling, no serialization overhead.
    to: /features/architecture
  - icon: i-lucide-palette
    title: Themes
    description: Theme-appearance synchronization. Dark theme loads dark appearance. Light theme loads light. Inspector overrides everything.
    to: /lisp/hyalo-themes
  - icon: i-lucide-cpu
    title: AI Integration
    description: Copilot module in init-agents. Context-aware completions wired through the init system.
    to: /init/agents
---
::
