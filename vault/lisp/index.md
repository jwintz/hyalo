---
title: Lisp Side
description: Hyalo's Emacs Lisp layer — hooks, channels, and data push to Swift
navigation:
  icon: i-lucide-parentheses
  order: 0
order: 0
tags:
  - lisp
  - overview
---

The Lisp side of Hyalo provides the Emacs Lisp layer that:
- Loads the Swift dynamic module at startup
- Opens bidirectional async channels (Swift ↔ Emacs)
- Pushes data to Swift views via hooks (no polling — hooks and advices only)
- Manages appearance, navigator, status bar, diagnostics, and toolbar state

All push operations are triggered by Emacs hooks and advices. No polling timers are used for data synchronization.

## Modules

![[lisp-modules.base]]

## See Also

- [[features/lisp-side|Lisp Side feature overview]]
- [[init/hyalo|init-hyalo.el]] — loads the dynamic module and channels
- [[swift/core|Swift Core]] — receives Lisp function calls at module load
- [[swift/index|Swift Side]] — companion Swift module reference
