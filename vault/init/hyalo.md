---
title: init-hyalo
description: macOS shell integration — dynamic module, early window setup, channels, and panel keybindings
navigation:
  icon: i-lucide-monitor
order: 12
tags:
  - init
  - emacs-lisp
  - configuration
  - hyalo
  - dynamic-module
  - channels
  - macos
---

`init-hyalo` is the integration point between Emacs and the surrounding Swift application. It loads the Hyalo dynamic module, installs the SwiftUI chrome early (before the rest of init runs), and wires panel keybindings, status, navigator, source control, system, compile, package, keycast, and appearance modules.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `hyalo` | dynamic module | Swift ↔ Emacs bridge |
| `ns-win` | built-in | macOS window frame configuration |

## Configuration Highlights

### macOS window frame

```elisp
(use-package ns-win
  :ensure nil
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (set-frame-parameter nil 'internal-border-width 0)
  (add-to-list 'default-frame-alist '(internal-border-width . 0))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  (general-unbind "C-z" "C-x C-z"))
```

### Loading the dynamic module

```elisp
(use-package hyalo
  :ensure nil
  :if (eq window-system 'ns)
  :custom
  (hyalo-auto-build t)
  :config
  (hyalo-load)
  ;; Early setup: install SwiftUI chrome before the rest of init.el runs.
  ;; Users see the Hyalo shell immediately, even if later init steps block.
  (when (hyalo-available-p)
    (require 'hyalo-window)
    (hyalo-window--early-setup)))
```

### Window controller and keybindings

```elisp
(when (and initial-window-system (hyalo-available-p))
  (with-eval-after-load 'general
    (leader-def
      "tn" '(hyalo-toggle-navigator    :wk "navigator")
      "ti" '(hyalo-toggle-inspector    :wk "inspector")
      "tu" '(hyalo-toggle-utility-area :wk "utility area")
      "tm" '(demap-toggle              :wk "minimap")))
  (add-hook 'window-setup-hook #'hyalo-window-setup))
```

Panel keybindings are bound under `C-c t` (the `leader-def` prefix):

| Key | Command | Action |
|-----|---------|--------|
| `C-c t n` | `hyalo-toggle-navigator` | Show / hide the Navigator sidebar |
| `C-c t i` | `hyalo-toggle-inspector` | Show / hide the Inspector panel |
| `C-c t u` | `hyalo-toggle-utility-area` | Show / hide the Utility Area |
| `C-c t m` | `demap-toggle` | Show / hide the minimap |

### macOS system integration

```elisp
(use-package hyalo-system
  :ensure nil
  :if (eq system-type 'darwin)
  :commands (fork-emacs hyalo-reveal-in-finder hyalo-share hyalo-show-emoji-picker)
  :general
  (leader-def
    "fE" '(fork-emacs :wk "fork emacs")
    "l"  '(:ignore t :wk "macOS")
    "lr" '(hyalo-reveal-in-finder :wk "reveal in finder")
    "ls" '(hyalo-share :wk "share...")
    "le" '(hyalo-show-emoji-picker :wk "emoji picker")
    "lm" '(hyalo-toggle-macos-menu-bar :wk "toggle menu bar")))
```

### Module setups loaded by init-hyalo

| Module | Purpose |
|--------|---------|
| `hyalo-status` | Status bar hook-driven updates |
| `hyalo-navigator` | Buffer list and project root push |
| `hyalo-source-control` | Git branch push to toolbar |
| `hyalo-menu` | macOS Application menu items |
| `hyalo-system` | Notifications, pasteboard, Services |
| `hyalo-compile` | Native compilation progress |
| `hyalo-package` | Package manager integration |
| `hyalo-keycast` | Keycast toolbar pill |
| `hyalo-appearance` | Theme color sync to Swift |

### Window dividers

```elisp
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places '(right bottom))
(window-divider-mode 1)
```

## See Also

- [[lisp/hyalo|hyalo.el]] — the dynamic module loaded by `(require 'hyalo)`
- [[lisp/hyalo-channels|hyalo-channels.el]] — channels opened on `window-setup-hook`
- [[lisp/hyalo-window|hyalo-window.el]] — `hyalo-window--early-setup` and `hyalo-window-setup`
- [[swift/core|Swift Core]] — module entry point registered at `module-load` time
