# Hyalo

macOS IDE shell around Emacs via dynamic module (`.dylib`).

## Architecture

- **61 Swift source files** across 12 modules: Core, Window, Editor, Navigator, Inspector, StatusBar, UtilityArea, Toolbar, CommandPalette, Appearance, Shared
- **26 Emacs Lisp files** in `lisp/`
- **14 modular init files** in `init/`
- **NSSplitViewController** 3-panel layout: Navigator | Editor | Inspector
- **NSToolbar** with tracking separators, expanding pill activity viewer, branch picker
- **Channel architecture** for bidirectional Swift<->Emacs Lisp communication (see [Channel Architecture](#channel-architecture) below)
- **NSPanel-based** command palette (Cmd+P) and open-quickly (Cmd+O) dialogs
- **SwiftTerm** terminal integration in inspector and utility panels
- **ProjectNavigator** (`mchakravarty/ProjectNavigator`) for file tree navigation with `FileNavigator`, UUID-based identity, expansion/selection state, and editable labels

## Test Procedure

```bash
emacs --init-directory ~/Syntropment/hyalo
```

This launches Emacs with the modular init system:
1. `init-bootstrap` — Package archives, GC tuning, exec-path-from-shell
2. `init-core` — diminish, general.el, which-key
3. `init-emacs` — Cursor, startup, recentf, saveplace, autorevert
4. `init-appearance` — Fonts, modus-themes, nano-themes, ef-themes, lin, iota-dimmer
5. `init-editing` — Editing packages (god-mode, windmove, outline)
6. `init-completion` — Vertico, Consult, Marginalia, Orderless
7. `init-tools` — Dev tools (project, magit, eglot, flymake)
8. `init-help` — Help system (helpful, elisp-refs)
9. `init-modes` — Language modes (json, swift, toml, typescript, git-modes)
10. `init-markdown` — Markdown and knowledge management
11. `init-header` — File header management (header2)
12. `init-hyalo` — Module load, window setup, panel toggles, keybindings
13. `init-agents` — AI agents (copilot)
14. `init-tengwar` — Tengwar script rendering (optional)

### Prerequisites

- macOS with Xcode Command Line Tools
- Swift 6.0 or later
- **Emacs 30.1 or later compiled with `--with-modules`**

### Build

```bash
swift build
```

### Theme System

Themes use [modus-themes](https://github.com/protesilaos/modus-themes) as the foundation, with [nano-themes](https://github.com/rougier/nano-theme) (built on modus) providing the default light/dark pair:

- **Default light**: `nano-light` (Material Design palette)
- **Default dark**: `nano-dark` (Nord palette)
- **Additional**: `ef-themes` collection available via `M-x load-theme`

Theme-appearance synchronization:
- Loading a dark theme automatically switches the Swift window to dark appearance
- Loading a light theme automatically switches the Swift window to light appearance
- System appearance changes (`ns-system-appearance-change-functions`) load the configured default theme
- Selecting Light/Dark in the inspector appearance panel loads the corresponding theme via channel callback
- Appearance settings (mode, opacity, material) persist across sessions via UserDefaults
- Window divider color is derived from the active theme palette
- Terminal palette is derived from theme face colors

### Init Files (init/)

| File | Purpose |
|------|---------|
| `init-bootstrap.el` | Package system, GC optimization, exec-path-from-shell |
| `init-core.el` | diminish, general.el keybinder, which-key |
| `init-emacs.el` | Cursor, startup, recentf, saveplace, autorevert |
| `init-appearance.el` | Fonts, modus-themes, nano-themes, ef-themes, lin, iota-dimmer |
| `init-editing.el` | Editing packages (god-mode, windmove, outline) |
| `init-completion.el` | Vertico, Consult, Marginalia, Orderless |
| `init-tools.el` | Dev tools (project, magit, eglot, flymake, diff-hl) |
| `init-help.el` | Help system (helpful, elisp-refs) |
| `init-modes.el` | Language modes (json, swift, toml, typescript, git-modes) |
| `init-markdown.el` | Markdown and knowledge management |
| `init-header.el` | File header management (header2) |
| `init-hyalo.el` | macOS integration, module load, window setup, keybindings |
| `init-agents.el` | AI agents (copilot) |
| `init-tengwar.el` | Tengwar script rendering (optional) |

### Lisp Files (lisp/)

| File | Purpose |
|------|---------|
| `hyalo.el` | Core loader, build, module-load |
| `hyalo-window.el` | Window controller, setup orchestration, Cmd+O/P, panel toggles |
| `hyalo-channels.el` | Channel lifecycle, callback handlers, rg search execution |
| `hyalo-navigator.el` | Buffer list, file tree push to Swift |
| `hyalo-status.el` | Hook-driven status updates (cursor, tabs, branch, file info, navigator refresh) |
| `hyalo-source-control.el` | Git changed files and commit history push (debounced on save) |
| `hyalo-appearance.el` | Vibrancy, background color, divider color, frame transparency |
| `hyalo-themes.el` | Theme switching, appearance sync, terminal palette |
| `hyalo-activities.el` | Activities (tab-bar) integration for breadcrumb |
| `hyalo-compile.el` | Native compilation activity tracking |
| `hyalo-diagnostics.el` | Flymake diagnostics panel integration |
| `hyalo-environment.el` | Environment detection and breadcrumb push |
| `hyalo-gutter.el` | Diff-hl gutter integration |
| `hyalo-keycast.el` | Keycast toolbar integration |
| `hyalo-lib.el` | Transient hooks, first-use hooks, idle package loader |
| `hyalo-menu.el` | Menu bar integration |
| `hyalo-minimap.el` | Minimap integration |
| `hyalo-package.el` | Package manager toolbar integration |
| `hyalo-splash.el` | Splash screen |
| `hyalo-system.el` | System information panel |
| `iota-dimmer.el` | Inactive window dimming (HSL-based face color manipulation) |
| `iota-faces.el` | Iota face definitions |
| `nano-themes.el` | N Λ N O theme infrastructure (built on modus-themes) |
| `nano-light-theme.el` | N Λ N O light theme (Material Design palette) |
| `nano-dark-theme.el` | N Λ N O dark theme (Nord palette) |
| `header2.el` | File header creation and update (vendored) |

### Channel Architecture

Hyalo uses two communication mechanisms between Emacs Lisp and Swift:

#### 1. Synchronous Functions (Emacs → Swift)

Defined via `env.defun()` in `Module.swift`. Called directly from Elisp. Execute on the Emacs thread. Most dispatch to `DispatchQueue.main.async` for @MainActor-isolated SwiftUI state updates, meaning the actual state change is **deferred** — the Elisp function returns before Swift processes the update.

| Function | Purpose | Dispatch |
|----------|---------|----------|
| `hyalo-navigator-set-active-file` | Set navigator tree selection from Emacs buffer change | `main.async` |
| `hyalo-navigator-set-active-buffer` | Set active buffer in buffer list | `main.async` |
| `hyalo-navigator-set-project-root` | Set project root, rebuild file tree | `main.async` |
| `hyalo-navigator-refresh-file-tree` | Force file tree rebuild | `main.async` |
| `hyalo-navigator-update-buffers` | Push buffer list JSON | `main.async` |
| `hyalo-status-update` | Push cursor, mode, encoding, etc. | `main.async` |
| `hyalo-update-editor-tabs` | Push editor tab list JSON | `assumeIsolated` |
| `hyalo-select-editor-tab` | Select tab by buffer name | `assumeIsolated` |
| `hyalo-update-branch-info` | Push git branch JSON | `main.async` |
| `hyalo-set-project-name` | Set toolbar project name | `main.async` |
| `hyalo-set-workspace-appearance` | Set light/dark appearance | `main.async` |
| `hyalo-set-current-theme-name` | Set theme name in inspector | `main.async` |
| `hyalo-set-color-theme` | Push color theme JSON | `main.async` |
| `hyalo-set-terminal-palette` | Push terminal palette JSON | `main.async` |
| `hyalo-set-vibrancy-material` | Set vibrancy material level | `main.async` |
| `hyalo-set-background-color` | Set tint color hex + alpha | `main.async` |
| `hyalo-update-file-info` | Push file info JSON to inspector | `main.async` |
| `hyalo-update-git-history` | Push git history JSON to inspector | `main.async` |
| `hyalo-update-search-results` | Push search results JSON | `main.async` |
| `hyalo-update-search-status` | Push search status counts | `main.async` |
| `hyalo-update-changed-files` | Push git changed files JSON | `main.async` |
| `hyalo-update-commit-history` | Push git commit history JSON | `main.async` |
| `hyalo-update-diagnostics` | Push flymake diagnostics JSON | `main.async` |
| `hyalo-update-build-status` | Push native compilation status | `main.async` |
| `hyalo-update-build-progress` | Push compilation progress | `main.async` |
| `hyalo-update-package-status` | Push package list JSON | `main.async` |
| `hyalo-update-open-quickly-items` | Push file list for Cmd+O | `main.async` |
| `hyalo-update-command-list` | Push command list for Cmd+P | `main.async` |

**Key insight**: Functions using `DispatchQueue.main.async` return to Elisp immediately. The Swift-side state update happens on the **next** main run loop iteration. Multiple synchronous calls from the same Elisp function (e.g., `set-active-buffer` followed by `set-active-file`) are queued in order but execute later.

#### 2. Async Channels (Swift → Emacs)

Created via `env.openChannel()`. Channel callbacks are Swift closures that, when called, schedule Elisp execution on the Emacs event loop via `wakeEmacs()`. The Elisp runs when Emacs processes its next event.

| Channel | Callbacks (Swift → Emacs) | Purpose |
|---------|--------------------------|---------|
| `hyalo-navigator` | `switch-to-buffer`, `kill-buffer`, `find-file` | User clicks in navigator sidebar |
| `hyalo-editor-tabs` | `switch-to-buffer`, `kill-buffer`, `previous-buffer`, `next-buffer` | User clicks tab bar |
| `hyalo-status` | `hyalo-status--set-encoding`, `set-line-ending`, `set-indent` | User clicks status bar items |
| `hyalo-toolbar` | `hyalo-channels--handle-branch-switch` | User switches branch in picker |
| `hyalo-command-palette` | `hyalo-channels--handle-open-file`, `handle-execute-command` | User selects in Cmd+O / Cmd+P |
| `hyalo-search` | `hyalo-channels--handle-search`, `handle-search-navigate` | User searches or clicks result |
| `hyalo-appearance` | `hyalo-channels--handle-appearance-mode`, `handle-opacity-change` | User changes appearance settings |
| `hyalo-diagnostics` | `hyalo-channels--handle-diagnostic-navigate` | User clicks diagnostic |
| `hyalo-package` | `handle-package-refresh`, `upgrade-all`, `upgrade-single`, `list` | Package manager actions |
| `hyalo-source-control` | `hyalo-channels--handle-show-commit`, `handle-show-diff` | User clicks commit or changed file |

**Key insight**: Channel callbacks invoke `wakeEmacs()` after queuing the callback. Emacs processes the callback when it returns to its event loop. If Emacs is busy (e.g., processing a hook), the callback is deferred. This means there is an **indeterminate delay** between the Swift-side action and the Elisp execution.

#### Navigator Selection Flow (example)

This illustrates the async ordering problem. When the user clicks file B in the sidebar (currently showing file A):

```
Swift (main thread)                          Emacs (event loop)
──────────────────                          ──────────────────
1. List sets viewState.selection = B.id
2. onChange fires → onFileSelect(B_path)
3. Guard: B_path != activeFilePath(A) → pass
4. NavigatorManager.onFileSelect(B_path)
5. channel.callback(find-file B)
6. wakeEmacs()
   ↓ (async)                                 7. find-file B executes
                                              8. window-buffer-change-functions fires
                                              9. hyalo-navigator-set-active-file(B)
                                                 ↓ (sync call into Swift defun)
10. DispatchQueue.main.async {
      setActiveFile(B)                       10'. [returns to Emacs immediately]
    }                                        11. buffer-list-update-hook fires
   ↓ (async)                                 12. hyalo-navigator--update-buffers
13. setActiveFile(B) executes
14. activeFilePath = B
15. viewState.selection = B.id (already B)

RISK: Between steps 6 and 7, if Emacs processes other events
(e.g., timer-fired buffer-list-update-hook), it may call
hyalo-navigator-set-active-file(A) for the OLD buffer, which
queues setActiveFile(A) on main.async. If that executes AFTER
step 13, the selection reverts to A.
```

#### Race Condition Fix

The actual race was more subtle — `activeFilePath`/`activeBuffer`/`pendingTabId` were only updated in the async callback from Emacs, not immediately when the user clicked. This meant:

1. User clicks A → `activeFilePath = nil` (not yet set) → channel sends `find-file A`
2. User clicks B (before A's callback) → `activeFilePath = nil` → guard passes! → channel sends `find-file B`
3. Callback for A arrives → `setActiveFile(A)` → sets `activeFilePath = A`, `selection = A` → **revert!**

**Fix** (applied to `ProjectNavigatorViewModel`, `BufferListViewModel`, `EditorTabViewModel`):

1. **Set active/pending state immediately** when the user clicks (`selectFile`, `selectBuffer`, `selectFile` for tabs) **before** calling the channel callback
2. **Check for stale callbacks** in the Emacs callback methods (`setActiveFile`, `setActiveBuffer`, `onTabSelected`) — skip if the callback doesn't match what's currently active/pending
3. **Use immediate state in guards** — the guard in `onFileSelect`/`onBufferSelect` now uses the immediately-set value to block rapid duplicates

For the editor tab bar specifically, the flow is:
1. Click file A in navigator → `selectFile(A)` sets `pendingTabId = A` → channel sends `find-file A`
2. Click file B → `selectFile(B)` sets `pendingTabId = B` → channel sends `find-file B`
3. Tab callback for B arrives → `onTabSelected(B)` → matches `pendingTabId(B)` → update tab selection → **correct**
4. Tab callback for A arrives (late) → `onTabSelected(A)` → `A != pendingTabId(B)` → **skip stale callback**

Note: `pendingTabId` is NOT cleared after processing a callback. It always tracks the most recent user-initiated selection, protecting against arbitrarily late stale callbacks.

#### Centralized State Push

All buffer switch operations (file navigator, buffer list, tab bar, command palette) now use a centralized `hyalo-push-active-buffer-state` function in `hyalo-status.el`. This function updates all Swift UI components (buffer list selection, file navigator selection, editor tab bar) in a single call, ensuring consistency.

Channel callbacks use wrapper functions (`hyalo-channels--handle-switch-buffer`, `hyalo-channels--handle-find-file`) that:
1. Perform the Emacs action (`switch-to-buffer`, `find-file`)
2. Call `hyalo-push-active-buffer-state` to sync all UI components

The `window-buffer-change-functions` hook also calls `hyalo-push-active-buffer-state` for buffer changes initiated within Emacs (C-x b, etc.).

#### Tracing

All navigator selection touchpoints emit `[Hyalo:Nav:trace]` logs (NSLog on Swift, `hyalo-trace` on Elisp). To observe the flow:

1. **Swift logs**: visible in Console.app (filter: `Hyalo:Nav:trace`)
2. **Elisp logs**: visible in `*elog*` buffer (requires `elog` package, auto-initialized)
3. Enable elog trace level: `(setq hyalo-elog (elog-logger :name "hyalo" :level 'trace :buffer "*elog*" :handlers '(buffer)))`

### Status Update Architecture

Status updates use Emacs hooks with debounced timers (no polling):

| Data | Trigger | Debounce |
|------|---------|----------|
| Cursor position, mode, encoding | `post-command-hook` | 50ms |
| Editor tabs | `buffer-list-update-hook`, `after-save-hook`, `first-change-hook` | 200ms |
| File info + git history | `window-buffer-change-functions` | immediate |
| Branch info + project name | `window-buffer-change-functions` | immediate |
| Navigator file tree | `window-buffer-change-functions` (on project root change) | 500ms |
| Source control (changes, history) | `after-save-hook` | 3s |

On buffer switch, `default-directory` is updated to the enclosing git root. If the project root changes, the navigator file tree and source control data refresh automatically.

### Keybindings

| Key | Command | Description |
|-----|---------|-------------|
| `Cmd+O` | `hyalo/open-quickly` | Fuzzy file search panel |
| `Cmd+P` | `hyalo/command-palette` | M-x style command palette |
| `C-c t n` | `hyalo-toggle-navigator` | Toggle left sidebar |
| `C-c t i` | `hyalo-toggle-inspector` | Toggle right sidebar |
| `C-c t u` | `hyalo-toggle-utility-area` | Toggle bottom panel |

### Development

Rebuild and reload without restarting Emacs:

```elisp
M-x hyalo-rebuild-and-reload
```
