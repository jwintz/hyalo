;;; hyalo-window.el --- Window controller interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Interface to the NSSplitViewController-based window layout.
;; Called from init-hyalo.el via window-setup-hook.

;;; Code:

(require 'hyalo)
(require 'json)

(defvar hyalo-window--post-setup-max-retries 20
  "Maximum retries (at 100ms intervals) to wait for the window controller.")

;;; Early Setup (called from init-hyalo.el, before window-setup-hook)

(defvar hyalo-window--early-setup-done nil
  "Non-nil once early setup (decoration + visibility) has completed.")

(defun hyalo-window--early-setup ()
  "Decorate the initial frame and show the loading proxy window.
Called from `init.el' at the very top, before `init-bootstrap' runs.
The Emacs frame itself stays invisible (`visibility . nil') until
`hyalo-loading-done' is called at the end of `hyalo-window--post-setup'.
A standalone SwiftUI proxy window is shown instead while init runs.
Channel setup and data push happen later in `hyalo-window-setup'."
  (when (and (hyalo-available-p)
             (not hyalo-window--early-setup-done)
             (fboundp 'hyalo-navigation-setup))
    ;; Set lisp dir first so LoadingView can locate the SVG before it appears.
    (when (fboundp 'hyalo-set-base-dir)
      (hyalo-set-base-dir (expand-file-name "lisp" hyalo--base-dir)))
    (let ((frame-id (string-to-number
                     (or (frame-parameter nil 'window-id) "0"))))
      (hyalo-navigation-setup frame-id))
    ;; Setup is now synchronous — the controller is ready immediately.
    ;; A single sit-for lets AppKit process the content-view swap before
    ;; we continue.  No polling loop needed.
    (sit-for 0)
    ;; When no bootstrap is needed, the proxy was not created.
    ;; Reveal the frame immediately so the user sees the IDE shell
    ;; while the rest of init loads.  When bootstrapping, the proxy
    ;; is visible and the frame stays hidden until hyalo-loading-done.
    (unless (bound-and-true-p hyalo--needs-bootstrap)
      (make-frame-visible))
    (setq hyalo-window--early-setup-done t)
    (message "Hyalo: Chrome installed")))

;;; Core Setup

(defun hyalo-window-setup ()
  "Setup channels, push initial data, start status updates.
Called from `window-setup-hook' after the first frame is ready.
The window decoration was already done by `hyalo-window--early-setup'."
  (interactive)
  (when (fboundp 'hyalo--boot-log)
    (hyalo--boot-log "hyalo-window-setup: entered"))
  (when (hyalo-available-p)
    ;; If early setup didn't run (e.g., module loaded late),
    ;; do the decoration now.  decorateWindow creates the proxy window;
    ;; hyalo-loading-done (at the end of post-setup) reveals the frame.
    (unless hyalo-window--early-setup-done
      (when (fboundp 'hyalo-navigation-setup)
        (when (fboundp 'hyalo-set-base-dir)
          (hyalo-set-base-dir (expand-file-name "lisp" hyalo--base-dir)))
        (let ((frame-id (string-to-number
                         (or (frame-parameter nil 'window-id) "0"))))
          (hyalo-navigation-setup frame-id))
        (hyalo-window--wait-for-controller 0)
        (setq hyalo-window--early-setup-done t)))
    ;; If the controller is ready (from early setup), do post-setup
    ;; directly. Otherwise wait for it.
    (if (and (fboundp 'hyalo-window-controller-ready-p)
             (hyalo-window-controller-ready-p))
        (hyalo-window--post-setup)
      (hyalo-window--wait-for-controller 0))))

(defun hyalo-window--wait-for-controller (attempt)
  "Wait for the window controller, retrying up to max retries.
ATTEMPT is the current retry count."
  (if (and (fboundp 'hyalo-window-controller-ready-p)
           (hyalo-window-controller-ready-p))
      (hyalo-window--post-setup)
    (if (< attempt hyalo-window--post-setup-max-retries)
        (run-with-timer 0.1 nil #'hyalo-window--wait-for-controller (1+ attempt))
      (message "Hyalo: Window controller not ready after %d retries"
               hyalo-window--post-setup-max-retries))))

(defun hyalo-window--post-setup ()
  "Post-setup: initialize channels, push initial data, start status updates.
Wrapped in condition-case so a failure in one step does not prevent
subsequent steps from running.  Each step logs errors individually."
  (when (fboundp 'hyalo--boot-log)
    (hyalo--boot-log "hyalo-window--post-setup: entered"))
  ;; Load sub-modules
  (when (and (bound-and-true-p hyalo--needs-bootstrap) (fboundp 'hyalo-set-loading-message)) (hyalo-set-loading-message "Loading modules…") (sit-for 0.01))
  (condition-case err
      (progn
        (require 'hyalo-channels)
        (require 'hyalo-navigator)
        (require 'hyalo-status)
        (require 'hyalo-appearance)
        (require 'hyalo-diagnostics))
    (error (message "Hyalo: Failed to load sub-modules: %s" (error-message-string err))))
  ;; Open all async channels
  (when (and (bound-and-true-p hyalo--needs-bootstrap) (fboundp 'hyalo-set-loading-message)) (hyalo-set-loading-message "Opening channels…") (sit-for 0.01))
  (condition-case err
      (hyalo-channels-setup)
    (error (message "Hyalo: Channel setup error: %s" (error-message-string err))))
  ;; Push initial data to the UI
  (when (and (bound-and-true-p hyalo--needs-bootstrap) (fboundp 'hyalo-set-loading-message)) (hyalo-set-loading-message "Refreshing navigator…") (sit-for 0.01))
  (condition-case err
      (hyalo-navigator-refresh)
    (error (message "Hyalo: Navigator refresh error: %s" (error-message-string err))))
  ;; Seed initial project root for change detection and cache.
  ;; Only directories with a git root get a project root — bare
  ;; directories (e.g. ~/) must not be pushed to the navigator,
  ;; because the Swift file tree builder would scan recursively.
  (let* ((dir (or (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (and default-directory
                       (expand-file-name default-directory))))
         (git-root (when dir (locate-dominating-file dir ".git"))))
    (if git-root
        (let ((expanded (expand-file-name git-root)))
          (setq hyalo-status--last-project-root expanded)
          (setq hyalo-status--last-git-root expanded)
          (puthash dir expanded hyalo-status--project-root-cache))
      ;; No git repo — record :none so the cache prevents future walks,
      ;; but do NOT set a project root for the navigator.
      (when dir
        (setq hyalo-status--last-project-root nil)
        (setq hyalo-status--last-git-root nil)
        (puthash dir :none hyalo-status--project-root-cache))))
  ;; Register hook-driven status updates (no polling)
  (condition-case err
      (progn
        (hyalo-status-setup)
        ;; Push initial branch info and file info (one-time synchronous, at startup only)
        (hyalo-status--push-branch-info)
        (hyalo-status--push-file-info))
    (error (message "Hyalo: Status setup error: %s" (error-message-string err))))
  ;; Push initial source control data (changed files + commit history)
  (condition-case err
      (when (fboundp 'hyalo-source-control--do-update)
        (hyalo-source-control--do-update))
    (error (message "Hyalo: Source control update error: %s" (error-message-string err))))
  ;; Source control updates on save (debounced 3s)
  (add-hook 'after-save-hook #'hyalo-source-control--schedule-update)
  ;; Register hook-driven diagnostics updates (flymake → Swift)
  (condition-case err
      (hyalo-diagnostics-setup)
    (error (message "Hyalo: Diagnostics setup error: %s" (error-message-string err))))
  ;; Sync appearance with current Emacs theme
  (condition-case err
      (hyalo-appearance-sync)
    (error (message "Hyalo: Appearance sync error: %s" (error-message-string err))))
  ;; Push initial terminal palette and color theme (theme loaded before hook registered)
  (condition-case err
      (progn
        (when (fboundp 'hyalo-theme-send-palette)
          (hyalo-theme-send-palette))
        (when (fboundp 'hyalo-theme-send-color-theme)
          (hyalo-theme-send-color-theme))
        ;; Push current theme name (theme loaded before module, so hyalo-theme--on-enable skipped it)
        (when (and (fboundp 'hyalo-set-current-theme-name) custom-enabled-themes)
          (hyalo-set-current-theme-name (symbol-name (car custom-enabled-themes)))))
    (error (message "Hyalo: Theme push error: %s" (error-message-string err))))
  ;; Register multi-frame hooks
  (add-hook 'after-make-frame-functions #'hyalo-window--on-frame-created)
  (add-hook 'delete-frame-functions #'hyalo-window--on-frame-deleted)
  ;; Setup Cmd+O / Cmd+P interception
  (condition-case err
      (hyalo-window--setup-keybindings)
    (error (message "Hyalo: Keybindings setup error: %s" (error-message-string err))))
  ;; Close the proxy window (Swift side).  Then reveal the Emacs frame via
  ;; Emacs's own make-frame-visible so Emacs updates its internal visibility
  ;; state before AppKit fires display callbacks.  Calling makeKeyAndOrderFront
  ;; directly from Swift (as hyalo-loading-done used to do) bypasses that state
  ;; update and causes a segfault in the NS display code.
  (if (fboundp 'hyalo-loading-done)
      (hyalo-loading-done)
    ;; Module not available — nothing to close.
    nil)
  (make-frame-visible)
  (message "Hyalo: IDE shell initialized (v%s)"
           (or (ignore-errors (hyalo-version-check)) "?")))

;;; Multi-Frame Support

(defun hyalo-window--decoratable-frame-p (frame)
  "Return non-nil if FRAME should receive Hyalo decoration.
Excludes child frames, terminal frames, and daemon frames."
  (and (display-graphic-p frame)
       (eq (framep frame) 'ns)
       (not (frame-parameter frame 'parent-frame))))

(defun hyalo-window--on-frame-created (frame)
  "Hook for `after-make-frame-functions'.
Decorate FRAME with the Hyalo IDE shell if eligible.
The frame starts invisible via `default-frame-alist'.  The Swift
side reveals the window after setup() installs the chrome."
  (when (and (hyalo-available-p)
             (hyalo-window--decoratable-frame-p frame)
             (fboundp 'hyalo-decorate-frame))
    (let ((frame-id (string-to-number
                     (or (frame-parameter frame 'window-id) "0"))))
      (when (> frame-id 0)
        (hyalo-decorate-frame frame-id)
        ;; Apply appearance settings to the new frame
        (hyalo-appearance--setup-frame frame)
        (message "Hyalo: Decorated frame %s (frame-id %d)" frame frame-id)))))

(defun hyalo-window--on-frame-deleted (frame)
  "Hook for `delete-frame-functions'.
Remove Hyalo decoration from FRAME before it is destroyed."
  (when (and (hyalo-available-p)
             (hyalo-window--decoratable-frame-p frame)
             (fboundp 'hyalo-undecorate-frame))
    (let ((frame-id (string-to-number
                     (or (frame-parameter frame 'window-id) "0"))))
      (when (> frame-id 0)
        (hyalo-undecorate-frame frame-id)
        (message "Hyalo: Undecorated frame %s (frame-id %d)" frame frame-id)))))

;;; Keybindings (P8: intercept Cmd+O and Cmd+P)

(defun hyalo-window--setup-keybindings ()
  "Setup macOS-style keybindings for Hyalo.
Override Cmd+O (ns-open-file-using-panel) and Cmd+P (ns-print-buffer)
with Open Quickly and Command Palette respectively.
Setup Xcode-like Cmd-0..4 navigator and Cmd-Option-0..3 inspector shortcuts."
  (when (and (hyalo-available-p) (fboundp 'hyalo-show-open-quickly))
    ;; Override NS functions directly using advice
    (when (fboundp 'ns-open-file-using-panel)
      (advice-add 'ns-open-file-using-panel :override #'hyalo/open-quickly))
    (when (fboundp 'ns-print-buffer)
      (advice-add 'ns-print-buffer :override #'hyalo/command-palette))
    ;; Also bind in global keymap as fallback
    (keymap-global-set "s-o" #'hyalo/open-quickly)
    (keymap-global-set "s-p" #'hyalo/command-palette)
    ;; Navigator: Cmd-0 toggle, Cmd-1..4 tabs
    (keymap-global-set "s-0" #'hyalo-toggle-navigator)
    (keymap-global-set "s-1" #'hyalo-navigator-select-tab-1)
    (keymap-global-set "s-2" #'hyalo-navigator-select-tab-2)
    (keymap-global-set "s-3" #'hyalo-navigator-select-tab-3)
    (keymap-global-set "s-4" #'hyalo-navigator-select-tab-4)
    ;; Inspector: Cmd-Option-0 toggle, Cmd-Option-1..3 tabs
    (keymap-global-set "M-s-0" #'hyalo-toggle-inspector)
    (keymap-global-set "M-s-1" #'hyalo-inspector-select-tab-1)
    (keymap-global-set "M-s-2" #'hyalo-inspector-select-tab-2)
    (keymap-global-set "M-s-3" #'hyalo-inspector-select-tab-3)
    ;; Utility area: Cmd-Option-Shift-0 toggle, Cmd-Option-Shift-1 terminal, Cmd-Option-Shift-2 diagnostics
    (keymap-global-set "M-s-)" #'hyalo-toggle-utility-area)
    (keymap-global-set "M-s-!" #'hyalo-utility-select-tab-1)
    (keymap-global-set "M-s-@" #'hyalo-utility-select-tab-2)
    ;; Delayed setup in case NS functions load after us
    (run-with-timer 2 nil #'hyalo-window--setup-ns-advice-delayed)
    (message "Hyalo: Keybindings installed (Cmd+O/P, Cmd+0-4, Cmd+Opt+0-3, Cmd+Opt+Shift+0-2)")))

(defun hyalo-window--setup-ns-advice-delayed ()
  "Delayed setup of NS function advice."
  (when (and (hyalo-available-p) (fboundp 'hyalo-show-open-quickly))
    (when (and (fboundp 'ns-open-file-using-panel)
               (not (advice-member-p #'hyalo/open-quickly 'ns-open-file-using-panel)))
      (advice-add 'ns-open-file-using-panel :override #'hyalo/open-quickly))
    (when (and (fboundp 'ns-print-buffer)
               (not (advice-member-p #'hyalo/command-palette 'ns-print-buffer)))
      (advice-add 'ns-print-buffer :override #'hyalo/command-palette))))

(defun hyalo-window--teardown-keybindings ()
  "Remove Hyalo keybindings."
  (when (fboundp 'ns-open-file-using-panel)
    (advice-remove 'ns-open-file-using-panel #'hyalo/open-quickly))
  (when (fboundp 'ns-print-buffer)
    (advice-remove 'ns-print-buffer #'hyalo/command-palette))
  (dolist (key '("s-o" "s-p"
                 "s-0" "s-1" "s-2" "s-3" "s-4"
                 "M-s-0" "M-s-1" "M-s-2" "M-s-3"
                 "M-s-)" "M-s-!" "M-s-@"))
    (ignore-errors (keymap-global-unset key))))

;;; Open Quickly Command (Cmd+O)

(defun hyalo/open-quickly ()
  "Show the Open Quickly panel for fast file navigation.
Collects project files and opens the Swift panel."
  (interactive)
  (if (and (hyalo-available-p)
           (fboundp 'hyalo-show-open-quickly)
           (boundp 'hyalo-channels--initialized)
           hyalo-channels--initialized)
      (progn
        (let ((files (hyalo-window--collect-project-files)))
          (when (and files (fboundp 'hyalo-update-open-quickly-items))
            (hyalo-update-open-quickly-items (json-encode files))))
        (hyalo-show-open-quickly))
    (call-interactively 'find-file)))

;;; Command Palette Command (Cmd+P)

(defun hyalo/command-palette ()
  "Show the Command Palette for quick command execution.
Collects Emacs commands and opens the Swift panel."
  (interactive)
  (if (and (hyalo-available-p)
           (fboundp 'hyalo-show-command-palette)
           (boundp 'hyalo-channels--initialized)
           hyalo-channels--initialized)
      (progn
        (let ((commands (hyalo-window--collect-commands)))
          (when (and commands (fboundp 'hyalo-update-command-list))
            (hyalo-update-command-list (json-encode commands))))
        (hyalo-show-command-palette))
    (call-interactively 'execute-extended-command)))

;;; Data Collectors

(defun hyalo-window--get-project-root ()
  "Get project root: project.el root, git root, file directory, or `default-directory'."
  (or (when-let* ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          (car (project-roots proj))))
      (locate-dominating-file default-directory ".git")
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun hyalo-window--collect-project-files ()
  "Collect files in the current project for Open Quickly."
  (let ((root (hyalo-window--get-project-root)))
    (when root
      (condition-case nil
          (let* ((default-directory root)
                 (files (split-string
                         (shell-command-to-string
                          "git ls-files 2>/dev/null || find . -type f -not -path '*/\\.*' -not -path '*/node_modules/*' | head -1000")
                         "\n" t))
                 (result nil))
            (dolist (file files)
              (when (and (> (length file) 0)
                         (not (string-match-p "^\\." file)))
                (let* ((full-path (expand-file-name file root))
                       (name (file-name-nondirectory file))
                       (icon (cond
                              ((string-match-p "\\.swift$" name) "swift")
                              ((string-match-p "\\.el$" name) "doc.text")
                              ((string-match-p "\\.md$" name) "doc.plaintext")
                              ((string-match-p "\\.json$" name) "doc.text")
                              (t "doc"))))
                  (push `((name . ,name)
                          (path . ,full-path)
                          (relativePath . ,file)
                          (icon . ,icon))
                        result))))
            (nreverse result))
        (error nil)))))

(defun hyalo-window--collect-commands ()
  "Collect available Emacs commands for the Command Palette."
  (let ((commands nil))
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (symbol-name sym)
                  (not (string-match-p "^\\^" (symbol-name sym))))
         (let* ((name (symbol-name sym))
                (doc (documentation sym 'function))
                (desc (if doc
                          (substring doc 0 (min 60 (length doc)))
                        ""))
                (key (where-is-internal sym nil t))
                (keybinding (when key (key-description key)))
                (icon (cond
                       ((string-match-p "file" name) "doc")
                       ((string-match-p "buffer" name) "doc.on.doc")
                       ((string-match-p "window\\|frame" name) "macwindow")
                       ((string-match-p "compile\\|build" name) "hammer")
                       ((string-match-p "search\\|find\\|grep" name) "magnifyingglass")
                       ((string-match-p "git\\|magit" name) "chevron.branch")
                       ((string-match-p "toggle" name) "switch.2")
                       (t "command.square"))))
           (push `((name . ,name)
                   (description . ,desc)
                   (icon . ,icon)
                   (keybinding . ,keybinding)
                   (category . "commands"))
                 commands)))))
    (sort commands (lambda (a b)
                     (string< (cdr (assq 'name a))
                              (cdr (assq 'name b)))))))

;;; Panel Toggles

(defun hyalo-toggle-navigator ()
  "Toggle the navigator (left sidebar) visibility."
  (interactive)
  (when (fboundp 'hyalo-navigator-toggle)
    (hyalo-navigator-toggle)))

(defun hyalo-toggle-inspector ()
  "Toggle the inspector (right sidebar) visibility."
  (interactive)
  (when (fboundp 'hyalo-inspector-toggle)
    (hyalo-inspector-toggle)))

(defun hyalo-toggle-utility-area ()
  "Toggle the utility area (bottom panel) visibility."
  (interactive)
  (when (fboundp 'hyalo-utility-area-toggle)
    (hyalo-utility-area-toggle)))

(defun hyalo-show-navigator ()
  "Show the navigator sidebar."
  (interactive)
  (when (fboundp 'hyalo-navigator-show)
    (hyalo-navigator-show)))

(defun hyalo-hide-navigator ()
  "Hide the navigator sidebar."
  (interactive)
  (when (fboundp 'hyalo-navigator-hide)
    (hyalo-navigator-hide)))

(defun hyalo-show-inspector ()
  "Show the inspector sidebar."
  (interactive)
  (when (fboundp 'hyalo-inspector-show)
    (hyalo-inspector-show)))

(defun hyalo-hide-inspector ()
  "Hide the inspector sidebar."
  (interactive)
  (when (fboundp 'hyalo-inspector-hide)
    (hyalo-inspector-hide)))

;;; Xcode-like Tab Selection (Cmd-0..4, Cmd-Option-0..3)

(defun hyalo-navigator-select-tab-1 ()
  "Select navigator tab 1 (Project). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-navigator-select-tab)
    (hyalo-navigator-select-tab 1)))

(defun hyalo-navigator-select-tab-2 ()
  "Select navigator tab 2 (Buffers). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-navigator-select-tab)
    (hyalo-navigator-select-tab 2)))

(defun hyalo-navigator-select-tab-3 ()
  "Select navigator tab 3 (Source Control). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-navigator-select-tab)
    (hyalo-navigator-select-tab 3)))

(defun hyalo-navigator-select-tab-4 ()
  "Select navigator tab 4 (Search). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-navigator-select-tab)
    (hyalo-navigator-select-tab 4)))

(defun hyalo-inspector-select-tab-1 ()
  "Select inspector tab 1 (File). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-inspector-select-tab)
    (hyalo-inspector-select-tab 1)))

(defun hyalo-inspector-select-tab-2 ()
  "Select inspector tab 2 (History). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-inspector-select-tab)
    (hyalo-inspector-select-tab 2)))

(defun hyalo-inspector-select-tab-3 ()
  "Select inspector tab 3 (Appearance). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-inspector-select-tab)
    (hyalo-inspector-select-tab 3)))

(defun hyalo-utility-select-tab-1 ()
  "Select utility area tab 1 (Terminal). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-utility-area-select-tab)
    (hyalo-utility-area-select-tab 1)))

(defun hyalo-utility-select-tab-2 ()
  "Select utility area tab 2 (Diagnostics). Toggles if already selected."
  (interactive)
  (when (fboundp 'hyalo-utility-area-select-tab)
    (hyalo-utility-area-select-tab 2)))

(provide 'hyalo-window)
;;; hyalo-window.el ends here
