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
    (setq hyalo-window--early-setup-done t)))

;;; Core Setup

(defun hyalo-window-setup ()
  "Setup channels, push initial data, start status updates.
Called from `window-setup-hook' after the first frame is ready.
The window decoration was already done by `hyalo-window--early-setup'.
In daemon mode, defers post-setup to the first emacsclient frame."
  (interactive)
  (when (fboundp 'hyalo--boot-log)
    (hyalo--boot-log "hyalo-window-setup: entered"))
  (when (hyalo-available-p)
    (if (daemonp)
        ;; Daemon mode: no frame exists yet.  Register hooks and defer
        ;; post-setup to the first emacsclient frame.
        (progn
          (add-hook 'after-make-frame-functions #'hyalo-window--on-frame-created)
          (add-hook 'delete-frame-functions #'hyalo-window--on-frame-deleted)
          (add-hook 'after-make-frame-functions #'hyalo-window--daemon-first-frame))
      ;; Normal mode: frame exists, proceed as before.
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
        (hyalo-window--wait-for-controller 0)))))

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
        (require 'hyalo-diagnostics)
        (require 'hyalo-environment nil t))
    (error (message "Hyalo: Failed to load sub-modules: %s" (error-message-string err))))
  ;; Open all async channels
  (when (and (bound-and-true-p hyalo--needs-bootstrap) (fboundp 'hyalo-set-loading-message)) (hyalo-set-loading-message "Opening channels…") (sit-for 0.01))
  (condition-case err
      (hyalo-channels-setup)
    (error (message "Hyalo: Channel setup error: %s" (error-message-string err))))
  ;; Initialize environment bridge (user/host + dev envs → breadcrumb)
  (condition-case err
      (when (fboundp 'hyalo-environment-setup)
        (hyalo-environment-setup))
    (error (message "Hyalo: Environment setup error: %s" (error-message-string err))))
  ;; Push initial data to the UI
  (when (and (bound-and-true-p hyalo--needs-bootstrap) (fboundp 'hyalo-set-loading-message)) (hyalo-set-loading-message "Refreshing navigator…") (sit-for 0.01))
  (condition-case err
      (hyalo-navigator-refresh)
    (error (message "Hyalo: Navigator refresh error: %s" (error-message-string err))))
  ;; Register hook-driven status updates (no polling)
  ;; Must be called BEFORE seeding project root, because hyalo-status-setup
  ;; clears cached values to handle session restore scenarios.
  (condition-case err
      (hyalo-status-setup)
    (error (message "Hyalo: Status setup error: %s" (error-message-string err))))
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
          (puthash dir expanded hyalo-status--project-root-cache)
          ;; Push initial project root to Swift (navigator file tree)
          (when (fboundp 'hyalo-navigator-set-project-root)
            (hyalo-navigator-set-project-root expanded)))
      ;; No git repo — record :none so the cache prevents future walks,
      ;; but do NOT set a project root for the navigator.
      (when dir
        (setq hyalo-status--last-project-root nil)
        (setq hyalo-status--last-git-root nil)
        (puthash dir :none hyalo-status--project-root-cache))))
  ;; Push initial branch info and file info (one-time synchronous, at startup only)
  (condition-case err
      (progn
        (hyalo-status--push-branch-info)
        (hyalo-status--push-file-info))
    (error (message "Hyalo: Branch/file info push error: %s" (error-message-string err))))
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
  ;; Push the color theme and current theme name.
  ;; Terminal palette is loaded from iTermColors files in Swift.
  (condition-case err
      (progn
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

(defun hyalo-window--daemon-first-frame (frame)
  "One-shot hook for `after-make-frame-functions' in daemon mode.
When the first emacsclient FRAME arrives, run full post-setup
\(channels, data push, status) and remove this hook."
  (when (hyalo-window--decoratable-frame-p frame)
    (remove-hook 'after-make-frame-functions #'hyalo-window--daemon-first-frame)
    (select-frame frame)
    (hyalo-window--post-setup)))

(defun hyalo-window--decoratable-frame-p (frame)
  "Return non-nil if FRAME should receive Hyalo decoration.
Excludes child frames, terminal frames, and daemon frames."
  (and (display-graphic-p frame)
       (eq (framep frame) 'ns)
       (not (frame-parameter frame 'parent-frame))))

(defun hyalo-window--on-frame-created (frame)
  "Hook for `after-make-frame-functions'.
Decorate FRAME with the Hyalo IDE shell if eligible.
The frame starts invisible via `default-frame-alist'.  After
decoration completes, a timer reveals the frame."
  (when (and (hyalo-available-p)
             (hyalo-window--decoratable-frame-p frame)
             (fboundp 'hyalo-decorate-frame))
    (let ((frame-id (string-to-number
                     (or (frame-parameter frame 'window-id) "0"))))
      (when (> frame-id 0)
        (hyalo-decorate-frame frame-id)
        (hyalo-appearance--setup-frame frame)
        ;; Wait for Swift decoration to complete, then reveal the frame.
        ;; The retry mechanism in decorateFrameWithRetry takes up to 500ms;
        ;; poll at 50ms intervals for up to 1s.
        (hyalo-window--reveal-when-decorated frame frame-id 0)))))

(defun hyalo-window--reveal-when-decorated (frame frame-id attempt)
  "Reveal FRAME once Swift decoration for FRAME-ID is complete.
Polls at 50ms intervals for up to 20 attempts (1s total)."
  (if (and (frame-live-p frame)
           (fboundp 'hyalo-frame-decorated-p)
           (hyalo-frame-decorated-p frame-id))
      (make-frame-visible frame)
    (when (and (< attempt 20) (frame-live-p frame))
      (run-with-timer 0.05 nil
                      #'hyalo-window--reveal-when-decorated
                      frame frame-id (1+ attempt)))))

(defun hyalo-window--on-frame-deleted (frame)
  "Hook for `delete-frame-functions'.
Remove Hyalo decoration from FRAME before it is destroyed."
  (when (and (hyalo-available-p)
             (hyalo-window--decoratable-frame-p frame)
             (fboundp 'hyalo-undecorate-frame))
    (let ((frame-id (string-to-number
                     (or (frame-parameter frame 'window-id) "0"))))
      (when (> frame-id 0)
        (hyalo-undecorate-frame frame-id)))))

;;; Keybindings (P8: intercept Cmd+O and Cmd+P)

(defun hyalo-window--setup-keybindings ()
  "Setup macOS-style keybindings for Hyalo.
Override Cmd+O (ns-open-file-using-panel) and Cmd+P (ns-print-buffer)
with Open Quickly and Command Palette respectively.
Setup Xcode-like Cmd-0..4 navigator and Cmd-Option-0..3 inspector shortcuts."
  (when (hyalo-available-p)
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
    ;; Focus: Cmd-Option-f editor, Cmd-Option-u utility terminal, Cmd-Option-Tab toggle
    (keymap-global-set "M-s-f" #'hyalo/focus-emacs)
    (keymap-global-set "M-s-u" #'hyalo/focus-utility)
    (keymap-global-set "M-s-<tab>" #'hyalo/focus-toggle)
    ;; Delayed setup in case NS functions load after us
    (run-with-timer 2 nil #'hyalo-window--setup-ns-advice-delayed)))

(defun hyalo-window--setup-ns-advice-delayed ()
  "Delayed setup of NS function advice."
  (when (hyalo-available-p)
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
                 "M-s-)" "M-s-!" "M-s-@"
                 "M-s-f" "M-s-u" "M-s-<tab>"))
    (ignore-errors (keymap-global-unset key))))

;;; Open Quickly Command (Cmd+O)

(defun hyalo/open-quickly ()
  "Open a file using the native Swift minibuffer panel.
Delegates to `find-file'; the minibuffer bridge (hyalo-minibuffer-mode)
intercepts the minibuffer session and renders it natively."
  (interactive)
  (call-interactively 'find-file))

;;; Command Palette Command (Cmd+P)

(defun hyalo/command-palette ()
  "Execute a command using the native Swift minibuffer panel.
Delegates to `execute-extended-command'; the minibuffer bridge
\(hyalo-minibuffer-mode) intercepts the minibuffer session and
renders it natively."
  (interactive)
  (call-interactively 'execute-extended-command))

;;; Data Collectors (used by minibuffer bridge via Emacs completion)

(defun hyalo-window--get-project-root ()
  "Get project root: project.el root, git root, file directory, or `default-directory'."
  (or (when-let* ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          (car (project-roots proj))))
      (locate-dominating-file default-directory ".git")
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))


;;; Focus Commands

(defun hyalo/focus-emacs ()
  "Move keyboard focus to the main Emacs editor view."
  (interactive)
  (when (fboundp 'hyalo-focus-emacs)
    (hyalo-focus-emacs)))

(defun hyalo/focus-utility ()
  "Move keyboard focus to the utility area terminal, showing it if hidden."
  (interactive)
  (when (fboundp 'hyalo-focus-utility)
    (hyalo-focus-utility)))

(defun hyalo/focus-toggle ()
  "Toggle keyboard focus between the Emacs editor and the utility area terminal."
  (interactive)
  (when (fboundp 'hyalo-focus-toggle)
    (hyalo-focus-toggle)))

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
