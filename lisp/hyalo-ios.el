;;; hyalo-ios.el --- iOS compatible Hyalo module loader (no dlopen) -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Version: 2.0.0-ios
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, ios

;;; Commentary:

;; iOS-compatible loader for Hyalo.
;; On iOS, dlopen() is prohibited, so we cannot use dynamic modules.
;; Instead, this file:
;; - Sets hyalo--loaded to t (module functions provided natively)
;; - Implements channel communication via direct C FFI calls
;; - Stubs or reimplements macOS-specific functionality

;;; Code:

(require 'cl-lib)

(defgroup hyalo nil
  "Hyalo IDE shell for Emacs."
  :group 'frames
  :prefix "hyalo-")

;; Mark as loaded - functions will be provided by native iOS bridge
(setq hyalo--loaded t)
(setq hyalo--base-dir (expand-file-name "~"))

(defcustom hyalo-auto-build nil
  "Disabled on iOS - no dynamic module building."
  :type 'boolean
  :group 'hyalo)

;;; Logging

(defvar hyalo-elog nil
  "Hyalo module logger (nil if elog not loaded).")

(defun hyalo-trace (context msg &rest args)
  "Log trace level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-trace))
        (apply #'elog-trace hyalo-elog formatted args)
      nil)))

(defun hyalo-debug (context msg &rest args)
  "Log debug level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-debug))
        (apply #'elog-debug hyalo-elog formatted args)
      nil)))

(defun hyalo-info (context msg &rest args)
  "Log info level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-info))
        (apply #'elog-info hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] %s" out)))))

(defun hyalo-warn (context msg &rest args)
  "Log warning level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-warn))
        (apply #'elog-warn hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] WARNING: %s" out)))))

(defun hyalo-error (context msg &rest args)
  "Log error level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-error))
        (apply #'elog-error hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] ERROR: %s" out)))))

(defun hyalo-log (context msg &rest args)
  "Log MSG with ARGS in CONTEXT."
  (cond
   ((or (string-match-p "ERROR" msg) (string-match-p "failed" msg))
    (apply #'hyalo-error context msg args))
   ((string-match-p "WARNING" msg)
    (apply #'hyalo-warn context msg args))
   (t
    (apply #'hyalo-info context msg args))))

;;; Module Loading (NOP on iOS)

(defun hyalo-load ()
  "Load the hyalo module.
On iOS, this is a no-op - the module is statically linked."
  (interactive)
  (hyalo-log 'core "iOS: Module statically linked, no loading needed")
  t)

(defun hyalo--do-load ()
  "Internal function - NOP on iOS."
  t)

(defun hyalo--find-dylib ()
  "Find dylib - returns nil on iOS."
  nil)

(defun hyalo-build (&optional release)
  "Build module - disabled on iOS."
  (hyalo-log 'core "iOS: Module building disabled")
  nil)

(defun hyalo-rebuild-and-reload ()
  "Rebuild and reload - disabled on iOS."
  (hyalo-log 'core "iOS: Module rebuilding disabled")
  nil)

;;; Module Availability

(defun hyalo-available-p ()
  "Return non-nil if the hyalo module is available.
On iOS, always returns t since module is statically linked."
  t)

(defun hyalo-ensure ()
  "Ensure the module is loaded.
On iOS, always succeeds."
  t)

(defun hyalo-version-check ()
  "Display version information."
  (interactive)
  (message "Hyalo iOS - Statically Linked"))

;;; iOS-Specific Functions
;; These will be implemented as C functions callable from Emacs

;; Placeholder for C FFI functions that will be defined in the iOS app
;; and made available to Emacs

(defmacro hyalo--ios-function (name &optional docstring)
  "Declare an iOS function that will be provided by the native app."
  `(defun ,name (&rest args)
     ,@(when docstring (list docstring))
     (if (fboundp ',name)
         (apply ',name args)
       (hyalo-log 'ios "Function %s not yet implemented" ',name))))

;; Channel setup functions - will be provided by iOS app
(hyalo--ios-function hyalo-setup-navigator-channel
  "Setup navigator channel.")

(hyalo--ios-function hyalo-setup-editor-tab-channel
  "Setup editor tab channel.")

(hyalo--ios-function hyalo-setup-status-channel
  "Setup status channel.")

(hyalo--ios-function hyalo-setup-toolbar-channel
  "Setup toolbar channel.")

(hyalo--ios-function hyalo-setup-command-palette-channel
  "Setup command palette channel.")

(hyalo--ios-function hyalo-setup-search-channel
  "Setup search channel.")

(hyalo--ios-function hyalo-setup-appearance-channel
  "Setup appearance channel.")

(hyalo--ios-function hyalo-setup-diagnostics-channel
  "Setup diagnostics channel.")

(hyalo--ios-function hyalo-setup-package-channel
  "Setup package channel.")

(hyalo--ios-function hyalo-setup-source-control-channel
  "Setup source control channel.")

(hyalo--ios-function hyalo-setup-environment-channel
  "Setup environment channel.")

;; UI update functions
(hyalo--ios-function hyalo-update-editor-tabs
  "Update editor tabs.")

(hyalo--ios-function hyalo-select-editor-tab
  "Select editor tab.")

(hyalo--ios-function hyalo-navigator-update-buffers
  "Update navigator buffers.")

(hyalo--ios-function hyalo-navigator-set-active-buffer
  "Set active buffer in navigator.")

(hyalo--ios-function hyalo-navigator-set-active-file
  "Set active file in navigator.")

(hyalo--ios-function hyalo-navigator-refresh
  "Refresh navigator.")

(hyalo--ios-function hyalo-navigator-set-project-root
  "Set project root in navigator.")

(hyalo--ios-function hyalo-set-project-name
  "Set project name.")

;; Status functions
(hyalo--ios-function hyalo-status-update
  "Update status bar.")

(hyalo--ios-function hyalo-update-file-info
  "Update file info.")

(hyalo--ios-function hyalo-update-branch-info
  "Update branch info.")

(hyalo--ios-function hyalo-update-diagnostics
  "Update diagnostics.")

;; Appearance functions
(hyalo--ios-function hyalo-set-workspace-appearance
  "Set workspace appearance (light/dark).")

(hyalo--ios-function hyalo-set-background-color
  "Set background color.")

(hyalo--ios-function hyalo-show-appearance-panel
  "Show appearance panel.")

;; Window functions
(hyalo--ios-function hyalo-toggle-navigator
  "Toggle navigator visibility.")

(hyalo--ios-function hyalo-toggle-inspector
  "Toggle inspector visibility.")

(hyalo--ios-function hyalo-toggle-utility-area
  "Toggle utility area visibility.")

(hyalo--ios-function hyalo-show-command-palette
  "Show command palette.")

(hyalo--ios-function hyalo-show-open-quickly
  "Show open quickly dialog.")

;; Package functions
(hyalo--ios-function hyalo-update-package-status
  "Update package status.")

(hyalo--ios-function hyalo-activity-upsert
  "Upsert activity.")

(hyalo--ios-function hyalo-activity-finish
  "Finish activity.")

(hyalo--ios-function hyalo-activity-remove-after-delay
  "Remove activity after delay.")

(hyalo--ios-function hyalo-activity-append-log
  "Append log to activity.")

;; Source control functions
(hyalo--ios-function hyalo-update-changed-files
  "Update changed files.")

(hyalo--ios-function hyalo-update-commit-history
  "Update commit history.")

(hyalo--ios-function hyalo-update-git-history
  "Update git history.")

;; Search functions
(hyalo--ios-function hyalo-update-search-results
  "Update search results.")

(hyalo--ios-function hyalo-update-search-status
  "Update search status.")

;; Environment functions
(hyalo--ios-function hyalo-environment--push
  "Push environment data.")

(hyalo--ios-function hyalo-environment--push-initial
  "Push initial environment data.")

;; Keycast functions
(hyalo--ios-function hyalo-update-keycast
  "Update keycast display.")

(hyalo--ios-function hyalo-set-keycast-visible
  "Set keycast visibility.")

;; Redisplay function
(hyalo--ios-function hyalo-force-redisplay
  "Force redisplay.")

;;; iOS Bridge Setup

(defun hyalo-ios-setup-channels ()
  "Setup all channels for iOS.
Called by init-hyalo on iOS."
  (hyalo-log 'ios "Setting up channels")
  (when (fboundp 'hyalo-setup-navigator-channel)
    (hyalo-setup-navigator-channel))
  (when (fboundp 'hyalo-setup-editor-tab-channel)
    (hyalo-setup-editor-tab-channel))
  (when (fboundp 'hyalo-setup-status-channel)
    (hyalo-setup-status-channel))
  (when (fboundp 'hyalo-setup-toolbar-channel)
    (hyalo-setup-toolbar-channel))
  (when (fboundp 'hyalo-setup-command-palette-channel)
    (hyalo-setup-command-palette-channel))
  (when (fboundp 'hyalo-setup-search-channel)
    (hyalo-setup-search-channel))
  (when (fboundp 'hyalo-setup-appearance-channel)
    (hyalo-setup-appearance-channel))
  (when (fboundp 'hyalo-setup-diagnostics-channel)
    (hyalo-setup-diagnostics-channel))
  (when (fboundp 'hyalo-setup-package-channel)
    (hyalo-setup-package-channel))
  (when (fboundp 'hyalo-setup-source-control-channel)
    (hyalo-setup-source-control-channel))
  (when (fboundp 'hyalo-setup-environment-channel)
    (hyalo-setup-environment-channel))
  (hyalo-log 'ios "Channels setup complete"))

;;; Window Setup (Simplified for iOS)

(defun hyalo-window--early-setup ()
  "Early window setup for iOS.
Makes frame visible and sets up basic UI."
  (hyalo-log 'window "iOS early setup")
  (when initial-window-system
    (make-frame-visible)
    (hyalo-log 'window "Frame made visible")))

(defun hyalo-window-setup ()
  "Setup window for iOS.
Called during initialization."
  (hyalo-log 'window "iOS window setup")
  (hyalo-ios-setup-channels)
  (hyalo-log 'window "Setup complete"))

(provide 'hyalo-ios)
;;; hyalo-ios.el ends here
