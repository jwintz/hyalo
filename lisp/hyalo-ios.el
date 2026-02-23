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


;;; iOS Single Dispatch Bridge
;; Implements the Swift -> Emacs dispatch mechanism for iOS

;; Command IDs matching EmacsCommandID in DispatchRouter.swift
(defconst hyalo-ios-command-eval 1)
(defconst hyalo-ios-command-open-file 2)
(defconst hyalo-ios-command-switch-buffer 3)
(defconst hyalo-ios-command-kill-buffer 4)
(defconst hyalo-ios-command-find-file 5)
(defconst hyalo-ios-command-save-buffer 6)
(defconst hyalo-ios-command-execute-command 7)
(defconst hyalo-ios-command-navigator-select 8)
(defconst hyalo-ios-command-status-tap 9)
(defconst hyalo-ios-command-appearance-change 10)
(defconst hyalo-ios-command-search 11)
(defconst hyalo-ios-command-search-navigate 12)
(defconst hyalo-ios-command-diagnostic-navigate 13)
(defconst hyalo-ios-command-package-refresh 14)
(defconst hyalo-ios-command-package-upgrade 15)
(defconst hyalo-ios-command-git-show-commit 16)
(defconst hyalo-ios-command-git-show-diff 17)

;; Declare C functions provided by libemacs.a
(declare-function hyalo-ios-dispatch-raw "hyalo-ios" (command-id json-payload))
(declare-function hyalo-ios-dispatch-response "hyalo-ios" (request-id json-response))
(declare-function hyalo-ios-dispatch-error "hyalo-ios" (request-id error-message))

(defun hyalo-ios-dispatch (command-id payload)
  "Dispatch COMMAND-ID with JSON PAYLOAD to Swift.
COMMAND-ID is an integer matching EmacsCommandID enum.
PAYLOAD is an alist that will be encoded as JSON."
  (let ((json-payload (json-encode payload)))
    (hyalo-log 'ios "Dispatch %d: %s" command-id json-payload)
    (if (fboundp 'hyalo-ios-dispatch-raw)
        (hyalo-ios-dispatch-raw command-id json-payload)
      (hyalo-log 'ios "hyalo-ios-dispatch-raw not available")
      nil)))

(defun hyalo-ios-dispatch-sync (command-id payload)
  "Dispatch COMMAND-ID with PAYLOAD and wait for result.
Returns the parsed JSON response."
  (let ((result nil)
        (done nil))
    (hyalo-single-dispatch
     command-id
     payload
     (lambda (response)
       (setq result response)
       (setq done t)))
    ;; Wait for response with timeout
    (with-timeout (5 (progn
                       (hyalo-log 'ios "Dispatch timeout for %d" command-id)
                       `((success . nil) (error . "Timeout"))))
      (while (not done)
        (accept-process-output nil 0.1)))
    result))

;; Override the raw dispatch function for iOS
(defun hyalo-single-dispatch-raw (command-id payload)
  "iOS implementation of raw dispatch.
Sends the command to Swift via C FFI."
  (let ((json-payload (json-encode payload)))
    (when (fboundp 'hyalo-ios-dispatch-raw)
      (hyalo-ios-dispatch-raw command-id json-payload))))

;;; Command Handlers

(defun hyalo-ios-handle-eval (payload)
  "Handle eval command from Swift.
PAYLOAD contains: code - Elisp code to evaluate."
  (let ((code (cdr (assoc "code" payload))))
    (condition-case err
        (let ((result (eval (read code))))
          `((success . t) (result . ,(format "%S" result))))
      (error
       `((success . nil) (error . ,(error-message-string err)))))))

(defun hyalo-ios-handle-switch-buffer (payload)
  "Handle switch-buffer command from Swift.
PAYLOAD contains: buffer-name - name of buffer to switch to."
  (let ((buffer-name (cdr (assoc "buffer_name" payload))))
    (when buffer-name
      (hyalo-channels--handle-switch-buffer buffer-name))
    `((success . t))))

(defun hyalo-ios-handle-find-file (payload)
  "Handle find-file command from Swift.
PAYLOAD contains: file_path - path of file to open."
  (let ((file-path (cdr (assoc "file_path" payload))))
    (when file-path
      (hyalo-channels--handle-find-file file-path))
    `((success . t))))

(defun hyalo-ios-handle-kill-buffer (payload)
  "Handle kill-buffer command from Swift.
PAYLOAD contains: buffer_name - name of buffer to kill."
  (let ((buffer-name (cdr (assoc "buffer_name" payload))))
    (when buffer-name
      (let ((buf (get-buffer buffer-name)))
        (when buf (kill-buffer buf))))
    `((success . t))))

(defun hyalo-ios-handle-search (payload)
  "Handle search command from Swift.
PAYLOAD contains: query - search string."
  (let ((query (cdr (assoc "query" payload))))
    (when query
      (hyalo-channels--handle-search query))
    `((success . t))))

(defun hyalo-ios-handle-search-navigate (payload)
  "Handle search-navigate command from Swift.
PAYLOAD contains: location - file:line:col string."
  (let ((location (cdr (assoc "location" payload))))
    (when location
      (hyalo-channels--handle-search-navigate location))
    `((success . t))))

(defun hyalo-ios-handle-execute-command (payload)
  "Handle execute-command command from Swift.
PAYLOAD contains: command - command name to execute."
  (let ((command (cdr (assoc "command" payload))))
    (when command
      (hyalo-channels--handle-execute-command command))
    `((success . t))))

(defun hyalo-ios-handle-appearance-change (payload)
  "Handle appearance-change command from Swift.
PAYLOAD contains: mode - light/dark/auto."
  (let ((mode (cdr (assoc "mode" payload))))
    (when mode
      (hyalo-channels--handle-appearance-mode mode))
    `((success . t))))

(defun hyalo-ios-handle-show-commit (payload)
  "Handle git-show-commit command from Swift.
PAYLOAD contains: hash - commit hash."
  (let ((hash (cdr (assoc "hash" payload))))
    (when hash
      (hyalo-channels--handle-show-commit hash))
    `((success . t))))

(defun hyalo-ios-handle-show-diff (payload)
  "Handle git-show-diff command from Swift.
PAYLOAD contains: path - file path."
  (let ((path (cdr (assoc "path" payload))))
    (when path
      (hyalo-channels--handle-show-diff path))
    `((success . t))))

;;; Dispatch Router

(defun hyalo-ios-dispatch-command (command-id payload)
  "Route COMMAND-ID to appropriate handler with PAYLOAD.
Returns JSON-encoded response."
  (let ((result
         (pcase command-id
           (`1 (hyalo-ios-handle-eval payload))
           (`3 (hyalo-ios-handle-switch-buffer payload))
           (`4 (hyalo-ios-handle-kill-buffer payload))
           (`5 (hyalo-ios-handle-find-file payload))
           (`7 (hyalo-ios-handle-execute-command payload))
           (`10 (hyalo-ios-handle-appearance-change payload))
           (`11 (hyalo-ios-handle-search payload))
           (`12 (hyalo-ios-handle-search-navigate payload))
           (`16 (hyalo-ios-handle-show-commit payload))
           (`17 (hyalo-ios-handle-show-diff payload))
           (_ `((success . nil) (error . ,(format "Unknown command: %d" command-id)))))))
    (json-encode result)))

;;; Registration

(defun hyalo-ios-init ()
  "Initialize iOS bridge and register handlers."
  (hyalo-log 'ios "Initializing iOS bridge")
  ;; Register handlers with the single dispatch system
  (hyalo-register-channel-handler "ios" #'hyalo-ios-dispatch-command)
  (hyalo-log 'ios "iOS bridge initialized"))
(provide 'hyalo-ios)
;;; hyalo-ios.el ends here
