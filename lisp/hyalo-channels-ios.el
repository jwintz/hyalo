;;; hyalo-channels-ios.el --- iOS Channel Bridge -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; iOS channel implementation using C FFI instead of dynamic module.
;; Functions are provided by the iOS app via @_cdecl exports.

;;; Code:

;; Declare iOS bridge functions (provided by ChannelBridge.swift)
(declare-function hyalo-ios-setup-channels "hyalo-channels-ios" ())
(declare-function hyalo-ios-navigator-update-buffers "hyalo-channels-ios" (json))
(declare-function hyalo-ios-navigator-set-active-buffer "hyalo-channels-ios" (buffer-name))
(declare-function hyalo-ios-navigator-set-active-file "hyalo-channels-ios" (file-path))
(declare-function hyalo-ios-editor-update-tabs "hyalo-channels-ios" (json))
(declare-function hyalo-ios-editor-select-tab "hyalo-channels-ios" (buffer-name))
(declare-function hyalo-ios-status-update "hyalo-channels-ios" (json))
(declare-function hyalo-ios-appearance-set-mode "hyalo-channels-ios" (mode))

(defun hyalo-channels-setup ()
  "Setup channels for iOS."
  (hyalo-log 'channels "Setting up iOS channels")
  (condition-case err
      (progn
        (hyalo-ios-setup-channels)
        (hyalo-log 'channels "iOS channels initialized"))
    (error
     (hyalo-log 'channels "Failed to setup channels: %s" (error-message-string err)))))

(defun hyalo-channels-teardown ()
  "Teardown channels (NOP on iOS)."
  (hyalo-log 'channels "iOS channels teardown"))

;; Navigator channel functions
(defun hyalo-navigator-update-buffers (json)
  "Update navigator buffers with JSON data."
  (hyalo-ios-navigator-update-buffers json))

(defun hyalo-navigator-set-active-buffer (buffer-name)
  "Set active buffer in navigator."
  (hyalo-ios-navigator-set-active-buffer buffer-name))

(defun hyalo-navigator-set-active-file (file-path)
  "Set active file in navigator."
  (hyalo-ios-navigator-set-active-file file-path))

(defun hyalo-navigator-refresh ()
  "Refresh navigator."
  (hyalo-log 'navigator "Refreshing (iOS)"))

(defun hyalo-navigator-set-project-root (root)
  "Set project root in navigator."
  (hyalo-log 'navigator "Project root: %s" root))

(defun hyalo-set-project-name (name)
  "Set project name."
  (hyalo-log 'project "Name: %s" name))

;; Editor channel functions
(defun hyalo-update-editor-tabs (json)
  "Update editor tabs."
  (hyalo-ios-editor-update-tabs json))

(defun hyalo-select-editor-tab (buffer-name)
  "Select editor tab."
  (hyalo-ios-editor-select-tab buffer-name))

;; Status channel functions
(defun hyalo-status-update (line column percent
                               encoding eol indent
                               file-type file-size
                               modes lhs rhs)
  "Update status bar."
  (let ((json (json-encode
               `((line . ,line)
                 (column . ,column)
                 (percent . ,percent)
                 (encoding . ,encoding)
                 (eol . ,eol)
                 (indent . ,indent)
                 (file-type . ,file-type)
                 (file-size . ,file-size)
                 (modes . ,modes)
                 (lhs . ,lhs)
                 (rhs . ,rhs)))))
    (hyalo-ios-status-update json)))

;; Appearance channel functions
(defun hyalo-set-workspace-appearance (appearance)
  "Set workspace appearance."
  (hyalo-ios-appearance-set-mode appearance))

(defun hyalo-set-background-color (color)
  "Set background color."
  (hyalo-log 'appearance "Background: %s" color))

(defun hyalo-show-appearance-panel ()
  "Show appearance panel."
  (hyalo-log 'appearance "Show panel"))

;; Window functions
(defun hyalo-toggle-navigator ()
  "Toggle navigator."
  (hyalo-log 'window "Toggle navigator"))

(defun hyalo-toggle-inspector ()
  "Toggle inspector."
  (hyalo-log 'window "Toggle inspector"))

(defun hyalo-toggle-utility-area ()
  "Toggle utility area."
  (hyalo-log 'window "Toggle utility area"))

(defun hyalo-minibuffer-show (json)
  "Show the native minibuffer panel.  JSON is the session payload."
  (hyalo-log 'minibuffer "Show: %s" json))

(defun hyalo-minibuffer-update (json)
  "Update the native minibuffer panel.  JSON is the candidate payload."
  (hyalo-log 'minibuffer "Update: %s" json))

(defun hyalo-minibuffer-hide ()
  "Hide the native minibuffer panel."
  (hyalo-log 'minibuffer "Hide"))

;; File info
(defun hyalo-update-file-info (json)
  "Update file info."
  (hyalo-log 'file-info "Update: %s" json))

;; Branch info
(defun hyalo-update-branch-info (json)
  "Update branch info."
  (hyalo-log 'branch "Update: %s" json))

;; Diagnostics
(defun hyalo-update-diagnostics (json)
  "Update diagnostics."
  (hyalo-log 'diagnostics "Update: %s" json))

;; Search
(defun hyalo-update-search-results (json)
  "Update search results."
  (hyalo-log 'search "Results: %s" json))

(defun hyalo-update-search-status (id count)
  "Update search status."
  (hyalo-log 'search "Status: %d files" count))

;; Source control
(defun hyalo-update-changed-files (json)
  "Update changed files."
  (hyalo-log 'source-control "Changed files: %s" json))

(defun hyalo-update-commit-history (json)
  "Update commit history."
  (hyalo-log 'source-control "Commit history: %s" json))

(defun hyalo-update-git-history (json)
  "Update git history."
  (hyalo-log 'source-control "Git history: %s" json))

;; Package
(defun hyalo-update-package-status (json)
  "Update package status."
  (hyalo-log 'package "Status: %s" json))

;; Activity
(defun hyalo-activity-upsert (id title progress message)
  "Upsert activity."
  (hyalo-log 'activity "[%s] %s" id title))

(defun hyalo-activity-finish (id message)
  "Finish activity."
  (hyalo-log 'activity "[%s] Finished: %s" id message))

(defun hyalo-activity-remove-after-delay (id delay)
  "Remove activity after delay."
  (hyalo-log 'activity "[%s] Remove after %.1fs" id delay))

(defun hyalo-activity-append-log (id message)
  "Append log to activity."
  (hyalo-log 'activity "[%s] %s" id message))

;; Environment
(defun hyalo-environment--push ()
  "Push environment data."
  (hyalo-log 'environment "Push"))

(defun hyalo-environment--push-initial ()
  "Push initial environment data."
  (hyalo-log 'environment "Push initial"))

;; Keycast
(defun hyalo-update-keycast (key cmd)
  "Update keycast."
  (hyalo-log 'keycast "%s → %s" key cmd))

(defun hyalo-set-keycast-visible (visible)
  "Set keycast visibility."
  (hyalo-log 'keycast "Visible: %s" visible))

;; Redisplay
(defun hyalo-force-redisplay ()
  "Force redisplay."
  (redisplay t))

(provide 'hyalo-channels-ios)
;;; hyalo-channels-ios.el ends here
