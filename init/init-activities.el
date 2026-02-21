;;; init-activities.el --- Activity and workspace switcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures the activities system: Emacs tab-bar-mode provides named
;; persistent window configurations ("activities") surfaced in the
;; SwiftUI breadcrumb toolbar via hyalo-activities.el.
;;
;; activities.el (GNU ELPA) is loaded when available, adding suspend/resume
;; semantics and bookmark persistence.  The system degrades gracefully to
;; raw tab-bar when activities.el is not installed.

;;; Code:

;;;; activities.el (optional)

(use-package activities
  :if (eq window-system 'ns)
  ;; Installed from GNU ELPA when available; silently absent otherwise.
  :demand nil
  :config
  (when (fboundp 'activities-mode)
    (activities-mode 1))
  (when (fboundp 'activities-tabs-mode)
    ;; activities-tabs-mode maps each activity to a tab-bar tab.
    ;; (activities-tabs-mode 1)
    )
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b"   . activities-switch-buffer)
   ("C-x C-a g"   . activities-revert)
   ("C-x C-a l"   . activities-list)))

;;;; Tab Bar Keybindings (raw tab-bar, always available)

;; C-x t prefix is the canonical tab-bar prefix; keep it.
;; Add convenience shortcuts for quick tab switching.
(with-eval-after-load 'tab-bar
  ;; Switch to tab by number with Cmd-Shift-[1..9]
  (setq tab-bar-select-tab-modifiers '(super shift))
  ;; Show tab number hints (used when tab-bar-show is non-nil)
  (setq tab-bar-tab-hints t)
  ;; Name new tabs from the project or default-directory
  (setq tab-bar-tab-name-function #'hyalo-activities--tab-name-function))

(defun hyalo-activities--tab-name-function ()
  "Name a new tab from the current project root or directory."
  (or (and (boundp 'hyalo-status--last-project-root)
           hyalo-status--last-project-root
           (file-name-nondirectory
            (directory-file-name hyalo-status--last-project-root)))
      (file-name-nondirectory
       (directory-file-name default-directory))
      "Tab"))

(provide 'init-activities)
;;; init-activities.el ends here
