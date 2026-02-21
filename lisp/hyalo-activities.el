;;; hyalo-activities.el --- Tab-bar / activities breadcrumb bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Bridges Emacs tab-bar-mode (and optionally activities.el) to the
;; Swift breadcrumb bar in the toolbar.
;;
;; The Emacs tab bar itself is hidden (tab-bar-show nil).  The SwiftUI
;; breadcrumb segment replaces it, receiving tab state via
;; hyalo-update-activities and frame state via hyalo-update-frame-list.
;;
;; No polling.  State is pushed from:
;;   - tab-bar-tab-post-open-functions  (new tab created)
;;   - advice on tab-close              (tab closed)
;;   - tab-bar-tab-select-functions     (tab switched)
;;   - after-make-frame-functions       (new frame decorated)
;;   - delete-frame-functions           (frame deleted)
;;   - hyalo-sync--push                 (piggyback on existing sync)

;;; Code:

;; MARK: - Tab State Serialisation

(defun hyalo-activities--tab-list ()
  "Return a JSON-encodable list describing all tabs in the selected frame."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (let* ((tabs (funcall tab-bar-tabs-function))
           (current-name (alist-get 'name (tab-bar--current-tab)))
           (result nil))
      (dolist (tab tabs)
        (let* ((name (alist-get 'name tab))
               (is-current (equal name current-name)))
          (push `((name . ,name)
                  (isCurrent . ,(if is-current t :json-false)))
                result)))
      (nreverse result))))

(defun hyalo-activities--push-tabs ()
  "Push current tab state to Swift."
  (when (fboundp 'hyalo-update-activities)
    (let ((tabs (hyalo-activities--tab-list)))
      (hyalo-update-activities (json-encode (vconcat tabs))))))

;; MARK: - Frame State Serialisation

(defun hyalo-activities--frame-list ()
  "Return a JSON-encodable list of all decorated frames."
  (when (fboundp 'hyalo-decorated-frame-ids)
    (let ((decorated-ids (hyalo-decorated-frame-ids))
          (current-id (string-to-number
                       (or (frame-parameter nil 'window-id) "0")))
          (result nil))
      (dolist (frame (frame-list))
        (let* ((fid (string-to-number
                     (or (frame-parameter frame 'window-id) "0")))
               (name (or (frame-parameter frame 'hyalo-project-name)
                         (frame-parameter frame 'name)
                         "Emacs")))
          (when (member fid decorated-ids)
            (push `((id . ,fid)
                    (name . ,name)
                    (isCurrent . ,(if (= fid current-id) t :json-false)))
                  result))))
      (nreverse result))))

(defun hyalo-activities--push-frames ()
  "Push current frame list to Swift."
  (when (fboundp 'hyalo-update-frame-list)
    (let ((frames (hyalo-activities--frame-list)))
      (hyalo-update-frame-list (json-encode (vconcat frames))))))

;; MARK: - Combined Push (called from hyalo-sync--push)

(defun hyalo-activities--push-state ()
  "Push both tab and frame state to Swift.
Called by `hyalo-sync--push' so it piggybacks on existing hook
infrastructure.  No separate polling or timers."
  (hyalo-activities--push-tabs)
  (hyalo-activities--push-frames))

;; MARK: - Hook Handlers

(defun hyalo-activities--on-tab-open (&rest _)
  "Hook for `tab-bar-tab-post-open-functions': push tab state."
  (hyalo-activities--push-tabs))

(defun hyalo-activities--on-tab-select (&rest _)
  "Hook for `tab-bar-tab-select-functions': push tab state."
  (hyalo-activities--push-tabs))

(defun hyalo-activities--on-frame-created (frame)
  "Hook for `after-make-frame-functions': push frame list."
  ;; Give decorateWindow time to register the frame before pushing.
  (when (and (fboundp 'hyalo-window--decoratable-frame-p)
             (hyalo-window--decoratable-frame-p frame))
    (run-with-timer 0.5 nil #'hyalo-activities--push-frames)))

(defun hyalo-activities--on-frame-deleted (_frame)
  "Hook for `delete-frame-functions': push updated frame list."
  (hyalo-activities--push-frames))

;; MARK: - Advice: tab-close sends a push after closing

(defun hyalo-activities--after-tab-close (&rest _)
  "After-advice for `tab-close': push updated tab state."
  (hyalo-activities--push-tabs))

;; MARK: - Channel Callback Handlers (called from Swift via channel)

(defun hyalo-activities--handle-tab-switch (name)
  "Switch to tab named NAME.  Called from Swift breadcrumb tap."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (if (and (featurep 'activities) (fboundp 'activities-switch))
        ;; activities.el: prefer activities-switch which preserves state
        (let ((activity (gethash name activities--activities nil)))
          (if activity
              (activities-switch activity)
            (tab-bar-switch-to-tab name)))
      (tab-bar-switch-to-tab name)))
  (hyalo-activities--push-tabs))

(defun hyalo-activities--handle-tab-new ()
  "Create a new tab.  Called from Swift 'New Activity...' item."
  (if (and (featurep 'activities) (fboundp 'activities-new))
      (call-interactively #'activities-new)
    (tab-new))
  (hyalo-activities--push-tabs))

(defun hyalo-activities--handle-tab-close (name)
  "Close tab named NAME.  Called from Swift."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (let ((tabs (funcall tab-bar-tabs-function)))
      (when (> (length tabs) 1)
        (tab-bar-switch-to-tab name)
        (tab-close))))
  (hyalo-activities--push-tabs))

(defun hyalo-activities--handle-tab-rename (name new-name)
  "Rename tab named NAME to NEW-NAME.  Called from Swift."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-switch-to-tab name)
    (tab-rename new-name))
  (hyalo-activities--push-tabs))

(defun hyalo-activities--handle-frame-switch (frame-id)
  "Switch to the Hyalo frame with FRAME-ID.  Called from Swift."
  (let ((target (cl-find-if
                 (lambda (f)
                   (= (string-to-number
                       (or (frame-parameter f 'window-id) "0"))
                      frame-id))
                 (frame-list))))
    (when target
      (select-frame-set-input-focus target))))

;; MARK: - Setup

(defun hyalo-activities-setup ()
  "Enable tab-bar-mode and register all activity hooks.
Called from `hyalo-channels-setup' after channels are open."
  ;; Enable tab-bar-mode but keep the native tab bar hidden.
  ;; The SwiftUI breadcrumb replaces it entirely.
  (when (fboundp 'tab-bar-mode)
    (tab-bar-mode 1)
    (setq tab-bar-show nil))
  ;; Rename the initial tab to the project name if available
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (let ((project-name (or (and (boundp 'hyalo-status--last-project-root)
                                 (when hyalo-status--last-project-root
                                   (file-name-nondirectory
                                    (directory-file-name hyalo-status--last-project-root))))
                            nil)))
      (when project-name
        (ignore-errors (tab-rename project-name)))))
  ;; Hooks
  (add-hook 'tab-bar-tab-post-open-functions #'hyalo-activities--on-tab-open)
  (add-hook 'tab-bar-tab-select-functions #'hyalo-activities--on-tab-select)
  (add-hook 'after-make-frame-functions #'hyalo-activities--on-frame-created)
  (add-hook 'delete-frame-functions #'hyalo-activities--on-frame-deleted)
  ;; Advice on tab-close to push state after a tab is closed
  (when (fboundp 'tab-close)
    (advice-add 'tab-close :after #'hyalo-activities--after-tab-close))
  ;; Push initial state
  (hyalo-activities--push-state)
  (message "Hyalo: Activities bridge initialized"))

(defun hyalo-activities-teardown ()
  "Remove activity hooks and advice."
  (remove-hook 'tab-bar-tab-post-open-functions #'hyalo-activities--on-tab-open)
  (remove-hook 'tab-bar-tab-select-functions #'hyalo-activities--on-tab-select)
  (remove-hook 'after-make-frame-functions #'hyalo-activities--on-frame-created)
  (remove-hook 'delete-frame-functions #'hyalo-activities--on-frame-deleted)
  (when (fboundp 'tab-close)
    (advice-remove 'tab-close #'hyalo-activities--after-tab-close)))

(provide 'hyalo-activities)
;;; hyalo-activities.el ends here
