;;; hyalo-diagnostics.el --- Push flymake diagnostics to Swift UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Collects flymake diagnostics (from eglot or any flymake backend) and
;; pushes them as JSON to the Swift diagnostics panel in the utility area.
;; Uses :after advice on `flymake--handle-report' — no polling.
;; This advice fires each time a backend delivers diagnostics to flymake.

;;; Code:

(require 'json)
(require 'hyalo)

;; Declared in hyalo-status.el — used to suppress re-entry via first-change-hook
(defvar hyalo-sync--inhibit)

(defvar hyalo-diagnostics--timer nil
  "Debounce timer for diagnostics push.
Coalesces rapid diagnostic updates (e.g., during typing) into a single push.")

(defun hyalo-diagnostics-setup ()
  "Register advice and hooks for diagnostics updates."
  (hyalo-diagnostics-teardown)
  ;; Advise flymake's report handler — fires when any backend delivers diagnostics
  (advice-add 'flymake--handle-report :after #'hyalo-diagnostics--on-report)
  ;; Buffer switch diagnostics refresh is triggered explicitly by
  ;; hyalo-status--on-buffer-change (inside its internal-buffer filter).
  ;; NOT registered on window-buffer-change-functions directly — doing so
  ;; caused a feedback loop: push → json-encode → temp buffer → hook → push.
  (hyalo-log 'diagnostics "Setup complete (advice on flymake--handle-report)"))

(defun hyalo-diagnostics-teardown ()
  "Remove advice, hooks, and cancel pending timers."
  (advice-remove 'flymake--handle-report #'hyalo-diagnostics--on-report)
  (remove-hook 'window-buffer-change-functions #'hyalo-diagnostics--on-buffer-change)
  (when hyalo-diagnostics--timer
    (cancel-timer hyalo-diagnostics--timer)
    (setq hyalo-diagnostics--timer nil)))

(defun hyalo-diagnostics--on-report (&rest _)
  "After-advice for `flymake--handle-report'.  Debounces at 300ms."
  (hyalo-diagnostics--schedule-push))

(defun hyalo-diagnostics--on-buffer-change (_frame)
  "Push diagnostics on buffer switch so the panel reflects all open buffers."
  (hyalo-diagnostics--schedule-push))

(defun hyalo-diagnostics--schedule-push ()
  "Schedule a debounced diagnostics push."
  (when hyalo-diagnostics--timer
    (cancel-timer hyalo-diagnostics--timer))
  (setq hyalo-diagnostics--timer
        (run-with-timer 0.3 nil #'hyalo-diagnostics--push)))

(defun hyalo-diagnostics--push ()
  "Collect diagnostics from all flymake-enabled buffers and push to Swift."
  (when (fboundp 'hyalo-update-diagnostics)
    (condition-case err
        (let ((all-diags nil)
              (id 0))
          (dolist (buf (buffer-list))
            (when (and (buffer-live-p buf)
                       (buffer-file-name buf))
              (with-current-buffer buf
                (when (and (bound-and-true-p flymake-mode)
                           (fboundp 'flymake-diagnostics))
                  (let ((diags (flymake-diagnostics)))
                    (when diags
                      (hyalo-log 'diagnostics "Buffer %s: %d diagnostics"
                                 (buffer-name buf) (length diags)))
                    (dolist (diag diags)
                      (setq id (1+ id))
                      (let* ((beg (flymake-diagnostic-beg diag))
                             (severity (hyalo-diagnostics--severity-string
                                       (flymake-diagnostic-type diag)))
                             (msg (flymake-diagnostic-text diag))
                             (backend (flymake-diagnostic-backend diag))
                             (source (if backend (symbol-name backend) "")))
                        (push `((id . ,(number-to-string id))
                                (file . ,(buffer-file-name buf))
                                (line . ,(line-number-at-pos beg t))
                                (column . ,(save-excursion
                                             (goto-char beg)
                                             (current-column)))
                                (severity . ,severity)
                                (message . ,msg)
                                (source . ,source))
                              all-diags))))))))
          (hyalo-log 'diagnostics "Pushing %d total diagnostics to Swift" id)
          ;; Bind hyalo-sync--inhibit so json-encode's temp buffers
          ;; cannot re-enter hyalo-sync--push via first-change-hook.
          (let ((hyalo-sync--inhibit t))
            (hyalo-update-diagnostics
             (json-encode (vconcat (nreverse all-diags))))))
      (error
       (message "Hyalo: diagnostics push error: %s" (error-message-string err))))))

(defun hyalo-diagnostics--severity-string (type)
  "Convert flymake diagnostic TYPE to severity string."
  (cond
   ((eq type :error) "error")
   ((eq type :warning) "warning")
   ((eq type :note) "note")
   ;; eglot uses these symbols
   ((eq type 'eglot-error) "error")
   ((eq type 'eglot-warning) "warning")
   ((eq type 'eglot-note) "note")
   ;; Fallback: check the type name string
   (t (let ((name (if (symbolp type) (symbol-name type) (format "%s" type))))
        (cond
         ((string-match-p "error" name) "error")
         ((string-match-p "warning" name) "warning")
         (t "note"))))))

(provide 'hyalo-diagnostics)
;;; hyalo-diagnostics.el ends here
