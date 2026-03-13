;;; hyalo-profiler.el --- Unified profiling for Hyalo -*- lexical-binding: t; -*-

;; Collects init phase timings and provides reporting commands.
;; Integrates with OSSignposter on the Swift side when the module is loaded.

;;; Code:

(defvar hyalo-profiler--timings nil
  "Alist of (PHASE-NAME . DURATION-SECONDS) collected during init.")

(defvar hyalo-profiler--init-start (float-time)
  "Timestamp when profiling data collection began.")

(defun hyalo-profiler-record (phase duration)
  "Record PHASE with DURATION seconds."
  (push (cons phase duration) hyalo-profiler--timings))

(defun hyalo-profiler--pad (str width)
  "Right-pad STR to WIDTH characters."
  (let ((len (length str)))
    (if (>= len width) str
      (concat str (make-string (- width len) ?\s)))))

(defun hyalo-profiler-report ()
  "Display a formatted buffer showing init phase timings."
  (interactive)
  (if (null hyalo-profiler--timings)
      (message "No profiler timings recorded.")
    (let* ((timings (reverse hyalo-profiler--timings))
           (total (apply #'+ (mapcar #'cdr timings)))
           (max-name-len (apply #'max (mapcar (lambda (e) (length (symbol-name (car e)))) timings)))
           (col (max max-name-len 15))
           (bar-width 30)
           (buf (get-buffer-create "*hyalo-profiler*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Hyalo Init Profiler Report\n")
          (insert "=========================\n\n")
          (insert (format "%s  %8s  %5s  %s\n"
                          (hyalo-profiler--pad "Phase" col) "Time(ms)" "%" ""))
          (insert (make-string (+ col 50) ?-) "\n")
          (dolist (entry timings)
            (let* ((name (symbol-name (car entry)))
                   (dur (cdr entry))
                   (pct (if (> total 0) (* 100.0 (/ dur total)) 0))
                   (bar-len (if (> total 0)
                                (round (* bar-width (/ dur total)))
                              0)))
              (insert (format "%s  %8.1f  %4.1f%%  %s\n"
                              (hyalo-profiler--pad name col)
                              (* dur 1000.0) pct
                              (make-string bar-len ?#)))))
          (insert (make-string (+ col 50) ?-) "\n")
          (insert (format "%s  %8.1f\n"
                          (hyalo-profiler--pad "TOTAL" col) (* total 1000.0)))
          (insert "\n")
          (insert (format "GC runs:   %d\n" gcs-done))
          (insert (format "GC time:   %.1fms\n" (* gc-elapsed 1000.0)))
          (insert (format "Emacs init: %.1fms\n"
                          (* (float-time (time-subtract after-init-time before-init-time)) 1000.0)))
          (goto-char (point-min))
          (special-mode)))
      (display-buffer buf))))

(defun hyalo-profiler-dump-csv ()
  "Export profiler timings to a CSV file in /tmp."
  (interactive)
  (let ((file "/tmp/hyalo-profiler.csv")
        (timings (reverse hyalo-profiler--timings)))
    (with-temp-file file
      (insert "phase,duration_ms,percent\n")
      (let ((total (apply #'+ (mapcar #'cdr timings))))
        (dolist (entry timings)
          (insert (format "%s,%.3f,%.1f\n"
                          (symbol-name (car entry))
                          (* (cdr entry) 1000.0)
                          (if (> total 0) (* 100.0 (/ (cdr entry) total)) 0))))))
    (message "Profiler data written to %s" file)))

(provide 'hyalo-profiler)
;;; hyalo-profiler.el ends here
