;;; hyalo-compile.el --- Native compilation status tracking -*- lexical-binding: t; -*-

;;; Commentary:
;; Tracks async native compilation activity and pushes status to the
;; Swift activity viewer via `hyalo-activity-*' functions.
;;
;; Detection:
;; - `comp-files-queue' (pending) + `comp-async-compilations' (active)
;; - `native-comp-async-cu-done-functions' fires per completed file
;; - `native-comp-async-all-done-hook' fires when queue drains
;;
;; The activity viewer shows:
;; - Circular progress + "Native compiling N files..." while active
;; - Auto-removes after 8 seconds when idle

;;; Code:

(require 'hyalo)

(defvar hyalo-compile--active nil
  "Non-nil when native compilation is active.")

(defvar hyalo-compile--total 0
  "Total files queued at the start of a compilation batch.")

(defvar hyalo-compile--done 0
  "Files completed in the current compilation batch.")

(defconst hyalo-compile--activity-id "native-compilation"
  "Stable activity ID for native compilation.")

(defun hyalo-compile--queue-length ()
  "Return the number of files queued or actively compiling."
  (+ (if (bound-and-true-p comp-files-queue)
         (length comp-files-queue)
       0)
     (if (bound-and-true-p comp-async-compilations)
         (hash-table-count comp-async-compilations)
       0)))

(defvar hyalo-compile--quiet-threshold 2
  "Minimum file count before showing native compilation activity.
A single file compiling at startup is normal background work and
not worth showing in the activity viewer.")

(defun hyalo-compile--update-status ()
  "Push current compilation status to the Swift activity viewer.
Only shows the activity when the queue has at least
`hyalo-compile--quiet-threshold' files, to avoid noise from
single-file background compilations at startup."
  (let ((remaining (hyalo-compile--queue-length)))
    (cond
     ;; Compilation just started (only show if above threshold)
     ((and (> remaining 0) (not hyalo-compile--active))
      (setq hyalo-compile--total (+ remaining hyalo-compile--done)
            hyalo-compile--done 0)
      (when (> remaining hyalo-compile--total)
        (setq hyalo-compile--total remaining))
      (when (>= hyalo-compile--total hyalo-compile--quiet-threshold)
        (setq hyalo-compile--active t)
        (when (fboundp 'hyalo-activity-clear-log)
          (hyalo-activity-clear-log hyalo-compile--activity-id))
        (hyalo-compile--push-building remaining)))
     ;; Compilation ongoing
     ((and (> remaining 0) hyalo-compile--active)
      (when (> (+ remaining hyalo-compile--done) hyalo-compile--total)
        (setq hyalo-compile--total (+ remaining hyalo-compile--done)))
      (hyalo-compile--push-building remaining))
     ;; Compilation finished
     (hyalo-compile--active
      (hyalo-compile--push-idle)))))

(defun hyalo-compile--push-building (remaining)
  "Push building state with REMAINING file count to activity viewer."
  (when (fboundp 'hyalo-activity-upsert)
    (let ((progress (if (> hyalo-compile--total 0)
                        (/ (float hyalo-compile--done) hyalo-compile--total)
                      nil)))
      (hyalo-activity-upsert
       hyalo-compile--activity-id
       "native-compilation"
       (format "Native compiling %d file%s…"
               remaining (if (= remaining 1) "" "s"))
       nil     ;; message
       progress
       t))))   ;; is-active

(defun hyalo-compile--push-idle ()
  "Push idle state to activity viewer."
  (let ((done hyalo-compile--done))
    (setq hyalo-compile--active nil
          hyalo-compile--total 0
          hyalo-compile--done 0)
    (when (fboundp 'hyalo-activity-finish)
      (hyalo-activity-finish
       hyalo-compile--activity-id
       (if (> done 0)
           (format "Compiled %d file%s" done (if (= done 1) "" "s"))
         "All packages natively compiled")))
    (when (fboundp 'hyalo-activity-remove-after-delay)
      (hyalo-activity-remove-after-delay hyalo-compile--activity-id 8.0))))

(defun hyalo-compile--on-cu-done (file)
  "Handle completion of a single native compilation unit.
FILE is the path to the produced .eln file."
  (setq hyalo-compile--done (1+ hyalo-compile--done))
  ;; Push the compiled file name to the log
  (when (and (fboundp 'hyalo-activity-append-log) file)
    (hyalo-activity-append-log
     hyalo-compile--activity-id
     (file-name-nondirectory (if (stringp file) file (format "%s" file)))))
  (hyalo-compile--update-status))

(defun hyalo-compile--on-all-done ()
  "Handle completion of all queued native compilations."
  (hyalo-compile--push-idle))

;;; Setup

(defun hyalo-compile-setup ()
  "Setup native compilation status tracking."
  (when (and (featurep 'native-compile)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    ;; Per-file completion hook
    (add-hook 'native-comp-async-cu-done-functions
              #'hyalo-compile--on-cu-done)
    ;; All-done hook
    (add-hook 'native-comp-async-all-done-hook
              #'hyalo-compile--on-all-done)
    ;; Check if compilation is already in progress (e.g. from startup)
    (run-with-timer 2 nil #'hyalo-compile--update-status)))

(provide 'hyalo-compile)
;;; hyalo-compile.el ends here
