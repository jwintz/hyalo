;;; hyalo-lib.el --- Utility library for Hyalo -*- lexical-binding: t; -*-

;; Transient hooks, first-use hooks, and incremental idle loading.

;;; Code:

(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Transient hooks
;;; --------------------------------------------------------------------------

(defmacro hyalo-add-transient-hook (hook-or-function &rest forms)
  "Attach a self-removing handler to HOOK-OR-FUNCTION.
FORMS are evaluated once when the hook or function is first invoked,
then the handler removes itself.  HOOK-OR-FUNCTION is a quoted hook
variable or a sharp-quoted function (which will be advised)."
  (declare (indent 1))
  (let ((append (when (eq (car forms) :after) (pop forms)))
        (fn (gensym "hyalo-transient-hook-")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S." (if (symbolp hook-or-function)
                                               hook-or-function
                                             (cadr hook-or-function)))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

;;; --------------------------------------------------------------------------
;;; First-use hooks
;;; --------------------------------------------------------------------------

(defvar hyalo-first-input-hook nil
  "Transient hooks run before the first user input after startup.")

(defvar hyalo-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

(defvar hyalo-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")

(defvar hyalo--startup-finished nil
  "Non-nil once the startup sequence is complete.")

(defun hyalo--run-hook-once (hook-var)
  "Run HOOK-VAR exactly once, then reset it to nil."
  (when (and hyalo--startup-finished
             (symbol-value hook-var))
    (let ((gc-cons-threshold most-positive-fixnum))
      (run-hooks hook-var)
      (set hook-var nil))))

(defun hyalo--init-first-hooks ()
  "Wire the first-use hooks to their trigger hooks.
Called once from `emacs-startup-hook'."
  (setq hyalo--startup-finished t)
  ;; first-input: fires on first keypress
  (let ((fn (make-symbol "hyalo--first-input-h")))
    (fset fn (lambda (&rest _)
               (hyalo--run-hook-once 'hyalo-first-input-hook)
               (remove-hook 'pre-command-hook fn)))
    (add-hook 'pre-command-hook fn -101))
  ;; first-buffer: fires on first buffer switch or file open
  (let ((fn (make-symbol "hyalo--first-buffer-h")))
    (fset fn (lambda (&rest _)
               (hyalo--run-hook-once 'hyalo-first-buffer-hook)
               (remove-hook 'find-file-hook fn)
               (remove-hook 'window-buffer-change-functions fn)))
    (add-hook 'find-file-hook fn -101)
    (add-hook 'window-buffer-change-functions fn -101))
  ;; first-file: fires on first file open
  (let ((fn (make-symbol "hyalo--first-file-h")))
    (fset fn (lambda (&rest _)
               (hyalo--run-hook-once 'hyalo-first-file-hook)
               (advice-remove 'after-find-file fn)))
    ;; Use advice on after-find-file because find-file-hook
    ;; fires too late (after mode setup).
    (advice-add 'after-find-file :before fn '((depth . -101)))))

(add-hook 'emacs-startup-hook #'hyalo--init-first-hooks 95)

;;; --------------------------------------------------------------------------
;;; Incremental idle loading
;;; --------------------------------------------------------------------------

(defvar hyalo-incremental-packages '(t)
  "Packages to load incrementally during idle time after startup.
Append to this list via `hyalo-load-packages-incrementally'.
The leading t is a sentinel stripped before processing.")

(defvar hyalo-incremental-first-idle-timer 2.0
  "Seconds of idle time before incremental loading begins.
Set to nil to disable.  Set to 0 to load all immediately.")

(defvar hyalo-incremental-idle-timer 0.75
  "Seconds of idle time between loading successive packages.")

(defun hyalo-load-packages-incrementally (packages &optional now)
  "Register PACKAGES for incremental idle loading.
If NOW is non-nil, begin loading immediately (during idle)."
  (let* ((gc-cons-threshold most-positive-fixnum)
         (first-idle (or hyalo-incremental-first-idle-timer
                         hyalo-incremental-idle-timer)))
    (if (not now)
        (setq hyalo-incremental-packages
              (append hyalo-incremental-packages packages))
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              nil ; already loaded
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) first-idle)
                     (not
                      (while-no-input
                        (let ((default-directory user-emacs-directory)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler
                                            file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "hyalo: failed to incrementally load %S: %s" req e)
               (setq packages nil)))
            (if (null packages)
                nil ; finished
              (run-at-time (if idle-time
                               hyalo-incremental-idle-timer
                             first-idle)
                           nil #'hyalo-load-packages-incrementally
                           packages t)
              (setq packages nil))))))))

(defun hyalo--start-incremental-loading-h ()
  "Begin incrementally loading packages from `hyalo-incremental-packages'."
  (when (numberp hyalo-incremental-first-idle-timer)
    (let ((packages (cdr hyalo-incremental-packages))) ; strip sentinel
      (if (zerop hyalo-incremental-first-idle-timer)
          (mapc #'require packages)
        (run-with-idle-timer hyalo-incremental-first-idle-timer
                             nil #'hyalo-load-packages-incrementally
                             packages t)))))

(add-hook 'emacs-startup-hook #'hyalo--start-incremental-loading-h 100)

(provide 'hyalo-lib)
;;; hyalo-lib.el ends here
