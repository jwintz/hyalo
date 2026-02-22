;;; hyalo-gutter.el --- VC gutter enhancements -*- lexical-binding: t; -*-

;; VC gutter enhancements for hyalo.
;; Provides thin bar fringe indicators and enhanced UX.

;;; Code:

;;
;;; Face Configuration

(defun hyalo-gutter--update-faces (&rest _)
  "Update diff-hl faces from current theme colors.
Sets foreground colors from nano/modus semantic colors and removes backgrounds."
  (let ((insert-fg (or (face-foreground 'success nil t)
                       (face-foreground 'diff-added nil t)
                       "#50a14f"))
        (delete-fg (or (face-foreground 'error nil t)
                       (face-foreground 'diff-removed nil t)
                       "#e45649"))
        (change-fg (or (face-foreground 'warning nil t)
                       (face-foreground 'diff-changed nil t)
                       "#c18401")))
    ;; Set foreground colors explicitly
    (set-face-attribute 'diff-hl-insert nil
                        :foreground insert-fg
                        :background 'unspecified)
    (set-face-attribute 'diff-hl-delete nil
                        :foreground delete-fg
                        :background 'unspecified)
    (set-face-attribute 'diff-hl-change nil
                        :foreground change-fg
                        :background 'unspecified)))

;;
;;; Pretty Fringe Bitmaps (+pretty style)

(defun hyalo-gutter--define-thin-bitmaps (&rest _)
  "Define sleek thin bar bitmaps for diff-hl fringe indicators.
Replaces the default chunky bitmaps with half-width solid bars
similar to VSCode/Sublime Text git-gutter."
  (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                         (numberp text-scale-mode-amount))
                    (expt text-scale-mode-step text-scale-mode-amount)
                  1))
         (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
         (total-spacing (pcase spacing
                          ((pred numberp) spacing)
                          (`(,above . ,below) (+ above below))))
         (h (+ (ceiling (* (frame-char-height) scale))
               (if (floatp total-spacing)
                   (truncate (* (frame-char-height) total-spacing))
                 total-spacing)))
         (w (min (or (frame-parameter nil (intern (format "%s-fringe" diff-hl-side))) 8)
                 (if (boundp 'diff-hl-bmp-max-width) diff-hl-bmp-max-width 8)
                 8)))
    ;; Ensure minimum width
    (when (or (null w) (zerop w))
      (setq w 8))
    ;; Create thin bitmap: half-width filled rectangle
    (define-fringe-bitmap 'diff-hl-bmp-middle
      (make-vector
       h (string-to-number (let ((half-w (max 1 (1- (/ w 2)))))
                             (concat (make-string half-w ?1)
                                     (make-string (- w half-w) ?0)))
                           2))
      nil nil 'center)))

(defun hyalo-gutter--type-at-pos-fn (type _pos)
  "Return bitmap symbol for diff TYPE at position.
Uses thin bar for insert/change, standard delete bitmap if available."
  (if (eq type 'delete)
      ;; For delete, use the original delete bitmap or middle as fallback
      (if (fringe-bitmap-p 'diff-hl-bmp-delete)
          'diff-hl-bmp-delete
        'diff-hl-bmp-middle)
    'diff-hl-bmp-middle))

;;
;;; Update Hooks

(defvar-local hyalo-gutter--last-state nil
  "Last known VC state for debounced gutter updates.")

(defun hyalo-gutter--update (&rest _)
  "Update diff-hl gutter if buffer state has changed.
Debounces updates by comparing current state with last known state.
Returns nil to allow hook chaining."
  (when-let* (((or (bound-and-true-p diff-hl-mode)
                   (bound-and-true-p diff-hl-dir-mode)))
              (file (buffer-file-name (buffer-base-buffer)))
              ;; Debounce: skip if state unchanged
              ((not (equal (cons (point) hyalo-gutter--last-state)
                           (setq hyalo-gutter--last-state
                                 (cons (point)
                                       (copy-sequence
                                        (symbol-plist
                                         (intern (expand-file-name file)
                                                 vc-file-prop-obarray)))))))
              ))
    (ignore (diff-hl-update))))

(defun hyalo-gutter--on-buffer-switch (&rest _)
  "Update gutter when switching buffers.
Returns nil to allow hook chaining."
  (hyalo-gutter--update)
  nil)

(defun hyalo-gutter--on-frame-focus (&rest _)
  "Update gutter when frame receives focus.
Returns nil to allow hook chaining."
  (hyalo-gutter--update)
  nil)

(defun hyalo-gutter--on-exit-insert-state ()
  "Update gutter when exiting evil insert state.
Called from evil-insert-state-exit-hook."
  (when (bound-and-true-p diff-hl-flydiff-mode)
    (diff-hl-flydiff-update)))

;;
;;; Flycheck Integration

(defun hyalo-gutter--setup-flycheck ()
  "Configure flycheck to use right fringe to avoid overlap with diff-hl.
Must be called after flycheck is loaded."
  (setq flycheck-indication-mode 'right-fringe)
  ;; Define a subtle left-pointing arrow for flycheck errors
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

;;
;;; UX Enhancements

(defun hyalo-gutter--shrink-revert-popup (fn &rest args)
  "Advice to shrink revert hunk popup to content size.
FN is the original function, ARGS are its arguments."
  (let ((refine-mode diff-auto-refine-mode)
        (diff-auto-refine-mode t))
    (prog1 (apply fn args)
      (shrink-window-if-larger-than-buffer))))

(defun hyalo-gutter--save-excursion-around-revert (fn &rest args)
  "Preserve cursor position after revert hunk operation.
FN is the original function, ARGS are its arguments."
  (let ((pt (point)))
    (prog1 (apply fn args)
      (goto-char pt))))

(defun hyalo-gutter--silence-tramp-prompts (fn &rest args)
  "Silence TRAMP temp file prompts for diff-hl operations.
FN is the original function, ARGS are its arguments."
  (let ((tramp-allow-unsafe-temporary-files t))
    (apply fn args)))

;;
;;; Setup

;;;###autoload
(defun hyalo-gutter-setup ()
  "Enable and configure diff-hl with enhancements.
Sets up thin fringe bitmaps, transparent faces, and UX improvements.

Note: Fringe configuration must be done separately in init-appearance.el.
This function assumes fringes are already visible."
  ;; Set up face colors before enabling mode
  (hyalo-gutter--update-faces)
  (add-hook 'enable-theme-functions #'hyalo-gutter--update-faces)

  ;; Ensure bitmaps are defined (call original first, then our thin versions)
  (diff-hl-define-bitmaps)
  (hyalo-gutter--define-thin-bitmaps)

  ;; Define thin bitmaps after diff-hl loads (for later resize events)
  (advice-add #'diff-hl-define-bitmaps :after #'hyalo-gutter--define-thin-bitmaps)

  ;; Use thin bitmap function
  (setq diff-hl-fringe-bmp-function #'hyalo-gutter--type-at-pos-fn)
  (setq diff-hl-draw-borders nil)

  ;; Performance settings
  (setq diff-hl-global-modes '(not image-mode pdf-view-mode))
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5)
  (setq diff-hl-update-async (or (> emacs-major-version 30) 'thread))
  (setq diff-hl-show-staged-changes nil)

  ;; UX improvements
  (advice-add #'diff-hl-revert-hunk-1 :around #'hyalo-gutter--shrink-revert-popup)
  (advice-add #'diff-hl-revert-hunk :around #'hyalo-gutter--save-excursion-around-revert)
  (advice-add #'diff-hl-diff-buffer-with-reference :around #'hyalo-gutter--silence-tramp-prompts)

  ;; Flycheck integration (if available)
  (with-eval-after-load 'flycheck
    (hyalo-gutter--setup-flycheck))

  ;; Global mode
  (global-diff-hl-mode 1)

  ;; Flydiff for real-time updates (disable on macOS due to process issues)
  (unless (eq system-type 'darwin)
    (diff-hl-flydiff-mode 1))

  ;; Magit integration
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Update hooks: window switch, frame focus, escape key
  (add-hook 'window-selection-change-functions #'hyalo-gutter--on-buffer-switch)
  (add-hook 'focus-in-hook #'hyalo-gutter--on-frame-focus)

  ;; Evil insert state exit hook (if evil is used)
  (add-hook 'diff-hl-flydiff-mode-hook
            (lambda ()
              (if diff-hl-flydiff-mode
                  (add-hook 'evil-insert-state-exit-hook #'hyalo-gutter--on-exit-insert-state nil t)
                (remove-hook 'evil-insert-state-exit-hook #'hyalo-gutter--on-exit-insert-state t)))))

;;
;;; Commands

;;;###autoload
(defalias 'hyalo-gutter-stage-hunk #'diff-hl-stage-current-hunk
  "Stage the current hunk at point.")

;;;###autoload
(defalias 'hyalo-gutter-next-hunk #'diff-hl-next-hunk
  "Jump to next diff hunk.")

;;;###autoload
(defalias 'hyalo-gutter-previous-hunk #'diff-hl-previous-hunk
  "Jump to previous diff hunk.")

(defvar vc-suppress-confirm)

;;;###autoload
(defun hyalo-gutter-revert-hunk (&optional no-prompt)
  "Revert the current hunk.
With optional NO-PROMPT prefix, suppress confirmation."
  (interactive "P")
  (let ((vc-suppress-confirm (if no-prompt t)))
    (call-interactively #'diff-hl-revert-hunk)))

;;;###autoload
(defun hyalo-gutter-save-and-revert-hunk ()
  "Revert current hunk without confirmation."
  (interactive)
  (hyalo-gutter-revert-hunk t))

;;;###autoload
(defun hyalo-gutter-show-hunk ()
  "Show the current hunk in a popup."
  (interactive)
  (call-interactively #'diff-hl-show-hunk))

;;;###autoload
(defun hyalo-gutter-diagnose ()
  "Diagnose diff-hl gutter issues.
Displays state information to help debug why gutters might not appear."
  (interactive)
  (with-output-to-temp-buffer "*diff-hl-diagnose*"
    (princ "=== diff-hl Gutter Diagnostics ===\n\n")
    (princ (format "global-diff-hl-mode: %s\n" (bound-and-true-p global-diff-hl-mode)))
    (princ (format "diff-hl-mode: %s\n" (bound-and-true-p diff-hl-mode)))
    (princ (format "diff-hl-side: %s\n" diff-hl-side))
    (princ (format "diff-hl-fringe-bmp-function: %s\n" diff-hl-fringe-bmp-function))
    (princ (format "left-fringe (frame param): %s\n" (frame-parameter nil 'left-fringe)))
    (princ (format "right-fringe (frame param): %s\n" (frame-parameter nil 'right-fringe)))
    (princ (format "fringes-outside-margins: %s\n" (default-value 'fringes-outside-margins)))
    (princ (format "display-graphic-p: %s\n" (display-graphic-p)))
    (princ "\n--- Bitmap Status ---\n")
    (dolist (bmp '(diff-hl-bmp-top diff-hl-bmp-middle diff-hl-bmp-bottom diff-hl-bmp-single diff-hl-bmp-delete diff-hl-bmp-i))
      (princ (format "%s: %s\n" bmp (if (fringe-bitmap-p bmp) "defined" "MISSING"))))
    (princ "\n--- Face Colors ---\n")
    (dolist (face '(diff-hl-insert diff-hl-delete diff-hl-change))
      (princ (format "%s: fg=%s bg=%s\n"
                     face
                     (face-foreground face nil t)
                     (face-background face nil t))))
    (princ "\n--- VC State ---\n")
    (princ (format "Buffer file: %s\n" (or (buffer-file-name) "nil")))
    (when (buffer-file-name)
      (princ (format "VC state: %s\n" (vc-state (buffer-file-name))))
      ;; Check git status manually
      (let ((default-directory (file-name-directory (buffer-file-name))))
        (princ (format "Git diff output: %s\n"
                       (if (string-empty-p (shell-command-to-string "git diff --name-only 2>/dev/null"))
                           "no uncommitted changes in cwd"
                         "has uncommitted changes")))))
    (princ "\n--- Overlays in buffer ---\n")
    (let ((count 0))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'diff-hl)
          (setq count (1+ count))))
      (princ (format "diff-hl overlays: %d\n" count)))
    (princ "\nTo force an update, run: M-x diff-hl-update\n")))

(provide 'hyalo-gutter)
;;; hyalo-gutter.el ends here
