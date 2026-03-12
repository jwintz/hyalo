;;; init-tty.el --- Terminal Emacs enhancements -*- lexical-binding: t; -*-

;; TTY-specific enhancements for Hyalo.
;; Requires iTerm2: Preferences > General > Selection >
;;   "Applications in terminal may access clipboard" must be enabled

;;; Code:

(when (not window-system)

  ;;; Mouse Support
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)
  (setq mouse-yank-at-point t)

  ;;; Clipboard (OSC-52)
  (use-package clipetty
    :ensure t
    :config
    (global-clipetty-mode +1))

  ;;; Window Title
  (setq xterm-set-window-title t)

  ;;; Cursor
  (setq visible-cursor nil)

  ;;; Modeline
  ;; In GUI mode, hyalo-appearance.el hides the modeline (thin separator)
  ;; because Swift renders the status bar natively.  In TTY mode we need
  ;; the Emacs modeline to show buffer state, position, and mode info.
  ;;
  ;; Inspired by Doom's +light modeline — minimal, no external deps.

  ;; Cleaner EOL labels
  (setq eol-mnemonic-dos  "CRLF"
        eol-mnemonic-mac  "CR"
        eol-mnemonic-unix "LF"
        eol-mnemonic-undecided "??")

  (defun hyalo-tty--modeline-encoding ()
    "Return encoding string, only when non-UTF-8 or non-default EOL."
    (let* ((sys (coding-system-plist buffer-file-coding-system))
           (eol (coding-system-eol-type-mnemonic buffer-file-coding-system))
           (parts nil))
      (unless (equal eol "LF")
        (push eol parts))
      (when (and (plist-get sys :name)
                 (not (memq (plist-get sys :category)
                            '(coding-category-undecided coding-category-utf-8))))
        (push (upcase (symbol-name (plist-get sys :name))) parts))
      (when parts
        (concat " " (string-join (nreverse parts) " ")))))

  (defun hyalo-tty--modeline-vc ()
    "Return VC branch string for the modeline."
    (when (and vc-mode (stringp vc-mode))
      (let ((branch (string-trim (substring-no-properties vc-mode))))
        (when (string-match "^ ?\\(?:Git\\|SVN\\|Hg\\)[:-]\\(.+\\)" branch)
          (concat " " (match-string 1 branch))))))

  (setq-default mode-line-format
    '((:eval (if (buffer-modified-p)
                 (propertize " * " 'face 'error)
               "   "))
      ;; Buffer name — bold when active, modified indicator via face
      (:eval (propertize "%b" 'face (cond
                                      ((buffer-modified-p) '(error bold))
                                      (t 'bold))))
      ;; Read-only indicator
      (buffer-read-only
       (:propertize " RO" face warning))
      ;; Position: line:col percentage
      "  %l:%C %p"
      ;; VC branch
      (:eval (hyalo-tty--modeline-vc))
      ;; Right-align the rest
      (:eval (propertize
              " "
              'display
              `((space :align-to (- (+ right right-fringe right-margin)
                                    ,(string-width
                                      (format-mode-line
                                       '("" mode-name
                                         (:eval (hyalo-tty--modeline-encoding))
                                         "  "))))))))
      ;; Major mode
      mode-name
      ;; Encoding (only non-default)
      (:eval (hyalo-tty--modeline-encoding))
      "  ")))

(provide 'init-tty)
;;; init-tty.el ends here
