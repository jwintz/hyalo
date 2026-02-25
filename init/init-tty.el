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
  (setq visible-cursor nil))

(provide 'init-tty)
;;; init-tty.el ends here
