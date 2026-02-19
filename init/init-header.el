;;; init-header.el --- File header management -*- lexical-binding: t; -*-

;; Automatic file header creation and update via header2.

;;; Code:

(use-package header2
  :ensure nil  ; vendored in lisp/
  :defer t
  :commands (make-header auto-update-file-header make-box-comment make-divider)
  :hook (write-file-functions . auto-update-file-header)
  :hook (emacs-lisp-mode . auto-make-header)
  :general
  (:prefix "C-c e"
   "m" '(make-header :wk "make header")
   "c" '(make-box-comment :wk "make comment")
   "d" '(make-divider :wk "make divider"))
  :init
  (setq header-copyright-notice
        (concat "Copyright (C) "
                (format-time-string "%Y")
                " Julien Wintz\n"))
  (setq header-author 'user-full-name)
  (setq header-file-name 'buffer-file-name)
  :config
  ;; Header format: filename, description, author, copyright, blank, code
  (setq make-header-hook
        '(header-title
          header-blank
          header-description
          header-blank
          header-author
          header-copyright
          header-blank
          header-end-line
          header-commentary
          header-blank
          header-end-line
          header-code
          header-eof)))

(provide 'init-header)
;;; init-header.el ends here
