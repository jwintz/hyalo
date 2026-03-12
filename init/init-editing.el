;;; init-editing.el --- Editing: god-mode, windmove, outline -*- lexical-binding: t; -*-

;;; Code:

(use-package display-line-numbers
  :ensure nil
  :diminish display-line-numbers-mode
  :custom
  (display-line-numbers-width 2)
  (display-line-numbers-widen t)
  (display-line-numbers-grow-only t))

(use-package god-mode
  :ensure t
  :bind
  (("C-x C-1" . delete-other-windows)
   ("C-x C-2" . split-window-below)
   ("C-x C-3" . split-window-right)
   ("C-x C-0" . delete-window)
   ("C-x C-o" . other-window))
  :config
  (defun hyalo-god-mode-update-cursor-type ()
    "Change cursor shape based on god-mode state."
    (let ((new-cursor (if god-local-mode 'box '(hbar . 2))))
      (setq cursor-type new-cursor)
      (unless (display-graphic-p)
        (run-with-idle-timer 0.01 nil
          (lambda ()
            (let ((escape-seq (if (eq new-cursor 'box)
                                  "\033[2 q"
                                "\033[4 q")))
              (send-string-to-terminal escape-seq)))))))

  (defun hyalo-god-mode-restore-cursor ()
    "Restore cursor to underline on exit."
    (unless (display-graphic-p)
      (send-string-to-terminal "\033[4 q")))

  (add-hook 'post-command-hook #'hyalo-god-mode-update-cursor-type)
  (add-hook 'buffer-list-update-hook #'hyalo-god-mode-update-cursor-type)
  (add-hook 'window-configuration-change-hook #'hyalo-god-mode-update-cursor-type)
  (add-hook 'kill-emacs-hook #'hyalo-god-mode-restore-cursor))

(use-package move-dup
  :ensure t
  :general
  ("M-<up>"   'move-dup-move-lines-up)
  ("M-<down>" 'move-dup-move-lines-down)
  (:prefix "C-c e"
   "l" 'move-dup-duplicate-down
   "L" 'move-dup-duplicate-up))

(use-package stripspace
  :ensure t
  :diminish stripspace-local-mode
  :hook ((prog-mode text-mode conf-mode) . stripspace-local-mode)
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :custom
  (whitespace-line-column 100)
  :config
  (setq-default whitespace-style
                '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
  (setq-default whitespace-display-mappings
                '((space-mark 32 [183] [46])
                  (newline-mark ?\n [172 ?\n] [36 ?\n])
                  (newline-mark ?\r [182] [35])
                  (tab-mark ?\t [187 ?\t] [62 ?\t])))

  (defun hyalo-set-dimmed-whitespace-faces (&rest _)
    "Dim whitespace faces to 15% of the default foreground color."
    (let* ((fg-color (face-attribute 'default :foreground nil t))
           (bg-color (face-attribute 'default :background nil t))
           (alpha 0.15))
      (when (and (stringp fg-color) (stringp bg-color)
                 (color-name-to-rgb fg-color)
                 (color-name-to-rgb bg-color))
        (let* ((blended-list (color-blend (color-name-to-rgb fg-color)
                                          (color-name-to-rgb bg-color)
                                          alpha))
               (ws-color (apply 'color-rgb-to-hex blended-list)))
          (custom-set-faces
           `(whitespace-newline                ((t (:foreground ,ws-color))))
           `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
           `(whitespace-space                  ((t (:foreground ,ws-color))))
           `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
           `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
           `(whitespace-tab                    ((t (:foreground ,ws-color))))
           `(whitespace-trailing               ((t (:foreground ,ws-color)))))))))

  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions #'hyalo-set-dimmed-whitespace-faces))
  (hyalo-set-dimmed-whitespace-faces)
  :hook
  ((prog-mode conf-mode) . whitespace-mode))

(use-package windmove
  :ensure nil
  :general
  ("s-M-<left>"  'windmove-left
   "s-M-<right>" 'windmove-right
   "s-M-<up>"    'windmove-up
   "s-M-<down>"  'windmove-down))

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces
   '(("TODO"  . (:inherit warning :weight semibold))
     ("FIXME" . (:inherit error :weight semibold))
     ("HACK"  . (:inherit warning :weight semibold))
     ("NOTE"  . (:inherit success :weight semibold))
     ("XXX"   . (:inherit error :weight semibold))))
  :hook
  ((prog-mode conf-mode) . hl-todo-mode))

(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode)
  :general
  (:prefix "C-c o"
   :wk "outline"
   "a" '(outline-show-all :wk "show all")
   "b" '(outline-hide-body :wk "hide body")
   "c" '(outline-hide-entry :wk "hide entry")
   "e" '(outline-show-entry :wk "show entry")
   "l" '(outline-hide-leaves :wk "hide leaves")
   "k" '(outline-show-branches :wk "show branches")
   "q" '(outline-hide-sublevels :wk "hide sublevels")
   "o" '(outline-hide-other :wk "hide other")
   "n" '(outline-next-visible-heading :wk "next")
   "p" '(outline-previous-visible-heading :wk "prev")
   "f" '(outline-forward-same-level :wk "forward")
   "B" '(outline-backward-same-level :wk "backward")
   "u" '(outline-up-heading :wk "up")
   "^" '(outline-move-subtree-up :wk "move up")
   "v" '(outline-move-subtree-down :wk "move down")
   "<" '(outline-promote :wk "promote")
   ">" '(outline-demote :wk "demote")))

(use-package outline-indent
  :ensure t
  :custom
  (outline-indent-ellipsis "...")
  :general
  (:keymaps 'outline-indent-minor-mode-map
   :prefix "C-c o"
   "<up>" '(outline-indent-move-line-up :wk "move line up")
   "<down>" '(outline-indent-move-line-down :wk "move line down")
   "<left>" '(outline-indent-shift-left :wk "shift left")
   "<right>" '(outline-indent-shift-right :wk "shift right"))
  :hook
  ((python-mode . outline-indent-minor-mode)
   (python-ts-mode . outline-indent-minor-mode)
   (yaml-mode . outline-indent-minor-mode)
   (yaml-ts-mode . outline-indent-minor-mode)))

(provide 'init-editing)
;;; init-editing.el ends here
