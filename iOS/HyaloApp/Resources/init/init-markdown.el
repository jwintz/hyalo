;;; init-markdown.el --- Markdown and knowledge management -*- lexical-binding: t; -*-

;; Markdown support for hyalo standalone.
;; Based on emacs.d/init/init-markdown.el (without svg-lib, hyalo-markdown-mode)

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (defun hyalo-markdown--setup-pre-face ()
    "Set subtle background for markdown pre blocks based on theme.
In terminal mode with transparency, skip setting background."
    (let* ((bg (face-background 'default))
           ;; Skip if background is unspecified (terminal transparency)
           (subtle-bg (when (and bg
                                 (not (eq bg 'unspecified))
                                 (not (equal bg "unspecified-bg"))
                                 (not (string-match-p "unspecified" bg)))
                        (if (eq (frame-parameter nil 'background-mode) 'dark)
                            (color-lighten-name bg 5)
                          (color-darken-name bg 3)))))
      (set-face-attribute 'markdown-pre-face nil
                          :background (or subtle-bg 'unspecified)
                          :extend t)))
  (add-hook 'enable-theme-functions (lambda (&rest _) (hyalo-markdown--setup-pre-face)))
  (hyalo-markdown--setup-pre-face)
  :general
  (:keymaps 'markdown-mode-map
   :prefix "C-c m"
   "" '(:ignore t :which-key "markdown")
   "l" 'markdown-insert-link
   "u" 'markdown-insert-uri
   "f" 'markdown-insert-footnote
   "w" 'markdown-insert-wiki-link
   "c" 'markdown-insert-code
   "C" 'markdown-insert-gfm-code-block
   "p" 'markdown-insert-pre
   "t" 'markdown-insert-table
   "h" 'markdown-insert-header-dwim
   "b" 'markdown-insert-bold
   "i" 'markdown-insert-italic
   "s" 'markdown-insert-strike-through
   "q" 'markdown-insert-blockquote))

(use-package obsidian
  :ensure t
  :defer t
  :commands (obsidian-capture obsidian-jump obsidian-daily-note obsidian-search)
  :init
  (let ((path (expand-file-name "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Vault")))
    (setq obsidian-directory path)
    (setq obsidian--relative-path-length (length (file-name-as-directory path))))

  (with-eval-after-load 'obsidian
    (let ((setter (get 'obsidian-directory 'custom-set)))
      (when setter
        (put 'obsidian-directory 'custom-set
             (lambda (symbol value)
               (let ((inhibit-message t))
                 (funcall setter symbol value)))))))

  :config
  (global-obsidian-mode t)
  (unless (and (boundp 'obsidian-vault-cache) obsidian-vault-cache)
    (when (file-directory-p (expand-file-name obsidian-directory))
      (obsidian-update)))
  :custom
  (obsidian-inbox-directory "Inbox")
  (obsidian-wiki-link-create-file-in-inbox t)
  (markdown-enable-wiki-links t)
  :bind
  (:map obsidian-mode-map
   ("C-c C-o" . obsidian-follow-link-at-point))
  :general
  (:prefix "C-c n"
   "" '(:ignore t :which-key "notes")
   "n" 'obsidian-capture
   "j" 'obsidian-jump
   "d" 'obsidian-daily-note
   "t" 'obsidian-tag-find
   "l" 'obsidian-insert-link
   "w" 'obsidian-insert-wikilink
   "b" 'obsidian-backlink-jump
   "f" 'obsidian-follow-link-at-point
   "s" 'obsidian-search
   "m" 'obsidian-move-file
   "g" 'obsidian-grep
   "v" 'obsidian-jump))

(provide 'init-markdown)
;;; init-markdown.el ends here
