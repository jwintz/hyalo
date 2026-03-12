;;; init-lang-toml.el --- TOML language support -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit nil t)

;; Tree-sitter TOML mode (Emacs 29+)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
  (add-to-list 'major-mode-remap-alist '(toml-mode . toml-ts-mode)))

;; Fallback: toml-mode for non-treesit
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(provide 'init-lang-toml)
;;; init-lang-toml.el ends here
