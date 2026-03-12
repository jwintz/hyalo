;;; init-lang-toml.el --- TOML language support -*- lexical-binding: t; -*-

(require 'treesit nil t)

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
  (add-to-list 'major-mode-remap-alist '(toml-mode . toml-ts-mode)))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(provide 'init-lang-toml)
;;; init-lang-toml.el ends here
