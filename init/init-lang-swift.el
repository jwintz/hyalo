;;; init-lang-swift.el --- Swift language support -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit nil t)

;; Tree-sitter Swift mode (Emacs 29+)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-language-source-alist
               '(swift "https://github.com/alex-pinkus/tree-sitter-swift")))

;; swift-mode provides the major mode; tree-sitter grammar enhances it
(use-package swift-mode
  :ensure t
  :defer t
  :mode "\\.swift\\'")

(provide 'init-lang-swift)
;;; init-lang-swift.el ends here
