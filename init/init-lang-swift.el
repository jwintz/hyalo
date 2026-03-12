;;; init-lang-swift.el --- Swift language support -*- lexical-binding: t; -*-

(require 'treesit nil t)

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-language-source-alist
               '(swift "https://github.com/alex-pinkus/tree-sitter-swift")))

(use-package swift-mode
  :ensure t
  :defer t
  :mode "\\.swift\\'")

(provide 'init-lang-swift)
;;; init-lang-swift.el ends here
