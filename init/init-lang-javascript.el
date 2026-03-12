;;; init-lang-javascript.el --- JavaScript/TypeScript language support -*- lexical-binding: t; -*-

(require 'treesit nil t)

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-language-source-alist
               '(javascript "https://github.com/tree-sitter/tree-sitter-javascript"))
  (add-to-list 'treesit-language-source-alist
               '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                            nil "typescript/src"))
  (add-to-list 'treesit-language-source-alist
               '(tsx "https://github.com/tree-sitter/tree-sitter-typescript"
                     nil "tsx/src"))
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))

(use-package js-mode
  :ensure nil
  :mode ("\\.[mc]?js\\'" . js-mode)
  :mode ("\\.es6\\'" . js-mode)
  :mode ("\\.pac\\'" . js-mode)
  :config
  (when (fboundp 'electric-indent-mode)
    (add-hook 'js-mode-hook
              (lambda ()
                (setq-local electric-indent-chars
                            (append "}):" electric-indent-chars)))))
  (add-hook 'js-mode-hook #'eglot-ensure)
  (add-hook 'js-ts-mode-hook #'eglot-ensure)
  :general
  (:keymaps 'js-mode-map
   :prefix "C-c J"
   "" '(:ignore t :which-key "javascript")))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure))

(provide 'init-lang-javascript)
;;; init-lang-javascript.el ends here
