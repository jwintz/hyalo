;;; init-lang-json.el --- JSON language support -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit nil t)

;; Tree-sitter JSON mode (Emacs 29+)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-language-source-alist
               '(json "https://github.com/tree-sitter/tree-sitter-json"))
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode)))

;; Fallback: json-mode for non-treesit or when grammar is missing
(use-package json-mode
  :ensure t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (when (fboundp 'electric-indent-mode)
    (add-hook 'json-mode-hook
              (lambda ()
                (setq-local electric-indent-chars
                            (append "{}:,\n" electric-indent-chars)))))
  :general
  (:keymaps 'json-mode-map
   :prefix "C-c j"
   "" '(:ignore t :which-key "json")
   "p" #'json-mode-show-path
   "t" #'json-toggle-boolean
   "+" #'json-increment-number-at-point
   "-" #'json-decrement-number-at-point))

(use-package json-snatcher
  :ensure t
  :defer t)

(provide 'init-lang-json)
;;; init-lang-json.el ends here
