;;; init-lang-yaml.el --- YAML language support -*- lexical-binding: t; -*-

(require 'treesit nil t)

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (let ((yaml-recipe
         (if (fboundp 'treesit-library-abi-version)
             `(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
                    ,(if (< (treesit-library-abi-version) 15) "v0.7.2" "v0.7.0"))
           '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))
    (add-to-list 'treesit-language-source-alist yaml-recipe))
  (when (fboundp 'yaml-ts-mode)
    (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))))

(use-package yaml-mode
  :ensure t
  :mode (("Procfile\\'" . yaml-mode)
         ("\\.ya?ml\\'" . yaml-mode)))

(provide 'init-lang-yaml)
;;; init-lang-yaml.el ends here
