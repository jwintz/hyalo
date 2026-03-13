;;; init-modes.el --- Language mode loader -*- lexical-binding: t; -*-

;;; Code:

(use-package treesit-auto
  :ensure t
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  ;; Don't prompt during startup — grammars should be pre-installed.
  (setq treesit-auto-install nil
        treesit-auto-langs (delq 'markdown (copy-sequence treesit-auto-langs)))
  ;; Defer tree-sitter activation to first file open to speed up init.
  (add-hook 'hyalo-first-file-hook #'global-treesit-auto-mode))

(require 'init-lang-json)
(require 'init-lang-swift)
(require 'init-lang-toml)
(require 'init-lang-javascript)
(require 'init-lang-yaml)
(require 'init-lang-git)

(provide 'init-modes)
;;; init-modes.el ends here
