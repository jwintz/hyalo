;;; init-modes.el --- Language mode loader -*- lexical-binding: t; -*-

;;; Code:

(use-package treesit-auto
  :ensure t
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  (setq treesit-auto-install 'prompt
        treesit-auto-langs (delq 'markdown (copy-sequence treesit-auto-langs)))
  (global-treesit-auto-mode))

(require 'init-lang-json)
(require 'init-lang-swift)
(require 'init-lang-toml)
(require 'init-lang-javascript)
(require 'init-lang-yaml)
(require 'init-lang-git)

(provide 'init-modes)
;;; init-modes.el ends here
