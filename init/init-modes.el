;;; init-modes.el --- Language mode loader -*- lexical-binding: t; -*-

;;; Code:

(use-package treesit-auto
  :ensure t
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  ;; Keep grammar recipes available, but do not auto-remap modes globally.
  ;; The global mode is currently the strongest regression suspect for file-open
  ;; latency, so tree-sitter mode switching remains opt-in for now.
  (setq treesit-auto-install nil
        treesit-auto-langs (delq 'markdown (copy-sequence treesit-auto-langs))))

(require 'init-lang-json)
(require 'init-lang-swift)
(require 'init-lang-toml)
(require 'init-lang-javascript)
(require 'init-lang-yaml)
(require 'init-lang-git)
(require 'init-lang-abc)

(provide 'init-modes)
;;; init-modes.el ends here
