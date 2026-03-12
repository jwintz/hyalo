;;; init-modes.el --- Language mode loader -*- lexical-binding: t; -*-

;; Loads per-language init files from init/init-lang-*.el.
;; Each file sets up its major mode, tree-sitter grammar (when enabled),
;; LSP server, keybindings, and language-specific settings.
;;
;; To add a new language: create init/init-lang-<name>.el following the
;; pattern in existing files (see ~/Development/doom modules/lang/ for
;; reference).

;;; Code:

;;;; Tree-sitter auto-install

;; treesit-auto automatically installs missing grammars on first use
;; and remaps major modes to their tree-sitter variants.
(use-package treesit-auto
  :ensure t
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;;; Language modules

(require 'init-lang-json)
(require 'init-lang-swift)
(require 'init-lang-toml)
(require 'init-lang-javascript)
(require 'init-lang-git)

(provide 'init-modes)
;;; init-modes.el ends here
