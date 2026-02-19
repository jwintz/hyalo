;;; init-modes.el --- Major language modes -*- lexical-binding: t; -*-

;; Language modes for hyalo standalone.
;; Based on emacs.d/init/init-modes.el (with swift-mode moved from tools)
;; Tree-sitter is disabled — standard major modes only.

;;; Code:

;; ============================================================================
;; JSON Mode
;; ============================================================================

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

;; ============================================================================
;; Swift Mode
;; ============================================================================

(use-package swift-mode
  :ensure t
  :defer t
  :mode "\\.swift\\'")

;; ============================================================================
;; TOML Mode
;; ============================================================================

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;; ============================================================================
;; JavaScript/TypeScript Mode (with eglot)
;; ============================================================================

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
  :general
  (:keymaps 'js-mode-map
   :prefix "C-c J"
   "" '(:ignore t :which-key "javascript")))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-mode-hook #'eglot-ensure))

(provide 'init-modes)
;;; init-modes.el ends here
