;;; init-core.el --- Core packages: general, which-key, diminish -*- lexical-binding: t; -*-

;; Minimal core packages for hyalo standalone.
;; Based on emacs.d/init/init-core.el

;;; Code:

(use-package diminish
  :ensure t
  :demand t)

(use-package general
  :ensure t
  :demand t
  :config
  (general-create-definer leader-def :prefix "C-c")
  (leader-def
    "b" '(:ignore t :wk "buffer")
    "e" '(:ignore t :wk "editor")
    "f" '(:ignore t :wk "file")
    "h" '(:ignore t :wk "help")
    "l" '(:ignore t :wk "hyalo")
    "p" '(:ignore t :wk "project")
    "t" '(:ignore t :wk "toggle")
    "v" '(:ignore t :wk "vcs")
    "v n" '(hyalo-gutter-next-hunk :wk "next hunk")
    "v p" '(hyalo-gutter-previous-hunk :wk "previous hunk")
    "v r" '(hyalo-gutter-revert-hunk :wk "revert hunk")
    "v R" '(hyalo-gutter-save-and-revert-hunk :wk "revert hunk (no confirm)")
    "v S" '(hyalo-gutter-stage-hunk :wk "stage hunk")
    "v =" '(hyalo-gutter-show-hunk :wk "show hunk")))

(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-delay 1.0)
  (which-key-separator "  ")
  (which-key-prefix-prefix "")
  (which-key-inhibit-regexps '("^ESC"))
  :config
  (which-key-mode 1))

(provide 'init-core)
;;; init-core.el ends here
