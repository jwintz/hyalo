;;; init-lang-git.el --- Git file mode support -*- lexical-binding: t; -*-

;;; Code:

(use-package git-modes
  :ensure t
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/attributes\\'" . gitattributes-mode)
         ("/\\.gitattributes\\'" . gitattributes-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/\\.gitconfig\\'" . gitconfig-mode)))

(provide 'init-lang-git)
;;; init-lang-git.el ends here
