;;; init-lang-abc.el --- ABC music notation support -*- lexical-binding: t; -*-

;;; Code:

(use-package abc-mode
  :ensure t
  :vc (:url "https://github.com/mkjunker/abc-mode" :rev :newest)
  :mode "\\.abc\\'")

(provide 'init-lang-abc)
;;; init-lang-abc.el ends here
