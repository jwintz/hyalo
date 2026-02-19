;;; init-tengwar.el --- Tengwar script rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders visible text in Tengwar script using overlays.
;; Silently skipped if hyalo-tengwar is not installed.
;;
;; To install:
;;   git clone https://github.com/jwintz/hyalo-tengwar
;;   cp -r hyalo-tengwar/tecendil/fonts/* ~/Library/Fonts/

;;; Code:

(defcustom hyalo-tengwar-path nil
  "Path to external hyalo-tengwar repository.
If nil, looks for ../hyalo-tengwar relative to user-emacs-directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Custom path"))
  :group 'hyalo)

(defun init-tengwar--detect-path ()
  "Return path to hyalo-tengwar if found."
  (or hyalo-tengwar-path
      (let ((relative-path (expand-file-name "../hyalo-tengwar"
                                             user-emacs-directory)))
        (when (file-directory-p relative-path)
          relative-path))))

;; Only configure if tengwar is found
(when-let ((tengwar-path (init-tengwar--detect-path)))
  (when (file-exists-p (expand-file-name "hyalo-tengwar.el" tengwar-path))
    (add-to-list 'load-path tengwar-path)

    (use-package hyalo-tengwar
      :ensure nil
      :commands (hyalo-tengwar-minor-mode hyalo-tengwar-partial-mode)
      :custom
      (hyalo-tengwar-font "Tengwar Annatar")
      (hyalo-tengwar-font-height 1.3)
      (hyalo-tengwar-language "english")
      (hyalo-tengwar-mode "general-use")
      (hyalo-tengwar-partial-delimiters '("@@" . "@@")))

    (use-package hyalo-tengwar-tutorial
      :ensure nil
      :commands (hyalo/tengwar-tutorial
                 hyalo/tengwar-tutorial-reset
                 hyalo/tengwar-tutorial-lesson)
      :general
      (leader-def
        "T t" '(hyalo/tengwar-tutorial :wk "tengwar tutorial")))))

(provide 'init-tengwar)

;;; init-tengwar.el ends here
