;;; init-completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;; Description: Vertico, Consult, Marginalia, Orderless configuration
;; Part of the Hyalo Emacs configuration
;; On iOS: uses built-in fido-vertical-mode (no external packages available)

;;; Code:

;;;; iOS: Built-in Completion

;; On iOS, external packages (vertico, orderless, etc.) are not bundled.
;; Use Emacs's built-in fido-vertical-mode which provides vertical
;; minibuffer completion similar to vertico, with flex matching.
(when (eq window-system 'ios)
  (fido-vertical-mode 1)
  (setq completions-detailed t))

;;;; Desktop: External Completion Stack

(use-package vertico
  :if (not (eq window-system 'ios))
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :if (not (eq window-system 'ios))
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :if (not (eq window-system 'ios))
  :ensure t
  :custom
  (completion-styles '(basic partial-completion orderless))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package consult
  :if (not (eq window-system 'ios))
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line))
  :config
  (setq consult-preview-key nil))

(use-package embark
  :if (not (eq window-system 'ios))
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect\\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :if (not (eq window-system 'ios))
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Cape (completion-at-point extensions)

;; Adds file path and dabbrev as capf sources for inline completion.
;; No popup — corfu popup is suppressed; only inline preview is shown.
;; Zero startup cost — deferred to first input.
(use-package cape
  :if (not (eq window-system 'ios))
  :ensure t
  :defer t
  :init
  (add-hook 'hyalo-first-input-hook
            (lambda ()
              ;; Prepend so cape sources are tried before mode-specific capfs
              (add-hook 'completion-at-point-functions #'cape-file -90)
              (add-hook 'completion-at-point-functions #'cape-dabbrev -80))))

;;;; Corfu (in-buffer completion)

(defun my/corfu-complete-and-send ()
  "Insert completion and send input if in eshell at end of line."
  (interactive)
  (corfu-insert)
  (when (and (derived-mode-p 'eshell-mode)
             (eolp))
    (eshell-send-input)))

(use-package corfu
  :if (not (eq window-system 'ios))
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-echo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-preview-current t)
  (corfu-min-width 1)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . my/corfu-complete-and-send)
        ([return] . my/corfu-complete-and-send))
  :config
  ;; Suppress the popup frame entirely
  (advice-add #'corfu--popup-show :override #'ignore)
  (advice-add #'corfu--popup-hide :override #'ignore))

;;;; Hyalo Minibuffer Bridge

(require 'hyalo-minibuffer nil t)
(when (and (featurep 'hyalo-minibuffer) (display-graphic-p))
  (hyalo-minibuffer-mode 1))

(provide 'init-completion)
;;; init-completion.el ends here
