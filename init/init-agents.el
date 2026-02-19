;;; init-agents.el --- AI agents: copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Backported from emacs.d/init/init-agents.el.
;; Copilot inline completions with diminished mode lighter (nerd-icon glyph).

;;; Code:

(use-package copilot
  :ensure t
  :commands (copilot-mode)
  :init
  (autoload 'copilot-mode "copilot" "Copilot" t)
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :custom
  (copilot-idle-delay 0.2)
  (copilot-indent-offset-warning-disable t)
  (copilot-install-dir (locate-user-emacs-file "copilot/"))
  :config
  (diminish 'copilot-mode (concat " " (nerd-icons-codicon "nf-cod-copilot")))

  (general-def copilot-completion-map
    "<tab>" 'copilot-accept-completion
    "TAB"   'copilot-accept-completion)

  (defun hyalo/copilot-suppress-warnings (orig-fun &rest args)
    "Suppress startup messages from copilot agent."
    (let ((inhibit-message t))
      (apply orig-fun args)))

  (advice-add 'copilot--start-agent :around #'hyalo/copilot-suppress-warnings)
  (advice-add 'copilot--start-server :around #'hyalo/copilot-suppress-warnings)

  (defun hyalo/copilot-redirect-stderr (orig-fun &rest args)
    "Redirect copilot server stderr to a log file."
    (let ((stderr-file (locate-user-emacs-file "copilot-stderr")))
      (if (string= (plist-get args :name) "copilot server")
          (apply orig-fun (plist-put args :stderr stderr-file))
        (apply orig-fun args))))

  (advice-add 'make-process :around #'hyalo/copilot-redirect-stderr)

  (defun hyalo/copilot-suppress-log (orig-fun type msg &rest args)
    "Suppress \"Copilot server started\" message."
    (unless (string-match-p "Copilot server started" msg)
      (apply orig-fun type msg args)))

  (advice-add 'copilot--log :around #'hyalo/copilot-suppress-log))

;;;; Pi Coding Agent (Emacs frontend for pi CLI)

;; Emacs 31 built-in transient has a non-standard version string
;; ("v0.12.0-15-g....-builtin") that breaks version< parsing.
;; Sanitize it BEFORE any package that requires transient is loaded.
;; Must be outside use-package because the error occurs during
;; package-vc--unpack → package--reload-previously-loaded, which
;; loads pi-coding-agent files that call (version< transient-version ...).
;; Use defconst to override even if transient redefines it via defconst.
(when (require 'transient nil t)
  (when (and (boundp 'transient-version)
             (stringp transient-version)
             (not (string-match-p "\\`[0-9]" transient-version)))
    (setq transient-version
          (replace-regexp-in-string "\\`v" "" transient-version))
    (setq transient-version
          (car (split-string transient-version "-")))))

;; pi-coding-agent: Emacs frontend for the pi coding agent CLI.
;; Install from git to avoid MELPA version parsing issues.
(use-package pi-coding-agent
  :ensure t
  :vc (:url "https://github.com/dnouri/pi-coding-agent"
            :rev :newest
            :branch "master")
  :defer t
  :commands (pi-coding-agent)
  :init
  (defalias 'pi 'pi-coding-agent)
  :config
  ;; Apply Monaspace Krypton Frozen to pi chat buffers.
  ;; Buffer-local face remapping avoids affecting editor buffers.
  (defun hyalo/pi-apply-chat-font ()
    "Apply Monaspace Krypton Frozen to pi chat buffer."
    (face-remap-add-relative 'default
                             :family "Monaspace Krypton Frozen"
                             :height 110))

  (add-hook 'pi-coding-agent-chat-mode-hook #'hyalo/pi-apply-chat-font))

(provide 'init-agents)

;;; init-agents.el ends here
