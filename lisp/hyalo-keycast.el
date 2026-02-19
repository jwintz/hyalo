;;; hyalo-keycast.el --- Keycast toolbar pill integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Captures key bindings and command names after each command and pushes
;; them to the Swift toolbar as a Liquid Glass pill.
;;
;; Uses `post-command-hook' to read `this-command' and
;; `(key-description (this-command-keys))' after each command completes.
;; The pill auto-fades after `hyalo-keycast-fade-delay' seconds.
;;
;; Requires the Hyalo dynamic module to be loaded.

;;; Code:

(require 'hyalo)

(defgroup hyalo-keycast nil
  "Keycast display in the Hyalo toolbar."
  :group 'hyalo
  :prefix "hyalo-keycast-")

(defcustom hyalo-keycast-fade-delay 3.0
  "Seconds after the last command before the keycast pill fades."
  :type 'number
  :group 'hyalo-keycast)

(defvar hyalo-keycast--suppress-commands
  '(self-insert-command
    mouse-set-point mouse-drag-region
    mwheel-scroll scroll-up-command scroll-down-command
    ignore handle-switch-frame)
  "Commands to suppress from keycast display.
High-frequency or trivial commands that would cause excessive
flicker in the toolbar pill.")

(defun hyalo-keycast--post-command ()
  "Push the current command and key binding to the Swift toolbar.
Added to `post-command-hook' when `hyalo-keycast-mode' is active."
  (when (and this-command
             (not (memq this-command hyalo-keycast--suppress-commands)))
    (let* ((keys (this-command-keys))
           (key-str (if (and keys (> (length keys) 0))
                        (key-description keys)
                      ""))
           (cmd-str (symbol-name this-command)))
      (when (fboundp 'hyalo-update-keycast)
        (hyalo-update-keycast key-str cmd-str)))))

;;;###autoload
(define-minor-mode hyalo-keycast-mode
  "Show key bindings and commands in the Hyalo toolbar."
  :global t
  :lighter " Keycast"
  :group 'hyalo-keycast
  (if hyalo-keycast-mode
      (progn
        (add-hook 'post-command-hook #'hyalo-keycast--post-command)
        (when (fboundp 'hyalo-set-keycast-visible)
          (hyalo-set-keycast-visible t)))
    (remove-hook 'post-command-hook #'hyalo-keycast--post-command)
    (when (fboundp 'hyalo-set-keycast-visible)
      (hyalo-set-keycast-visible nil))))

(provide 'hyalo-keycast)
;;; hyalo-keycast.el ends here
