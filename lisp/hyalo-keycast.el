;;; hyalo-keycast.el --- Keycast toolbar pill integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Captures key bindings and command names after each command and pushes
;; them to the Swift toolbar as a Liquid Glass pill.
;;
;; Uses `pre-command-hook' to capture and push immediately.  This
;; ensures multi-key sequences like C-x C-f and C-c p s are displayed
;; at the moment the command is invoked, before any minibuffer
;; interaction can overwrite `this-command' or `this-command-keys'.
;; Only captures at minibuffer-depth 0 to avoid displaying intermediate
;; minibuffer commands (self-insert, completion, exit-minibuffer).
;;
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
    ignore handle-switch-frame
    hyalo-keycast-mode)
  "Commands to suppress from keycast display.
High-frequency or trivial commands that would cause excessive
flicker in the toolbar pill.")

(defun hyalo-keycast--pre-command ()
  "Push the current command and key binding to the Swift toolbar.
Runs in `pre-command-hook' — captures state before the command
executes so minibuffer interactions cannot overwrite the real
command.  Only captures at top level (minibuffer-depth 0)."
  (when (and (= (minibuffer-depth) 0)
             this-command
             (not (memq this-command hyalo-keycast--suppress-commands)))
    (let* ((keys (this-command-keys-vector))
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
        (add-hook 'pre-command-hook #'hyalo-keycast--pre-command)
        (when (fboundp 'hyalo-set-keycast-visible)
          (hyalo-set-keycast-visible t)))
    (remove-hook 'pre-command-hook #'hyalo-keycast--pre-command)
    (when (fboundp 'hyalo-set-keycast-visible)
      (hyalo-set-keycast-visible nil))))

(provide 'hyalo-keycast)
;;; hyalo-keycast.el ends here
