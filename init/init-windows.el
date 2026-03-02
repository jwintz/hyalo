;;; init-windows.el --- Window management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Window management using shackle for popup control.
;; Shackle provides a simple, declarative way to manage popup windows
;; without polling or timers - it uses display-buffer-alist hooks only.

;;; Code:

(use-package shackle
  :ensure t
  :demand t
  :config
  (setq shackle-default-rule '(:select t)
        shackle-default-alignment 'bottom
        shackle-default-size 0.4
        shackle-select-reused-windows t)

  (setq shackle-rules
        '(
          ;; Ignore async output completely
          ("\\*Async Shell.*\\*" :regexp t :ignore t)

          ;; Help & documentation - bottom popup, select on open
          (help-mode             :align t :size 0.42 :select t)
          (helpful-mode          :align t :size 0.42 :select t)
          (apropos-mode          :align t :size 0.42 :select t)
          ("*Help*"              :align t :size 0.42 :select t)
          ("*Apropos*"           :align t :size 0.42 :select t)
          ("\\*Man .*"           :regexp t :align t :size 0.5)
          ("\\*WoMan .*"         :regexp t :align t :size 0.5)

          ;; Info manual
          ("*info*"              :align t :size 0.45 :select t)

          ;; Compilation - bottom popup, don't steal focus
          (compilation-mode      :align t :size 0.3 :noselect t)
          ("*compilation*"       :align t :size 0.3 :noselect t)
          ("*Shell Command*"     :align t :size 0.2 :noselect t)
          ("*grep*"              :align t :size 0.3 :noselect t)
          ("*Occur*"             :align t :size 0.3 :noselect t)

          ;; Completions - small popup, don't select
          ("*Completions*"       :align t :size 0.2 :noselect t)

          ;; Version control
          ("*vc-diff*"           :align t :size 0.4)
          ("*vc-change-log*"     :align t :size 0.3)

          ;; Transient/temporary buffers - don't select
          ("*Messages*"          :align t :size 0.3 :noselect t)
          ("*Warnings*"          :align t :size 0.25 :noselect t)
          ("*Backtrace*"         :align t :size 0.4 :noselect t)

          ;; Flymake/Flycheck diagnostics
          ("\\*Flymake diagnostics.*" :regexp t :align t :size 0.3 :noselect t)

          ;; Eldoc (when displayed in separate buffer)
          ("*eldoc*"             :align t :size 0.2 :noselect t)

          ;; Magit - display in same window
          (magit-status-mode     :select t :same t)
          (magit-log-mode        :select t :same t)
          (magit-diff-mode       :select t :same t)

          ;; Hyalo build - bottom popup
          ("*hyalo-build*"       :align t :size 0.3 :select t)

          ;; Process lists
          ("*Process List*"      :align t :size 0.3 :noselect t)
          ))

  (shackle-mode 1))

(provide 'init-windows)
;;; init-windows.el ends here
