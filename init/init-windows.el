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
        shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-select-reused-windows t)

  (setq shackle-rules
        '(
          ;; Ignore async output completely
          ("\\*Async Shell.*\\*" :regexp t :ignore t)

          ;; Help & documentation - bottom popup, select on open
          (help-mode             :align 'below :size 0.42 :select t)
          (helpful-mode          :align 'below :size 0.42 :select t)
          (apropos-mode          :align 'below :size 0.42 :select t)
          ("*Help*"              :align 'below :size 0.42 :select t)
          ("*Apropos*"           :align 'below :size 0.42 :select t)
          ("\\*Man .*"           :regexp t :align 'below :size 0.5)
          ("\\*WoMan .*"         :regexp t :align 'below :size 0.5)

          ;; Info manual
          ("*info*"              :align 'below :size 0.45 :select t)

          ;; Compilation - bottom popup, don't steal focus
          (compilation-mode      :align 'below :size 0.3 :noselect t)
          ("*compilation*"       :align 'below :size 0.3 :noselect t)
          ("*Shell Command*"     :align 'below :size 0.2 :noselect t)
          ("*grep*"              :align 'below :size 0.3 :noselect t)
          ("*Occur*"             :align 'below :size 0.3 :noselect t)

          ;; Completions - small popup, don't select
          ("*Completions*"       :align 'below :size 0.2 :noselect t)

          ;; Version control
          ("*vc-diff*"           :align 'below :size 0.4)
          ("*vc-change-log*"     :align 'below :size 0.3)

          ;; Transient/temporary buffers - don't select
          ("*Messages*"          :align 'below :size 0.3 :noselect t)
          ("*Warnings*"          :align 'below :size 0.25 :noselect t)
          ("*Backtrace*"         :align 'below :size 0.4 :noselect t)

          ;; Flymake/Flycheck diagnostics
          ("\\*Flymake diagnostics.*" :regexp t :align 'below :size 0.3 :noselect t)

          ;; Eldoc (when displayed in separate buffer)
          ("*eldoc*"             :align 'below :size 0.2 :noselect t)

          ;; Magit - display in same window
          (magit-status-mode     :select t :same t)
          (magit-log-mode        :select t :same t)
          (magit-diff-mode       :select t :same t)

          ;; Hyalo build - bottom popup
          ("*hyalo-build*"       :align 'below :size 0.3 :select t)

          ;; Process lists
          ("*Process List*"      :align 'below :size 0.3 :noselect t)

          ;; Buffer list - bottom popup, select on open
          (Buffer-menu-mode      :align 'below :size 0.4 :select t)
          ("*Buffer List*"       :align 'below :size 0.4 :select t)
          (ibuffer-mode          :align 'below :size 0.4 :select t)
          ("*Ibuffer*"           :align 'below :size 0.4 :select t)
          ))

  (shackle-mode 1))

(provide 'init-windows)
;;; init-windows.el ends here
