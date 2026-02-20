;;; init-tools.el --- Development tools: project, magit, diff-hl, eglot -*- lexical-binding: t; no-byte-compile: t; -*-

;; Development tools for hyalo standalone.
;; Based on emacs.d/init/init-tools.el

;;; Code:

;; Guard transient-save-history at exit.  transient registers on
;; kill-emacs-hook at load time, so the guard must run before transient
;; is loaded (otherwise a corrupt in-memory history crashes on exit
;; even if magit was never invoked).  with-eval-after-load fires
;; immediately if transient is already loaded, or defers until it is.
(with-eval-after-load 'transient
  (advice-add 'transient-save-history :around
              (lambda (fn &rest args)
                (condition-case err
                    (progn
                      ;; Validate: drop malformed entries before save
                      (setq transient-history
                            (cl-remove-if-not
                             (lambda (entry)
                               (and (consp entry)
                                    (symbolp (car entry))
                                    (listp (cdr entry))))
                             transient-history))
                      (apply fn args))
                  (error
                   (message "transient: save-history failed — clearing (%s)"
                            (error-message-string err))
                   (setq transient-history nil)
                   (ignore-errors (apply fn args)))))
              '((name . hyalo-transient-save-guard))))

(use-package project
  :ensure nil
  :defer t
  :general
  (leader-def
    "p f" '(project-find-file :wk "find file")
    "p b" '(project-switch-to-buffer :wk "switch buffer")
    "p d" '(project-dired :wk "dired")
    "p k" '(project-kill-buffers :wk "kill buffers")
    "p p" '(project-switch-project :wk "switch project")
    "p c" '(project-compile :wk "compile")
    "p v" '(magit-project-status :wk "magit")
    "p s" '(consult-ripgrep :wk "search"))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (consult-ripgrep "Search" ?s)
     (project-dired "Dired")
     (magit-project-status "Magit" ?v)
     (project-eshell "Eshell"))))

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-log magit-blame magit-diff magit-project-status)
  :general
  (leader-def
    "v s" '(magit-status :wk "status")
    "v l" '(magit-log :wk "log")
    "v b" '(magit-blame :wk "blame")
    "v d" '(magit-diff :wk "diff")
    "v g" '(magit-generate-commit-message :wk "generate message"))
  (:keymaps 'git-commit-mode-map
   "C-c C-g" '(magit-generate-commit-message :wk "generate message"))
  :custom
  (magit-display-buffer-function
   (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  ;; Guard against corrupt transient history at runtime: if transient-setup
  ;; fails with a type error, wipe the bad entry and retry.
  (advice-add 'transient-setup :around
              (lambda (fn command &rest args)
                (condition-case err
                    (apply fn command args)
                  (wrong-type-argument
                   (message "transient: corrupt history for %s — resetting (%s)"
                            command (error-message-string err))
                   (setq transient-history
                         (assq-delete-all command transient-history))
                   (apply fn command args))))
              '((name . hyalo-transient-history-guard)))

  ;; Commit message generation with AI
  (defconst magit--commit-system-message
    "You are a commit message generator. Follow these rules strictly:

1. Use conventional commits format: <type>(<scope>): <description>
2. Types: feat, fix, docs, style, refactor, perf, test, chore
3. Keep the first line under 50 characters
4. Use imperative mood (\"add\" not \"added\", \"fix\" not \"fixed\")
5. Do not end with a period
6. Be specific but concise
7. If breaking change, add \"BREAKING CHANGE:\" footer

Respond with ONLY the commit message, no explanation, no markdown, no quotes."
    "System message for Gemini CLI commit message generation.")

  (defcustom magit-commit-provider "synthetic"
    "AI provider for commit message generation."
    :type 'string
    :group 'magit)

  (defcustom magit-commit-model "hf:MiniMaxAI/MiniMax-M2.1"
    "Model ID for commit message generation."
    :type 'string
    :group 'magit)

  (defun magit-generate-commit-message ()
    "Generate commit message using pi CLI based on staged changes.
Inserts the message at point in the commit buffer.
Runs asynchronously — shows a placeholder while generating."
    (interactive)
    (let ((diff (with-temp-buffer
                  (call-process "git" nil t nil "diff" "--cached" "--no-color")
                  (buffer-string))))
      (if (string-empty-p diff)
          (message "No staged changes to generate commit message from")
        (let* ((buf (current-buffer))
               (insert-pos (point))
               (placeholder (format "Generating using %s/%s..."
                                    magit-commit-provider magit-commit-model))
               (truncated-diff (if (> (length diff) 50000)
                                   (concat (substring diff 0 50000)
                                           "\n\n[Diff truncated due to length]")
                                 diff))
               (prompt (concat "\n\nGenerate a commit message for these changes:\n\n"
                               "```diff\n"
                               truncated-diff
                               "\n```")))
          ;; Insert placeholder using a marker so positions track edits
          (let ((marker (point-marker)))
            (set-marker-insertion-type marker nil)
            (insert placeholder)
            ;; Run pi asynchronously
            (let ((output-buf (generate-new-buffer " *magit-commit-gen*"))
                  (stderr-buf (generate-new-buffer " *magit-commit-gen-err*")))
              (make-process
               :name "magit-commit-gen"
               :buffer output-buf
               :stderr stderr-buf
               :command (list "pi"
                              "--provider" magit-commit-provider
                              "--model" magit-commit-model
                              "--system-prompt" magit--commit-system-message
                              "--no-tools"
                              "--no-session"
                              "-p" prompt)
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let* ((exit-code (process-exit-status proc))
                          (success (= exit-code 0))
                          (raw (with-current-buffer output-buf
                                 (buffer-string)))
                          (stderr (when (buffer-live-p stderr-buf)
                                    (string-trim
                                     (with-current-buffer stderr-buf
                                       (buffer-string)))))
                          ;; Strip OSC sequences (ESC ] ... BEL or ESC \) and
                          ;; CSI sequences (ESC [ ... final-byte)
                          (cleaned (replace-regexp-in-string
                                    "\e\\][^\a]*\a\\|\e\\].*?\e\\\\\\|\e\\[[0-9;]*[A-Za-z]"
                                    "" raw))
                          (result (string-trim cleaned)))
                     (kill-buffer output-buf)
                     (when (buffer-live-p stderr-buf)
                       (kill-buffer stderr-buf))
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (save-excursion
                           ;; Remove placeholder using marker position
                           (goto-char marker)
                           (let ((ph-end (+ (marker-position marker)
                                            (length placeholder))))
                             (when (and (<= ph-end (point-max))
                                        (string= (buffer-substring-no-properties
                                                  (marker-position marker) ph-end)
                                                 placeholder))
                               (delete-region (marker-position marker) ph-end)))
                           ;; Insert result or show error with details
                           (if (and success (not (string-empty-p result)))
                               (progn
                                 (goto-char marker)
                                 (insert result)
                                 (message "Commit message inserted"))
                             (message "Commit message generation failed (exit %d)%s%s"
                                      exit-code
                                      (if (and stderr (not (string-empty-p stderr)))
                                          (format ": %s" stderr) "")
                                      (if (and (not success) (not (string-empty-p result)))
                                          (format " [output: %s]"
                                                  (truncate-string-to-width result 200))
                                        ""))))))
                     (set-marker marker nil))))))))))))

;; VC gutter with enhancements (thin bars, transparent faces).
;; Deferred to first file open via hyalo-first-file-hook.
(use-package diff-hl
  :ensure t
  :defer t
  :commands (diff-hl-mode global-diff-hl-mode diff-hl-flydiff-mode
           diff-hl-show-hunk diff-hl-stage-current-hunk
           diff-hl-next-hunk diff-hl-previous-hunk diff-hl-revert-hunk)
  :init
  (require 'hyalo-gutter)
  (add-hook 'hyalo-first-file-hook
            (lambda ()
              (require 'diff-hl)
              (require 'diff-hl-show-hunk)
              (hyalo-gutter-setup)))
  :config
  ;; Ensure show-hunk is loaded for the popup command
  (require 'diff-hl-show-hunk)
  ;; Setup fringe bitmaps and faces
  (hyalo-gutter-setup))

(use-package diffview
  :ensure t
  :general
  (leader-def
    "v v" '(hyalo-diffview-file :wk "diffview file"))
  :config
  (require 'diff-mode)

  ;; Remove backgrounds from diff faces (weight-based highlights only)
  (defun hyalo-diffview--update-faces (&rest _)
    "Remove backgrounds from diff faces while preserving theme colors."
    (dolist (face '(diff-added diff-removed diff-changed
                    diff-indicator-added diff-indicator-removed diff-indicator-changed
                    diff-refine-added diff-refine-removed diff-refine-changed))
      (when (facep face)
        (set-face-attribute face nil :background 'unspecified))))

  (add-hook 'enable-theme-functions #'hyalo-diffview--update-faces)
  (hyalo-diffview--update-faces)

  ;; Force font-lock in diffview buffers
  (add-hook 'diffview-mode-hook
            (lambda ()
              (setq-local font-lock-defaults '(diff-font-lock-keywords t nil nil nil))
              (font-lock-mode 1)
              (font-lock-ensure)))

  ;; Custom command: get git diff for current file and show in diffview
  (defun hyalo-diffview-file ()
    "Show git diff for current file in side-by-side view."
    (interactive)
    (let* ((file (buffer-file-name))
           (default-directory (if file (file-name-directory file) default-directory)))
      (unless file
        (user-error "Buffer is not visiting a file"))
      (let ((diff-output (shell-command-to-string
                          (format "git diff -- %s" (shell-quote-argument file)))))
        (if (string-empty-p diff-output)
            (message "No changes in %s" (file-name-nondirectory file))
          ;; Use a named buffer so diffview-current works correctly
          (let ((diff-buf (get-buffer-create "*git-diff-output*")))
            (with-current-buffer diff-buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert diff-output)
                (goto-char (point-min))
                (diff-mode)))
            (switch-to-buffer diff-buf)
            (diffview-current)))))))

(use-package eglot
  :ensure nil
  :defer t
  :hook ((swift-mode . eglot-ensure))
  :init
  ;; Defaults for eglot
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-auto-display-help-buffer nil
        ;; Disable margin indicators — they can increase line height
        ;; due to glyph display or emoji font height differences.
        ;; The eldoc hint is sufficient.
        eglot-code-action-indications '(eldoc-hint))
  :config
  ;; Swift LSP
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("xcrun" "sourcekit-lsp")))
  ;; JavaScript/TypeScript LSP
  (add-to-list 'eglot-server-programs
               `((js-mode typescript-mode)
                 . ,(if (executable-find "typescript-language-server")
                        '("typescript-language-server" "--stdio")
                      '("npx" "typescript-language-server" "--stdio"))))

  ;; Disable the events buffer to avoid GC/CPU churn on pretty-printing
  ;; (set size to 0 in production)
  (cl-callf plist-put eglot-events-buffer-config :size 0)

  ;; Deferred server shutdown: wait 3s before killing the server
  ;; when its last buffer closes. Prevents expensive restarts when
  ;; quickly switching between project files.
  (defun hyalo--eglot-defer-shutdown-a (fn &optional server)
    "Around advice for `eglot--managed-mode' to defer server shutdown."
    (cl-letf (((symbol-function 'eglot-shutdown)
               (let ((orig-shutdown (symbol-function 'eglot-shutdown)))
                 (lambda (server)
                   (run-at-time
                    3 nil
                    (lambda (server)
                      (unless (eglot--managed-buffers server)
                        (funcall orig-shutdown server)))
                    server)))))
      (funcall fn server)))
  (advice-add 'eglot--managed-mode :around #'hyalo--eglot-defer-shutdown-a)

  ;; LSP optimization: increase process output buffer for talkative
  ;; language servers.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local read-process-output-max (* 64 1024))))

  ;; Inlay hint face: smaller, no box, non-intrusive
  (with-eval-after-load 'eglot
    (when (facep 'eglot-inlay-hint-face)
      (set-face-attribute 'eglot-inlay-hint-face nil
                          :height 0.8
                          :box nil)))

  ;; Flymake face overrides: straight underlines with semantic colors.
  ;; Prevents wavy underlines that increase line height.
  (with-eval-after-load 'flymake
    (set-face-attribute 'flymake-error nil
                        :underline '(:style line :color "#ef4444"))
    (set-face-attribute 'flymake-warning nil
                        :underline '(:style line :color "#f59e0b"))
    (set-face-attribute 'flymake-note nil
                        :underline '(:style line :color "#6b7280"))

    ;; Fringe indicators: circular plain dots instead of default rectangles.
    ;; 8x8 filled circle bitmap for each severity level.
    ;; Decimal values encode the 8-bit rows of the circle:
    ;; ..XXXX.. = 60, .XXXXXX. = 126, XXXXXXXX = 255
    (define-fringe-bitmap 'hyalo-flymake-dot
      (vector 60 126 255 255 255 255 126 60)
      nil nil 'center)

    ;; Override flymake diagnostic bitmap functions.
    ;; Flymake uses `flymake-error-bitmap', `flymake-warning-bitmap',
    ;; `flymake-note-bitmap' to control fringe indicators.
    (setq flymake-error-bitmap '(hyalo-flymake-dot compilation-error))
    (setq flymake-warning-bitmap '(hyalo-flymake-dot compilation-warning))
    (setq flymake-note-bitmap '(hyalo-flymake-dot compilation-info))))

(provide 'init-tools)
;;; init-tools.el ends here
