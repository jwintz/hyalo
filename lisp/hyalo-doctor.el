;;; hyalo-doctor.el --- Environment requirement checks -*- lexical-binding: t -*-

;; Copyright (C) 2026
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, diagnostics

;;; Commentary:

;; Verifies that all runtime prerequisites for Hyalo are met.
;; Run with M-x hyalo-doctor.  Results appear in *hyalo-doctor*.
;;
;; Checks performed:
;;   Required
;;   - Emacs >= 30.1 with dynamic module support
;;   - macOS >= 26.0
;;   - Swift >= 6.2
;;   - Xcode >= 17 (reported as 26.x on macOS 26)
;;   - Fonts: SF Mono, Recursive Mono Casual Static, Symbols Nerd Font Mono
;;   - Tools: rg, git, node
;;
;;   Optional
;;   - typescript-language-server (TypeScript LSP via Eglot)
;;   - ~/Syntropment/hyalo-feedstock-unified (iOS builds)
;;   - ~/Syntropment/hyalo-tengwar (Tengwar rendering)

;;; Code:

(defconst hyalo-doctor--required-emacs-version "30.1"
  "Minimum Emacs version for Hyalo.")

(defconst hyalo-doctor--required-swift-major 6
  "Required Swift major version.")

(defconst hyalo-doctor--required-swift-minor 2
  "Required Swift minor version.")

(defconst hyalo-doctor--required-xcode-major 17
  "Required Xcode major version (reported as 26.x on macOS 26).")

(defconst hyalo-doctor--required-macos-major 26
  "Required macOS major version.")

(defconst hyalo-doctor--required-fonts
  '("SF Mono" "Recursive Mono Casual Static" "Symbols Nerd Font Mono")
  "Fonts required by hyalo init-appearance and nerd-icons.")

(defconst hyalo-doctor--required-tools
  '("rg" "git" "node")
  "External tools that must be on exec-path.")

(defconst hyalo-doctor--optional-tools
  '("typescript-language-server")
  "External tools that improve functionality but are not required.")

;;; Internal helpers

(defun hyalo-doctor--pass (label msg)
  "Insert a passing check line: [OK] LABEL - MSG."
  (insert (propertize (format "  [OK]  %s" label)
                      'face '(:foreground "#a6e3a1" :weight bold))
          (format " — %s\n" msg)))

(defun hyalo-doctor--fail (label msg)
  "Insert a failing check line: [FAIL] LABEL - MSG."
  (insert (propertize (format "  [!!]  %s" label)
                      'face '(:foreground "#f38ba8" :weight bold))
          (format " — %s\n" msg)))

(defun hyalo-doctor--warn (label msg)
  "Insert a warning check line: [WARN] LABEL - MSG."
  (insert (propertize (format "  [--]  %s" label)
                      'face '(:foreground "#f9e2af" :weight bold))
          (format " — %s\n" msg)))

(defun hyalo-doctor--section (title)
  "Insert a section header."
  (insert "\n" (propertize title 'face '(:weight bold :underline t)) "\n"))

(defun hyalo-doctor--check-emacs ()
  "Check Emacs version and module support."
  (hyalo-doctor--section "Emacs")
  ;; Version
  (let ((current emacs-version)
        (required hyalo-doctor--required-emacs-version))
    (if (version<= required current)
        (hyalo-doctor--pass "Version" (format "%s (>= %s required)" current required))
      (hyalo-doctor--fail "Version" (format "%s — need >= %s" current required))))
  ;; Module support
  (if (fboundp 'module-load)
      (hyalo-doctor--pass "Module support" "--with-modules compiled in")
    (hyalo-doctor--fail "Module support"
                        "Emacs not compiled with --with-modules; Hyalo.dylib cannot be loaded")))

(defun hyalo-doctor--macos-version ()
  "Return macOS version as a list (MAJOR MINOR PATCH)."
  (let ((raw (string-trim (shell-command-to-string "sw_vers -productVersion"))))
    (mapcar #'string-to-number (split-string raw "\\."))))

(defun hyalo-doctor--check-macos ()
  "Check macOS version."
  (hyalo-doctor--section "macOS")
  (let* ((parts (hyalo-doctor--macos-version))
         (major (car parts))
         (version-str (mapconcat #'number-to-string parts ".")))
    (if (>= major hyalo-doctor--required-macos-major)
        (hyalo-doctor--pass "Version"
                            (format "%s (>= %d required)"
                                    version-str
                                    hyalo-doctor--required-macos-major))
      (hyalo-doctor--fail "Version"
                          (format "%s — need >= %d.0"
                                  version-str
                                  hyalo-doctor--required-macos-major)))))

(defun hyalo-doctor--swift-version ()
  "Return Swift version string or nil."
  (let ((raw (shell-command-to-string "swift --version 2>/dev/null")))
    (when (string-match "Swift version \\([0-9]+\\.[0-9]+\\)" raw)
      (match-string 1 raw))))

(defun hyalo-doctor--check-swift ()
  "Check Swift compiler version."
  (hyalo-doctor--section "Swift")
  (if-let* ((version (hyalo-doctor--swift-version)))
      (let* ((parts (split-string version "\\."))
             (major (string-to-number (nth 0 parts)))
             (minor (string-to-number (nth 1 parts)))
             (ok (or (> major hyalo-doctor--required-swift-major)
                     (and (= major hyalo-doctor--required-swift-major)
                          (>= minor hyalo-doctor--required-swift-minor)))))
        (if ok
            (hyalo-doctor--pass "Version"
                                (format "%s (>= %d.%d required)"
                                        version
                                        hyalo-doctor--required-swift-major
                                        hyalo-doctor--required-swift-minor))
          (hyalo-doctor--fail "Version"
                              (format "%s — need >= %d.%d"
                                      version
                                      hyalo-doctor--required-swift-major
                                      hyalo-doctor--required-swift-minor))))
    (hyalo-doctor--fail "swift" "not found on PATH")))

(defun hyalo-doctor--xcode-version ()
  "Return Xcode major version number or nil."
  (let ((raw (shell-command-to-string "xcodebuild -version 2>/dev/null")))
    (when (string-match "Xcode \\([0-9]+\\)" raw)
      (string-to-number (match-string 1 raw)))))

(defun hyalo-doctor--check-xcode ()
  "Check Xcode version."
  (hyalo-doctor--section "Xcode")
  (if-let* ((major (hyalo-doctor--xcode-version)))
      (if (>= major hyalo-doctor--required-xcode-major)
          (hyalo-doctor--pass "Version"
                              (format "Xcode %d (>= %d required)"
                                      major
                                      hyalo-doctor--required-xcode-major))
        (hyalo-doctor--fail "Version"
                            (format "Xcode %d — need >= %d"
                                    major
                                    hyalo-doctor--required-xcode-major)))
    (hyalo-doctor--fail "xcodebuild" "not found; install Xcode")))

(defun hyalo-doctor--font-installed-p (family)
  "Return non-nil if font FAMILY is available in Emacs."
  (and (display-graphic-p)
       (not (null (find-font (font-spec :family family))))))

(defun hyalo-doctor--check-fonts ()
  "Check required fonts."
  (hyalo-doctor--section "Fonts")
  (dolist (font hyalo-doctor--required-fonts)
    (if (hyalo-doctor--font-installed-p font)
        (hyalo-doctor--pass font "installed")
      (hyalo-doctor--fail font "not found — install and restart Emacs"))))

(defun hyalo-doctor--check-tools ()
  "Check required and optional external tools."
  (hyalo-doctor--section "Tools (required)")
  (dolist (tool hyalo-doctor--required-tools)
    (if (executable-find tool)
        (hyalo-doctor--pass tool (executable-find tool))
      (hyalo-doctor--fail tool "not found on exec-path")))
  (hyalo-doctor--section "Tools (optional)")
  (dolist (tool hyalo-doctor--optional-tools)
    (if (executable-find tool)
        (hyalo-doctor--pass tool (executable-find tool))
      (hyalo-doctor--warn tool "not found — install for full functionality"))))

(defun hyalo-doctor--check-paths ()
  "Check optional paths for iOS and Tengwar."
  (hyalo-doctor--section "Paths (optional)")
  (let ((feedstock (expand-file-name "~/Syntropment/hyalo-feedstock-unified")))
    (if (file-directory-p feedstock)
        (hyalo-doctor--pass "iOS feedstock" feedstock)
      (hyalo-doctor--warn "iOS feedstock"
                          (format "%s — required for iOS builds" feedstock))))
  (let* ((tengwar-default (expand-file-name "../hyalo-tengwar" user-emacs-directory))
         (tengwar-path (or (and (boundp 'hyalo-tengwar-path) hyalo-tengwar-path)
                          tengwar-default)))
    (if (file-directory-p tengwar-path)
        (hyalo-doctor--pass "hyalo-tengwar" tengwar-path)
      (hyalo-doctor--warn "hyalo-tengwar"
                          (format "%s — Tengwar rendering unavailable" tengwar-path)))))

;;; Public entry point

;;;###autoload
(defun hyalo-doctor ()
  "Check all Hyalo runtime requirements and display a report."
  (interactive)
  (let ((buf (get-buffer-create "*hyalo-doctor*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Hyalo Doctor\n" 'face '(:height 1.3 :weight bold)))
        (insert (format "Run at %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (hyalo-doctor--check-emacs)
        (hyalo-doctor--check-macos)
        (hyalo-doctor--check-swift)
        (hyalo-doctor--check-xcode)
        (hyalo-doctor--check-fonts)
        (hyalo-doctor--check-tools)
        (hyalo-doctor--check-paths)
        (insert "\n")
        (special-mode)))
    (pop-to-buffer buf)))

(provide 'hyalo-doctor)
;;; hyalo-doctor.el ends here
