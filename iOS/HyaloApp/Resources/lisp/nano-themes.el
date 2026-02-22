;;; nano-themes.el --- N Λ N O themes built on Modus -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-theme
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (modus-themes "5.0.0"))
;; Keywords: faces, theme, accessibility

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The N Λ N O themes are built on top of the `modus-themes'.  They
;; provide a minimal, consistent aesthetic based on Material Design
;; (light) and Nord (dark) color palettes.
;;
;; "N Λ N O" follows the design principles explained in "On the Design
;; of Text Editors" (https://arxiv.org/abs/2008.06030), using seven
;; core faces: default, critical, popout, strong, salient, faded, and
;; subtle.
;;
;; To make all the Modus commands that operate on a theme only consider
;; N Λ N O themes, enable `nano-themes-take-over-modus-themes-mode'.
;; Or, if you prefer to blend N Λ N O and Modus into a single group,
;; enable `modus-themes-include-derivatives-mode'.

;;; Code:

(require 'modus-themes)
(eval-when-compile (require 'subr-x))

;;;; Basics for building on top of Modus

(defgroup nano-themes ()
  "N Λ N O themes built on Modus.
The N Λ N O themes are built on top of the `modus-themes'.  To make all
the Modus commands that operate on a theme only consider N Λ N O themes,
enable the `nano-themes-take-over-modus-themes-mode'.  Or, if you prefer
to blend N Λ N O and Modus into a single group, enable
`modus-themes-include-derivatives-mode'."
  :group 'faces
  :group 'modus-themes
  :link '(url-link :tag "Homepage" "https://github.com/rougier/nano-theme")
  :link '(url-link :tag "Modus themes" "https://github.com/protesilaos/modus-themes")
  :prefix "nano-themes-"
  :tag "N Λ N O Themes")

(defconst nano-themes-light-themes
  '(nano-light)
  "List of symbols with the light N Λ N O themes.")

(defconst nano-themes-dark-themes
  '(nano-dark)
  "List of symbols with the dark N Λ N O themes.")

(defconst nano-themes-items
  (append nano-themes-light-themes nano-themes-dark-themes)
  "Symbols of all the N Λ N O themes.")

(defconst nano-themes-with-properties
  '((nano-light nano-themes "N Λ N O light theme based on Material Design." light nano-light-palette nil nano-light-palette-overrides)
    (nano-dark nano-themes "N Λ N O dark theme based on Nord." dark nano-dark-palette nil nano-dark-palette-overrides)))

(defvar nano-themes--declared-p nil)

(defun nano-themes-declare-themes ()
  "Declare the N Λ N O themes."
  (unless nano-themes--declared-p
    (dolist (theme nano-themes-with-properties)
      (apply #'modus-themes-declare theme)
      (modus-themes-register (car theme)))
    (setq nano-themes--declared-p t)))

(nano-themes-declare-themes)

;;;; Compatibility with older versions

(defvar nano-themes--aliased-p nil)

(defun nano-themes-define-alias (prefix suffix &optional is-function)
  "Make alias for the Modus themes symbol with PREFIX and SUFFIX.
If IS-FUNCTION is non-nil, use the appropriate calls for functions, else
assume this is a variable."
  (let ((our-symbol (intern (format "%s-%s" prefix suffix)))
        (modus-symbol (intern-soft (format "modus-themes-%s" suffix))))
    (when (symbolp modus-symbol)
      (funcall
       (if is-function
           #'defalias
         #'defvaralias)
       our-symbol
       modus-symbol
       (format "`%s' is an alias for `%s'.
Since version 1.0.0, `nano-themes' derive from the `modus-themes'.
You can configure the `nano-themes' via the user options of the
`modus-themes'.

To make all the Modus commands that operate on a theme consider only N Λ N O
themes, enable `nano-themes-take-over-modus-themes-mode'.  Or, if you
prefer to blend N Λ N O and Modus into a single group, enable
`modus-themes-include-derivatives-mode' instead.

Alternatively, use the commands `nano-themes-rotate',
`nano-themes-select', `nano-themes-load-random',
`nano-themes-load-random-dark', `nano-themes-load-random-light',
`nano-themes-list-colors', `nano-themes-list-colors-current'.  They are
all designed to only consider N Λ N O themes." our-symbol modus-symbol)))))

(defun nano-themes-define-option-aliases ()
  "Define aliases for the user options of the Modus themes."
  (unless nano-themes--aliased-p
    (dolist (suffix '(disable-other-themes to-toggle to-rotate after-load-theme-hook
                     post-load-hook italic-constructs bold-constructs variable-pitch-ui mixed-fonts
                     headings completions prompts common-palette-overrides))
      (nano-themes-define-alias "nano-themes" suffix))
    (setq nano-themes--aliased-p t)))

(nano-themes-define-option-aliases)

(defalias 'nano-themes-load-theme 'modus-themes-load-theme
  "Alias for `modus-themes-load-theme'.")

(defalias 'nano-themes-with-colors 'modus-themes-with-colors
  "Alias for `modus-themes-with-colors'.")

;;;; Limit the Modus themes to only N Λ N O themes

;;;###autoload
(define-minor-mode nano-themes-take-over-modus-themes-mode
  "When enabled, all Modus themes commands consider only N Λ N O themes.
Alternatively, use the commands `nano-themes-rotate', `nano-themes-select',
`nano-themes-load-random', `nano-themes-load-random-dark',
`nano-themes-load-random-light', `nano-themes-list-colors',
`nano-themes-list-colors-current'.  They are all designed to only consider
N Λ N O themes."
  :global t
  :init-value nil)

(cl-defmethod modus-themes-get-themes (&context (nano-themes-take-over-modus-themes-mode (eql t)))
  "Return list of N Λ N O themes, per `nano-themes-take-over-modus-themes-mode'."
  (if-let* ((themes (modus-themes-get-all-known-themes 'nano-themes))
            (sorted-a-z (sort themes #'string-lessp))
            (sorted-light-dark (modus-themes-sort sorted-a-z 'light)))
      sorted-light-dark
    nano-themes-items))

;;;; Convenience commands

;;;###autoload (autoload 'nano-themes-toggle "nano-themes")
(modus-themes-define-derivative-command nano-themes toggle)

;;;###autoload (autoload 'nano-themes-rotate "nano-themes")
(modus-themes-define-derivative-command nano-themes rotate)

;;;###autoload (autoload 'nano-themes-select "nano-themes")
(modus-themes-define-derivative-command nano-themes select)

;;;###autoload (autoload 'nano-themes-select-dark "nano-themes")
(modus-themes-define-derivative-command nano-themes select-dark)

;;;###autoload (autoload 'nano-themes-select-light "nano-themes")
(modus-themes-define-derivative-command nano-themes select-light)

;;;###autoload (autoload 'nano-themes-load-random "nano-themes")
(modus-themes-define-derivative-command nano-themes load-random)

;;;###autoload (autoload 'nano-themes-load-random-dark "nano-themes")
(modus-themes-define-derivative-command nano-themes load-random-dark)

;;;###autoload (autoload 'nano-themes-load-random-light "nano-themes")
(modus-themes-define-derivative-command nano-themes load-random-light)

;;;###autoload (autoload 'nano-themes-list-colors "nano-themes")
(modus-themes-define-derivative-command nano-themes list-colors)

;;;###autoload (autoload 'nano-themes-list-colors-current "nano-themes")
(modus-themes-define-derivative-command nano-themes list-colors-current)

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'nano-themes)
;;; nano-themes.el ends here
