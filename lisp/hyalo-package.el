;;; hyalo-package.el --- Package management toolbar integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Detects upgradable Emacs packages and pushes status to the Swift
;; toolbar (PackageManagerView).  Provides refresh, upgrade-all, and
;; upgrade-single operations triggered from the toolbar popover.
;;
;; Detection:
;; - `package-alist' (installed) vs `package-archive-contents' (available)
;; - Version comparison via `version-list-<'
;; - `:vc' packages detected via `package-desc-kind' eq 'vc
;;
;; The toolbar shows:
;; - `shippingbox' icon with badge count for archive-upgradable packages
;; - :vc packages are listed but do NOT count toward the badge
;; - Upgradable packages grouped by archive (MELPA, GNU ELPA, etc.)
;; - Popover with Refresh, Upgrade All, and List Packages buttons
;; - Status: idle / refreshing / upgrading
;; - Last checked timestamp

;;; Code:

(require 'hyalo)
(require 'package)

(defvar hyalo-package--refreshing nil
  "Non-nil when package archives are being refreshed.")

(defvar hyalo-package--upgrading nil
  "Non-nil when packages are being upgraded.")

(defconst hyalo-package--activity-id "package-installation"
  "Stable activity ID for package installation.")

(defvar hyalo-package--last-checked nil
  "Timestamp string of the last successful package archive refresh.
Format: ISO-8601 (e.g. \"2026-02-15T23:48:00\").")

(defun hyalo-package--upgradable ()
  "Return list of (name installed-version available-version archive) for upgradable non-vc packages.
ARCHIVE is the source archive name (e.g. \"melpa\", \"gnu\")."
  (let (result)
    (dolist (entry package-alist result)
      (let* ((name (car entry))
             (installed (cadr entry))
             (available (cadr (assq name package-archive-contents))))
        (when (and available
                   (not (eq (package-desc-kind installed) 'vc))
                   (version-list-< (package-desc-version installed)
                                   (package-desc-version available)))
          (push (list (symbol-name name)
                      (package-version-join (package-desc-version installed))
                      (package-version-join (package-desc-version available))
                      (or (package-desc-archive available) "unknown"))
                result))))))

(defun hyalo-package--vc-packages ()
  "Return list of (name version) for packages installed via :vc."
  (let (result)
    (dolist (entry package-alist result)
      (let* ((name (car entry))
             (desc (cadr entry)))
        (when (eq (package-desc-kind desc) 'vc)
          (push (list (symbol-name name)
                      (package-version-join (package-desc-version desc)))
                result))))))

(defun hyalo-package--push-status ()
  "Push current package status to the Swift toolbar."
  (when (fboundp 'hyalo-update-package-status)
    (let* ((upgradable (hyalo-package--upgradable))
           (vc-pkgs (hyalo-package--vc-packages))
           (status (cond (hyalo-package--refreshing "refreshing")
                         (hyalo-package--upgrading  "upgrading")
                         (t                         "idle")))
           (packages (mapcar (lambda (pkg)
                               `((name . ,(nth 0 pkg))
                                 (installed . ,(nth 1 pkg))
                                 (available . ,(nth 2 pkg))
                                 (archive . ,(nth 3 pkg))))
                             upgradable))
           (vc-entries (mapcar (lambda (pkg)
                                 `((name . ,(nth 0 pkg))
                                   (version . ,(nth 1 pkg))))
                               vc-pkgs))
           (payload `((status . ,status)
                      (upgradable . ,(vconcat packages))
                      (vcPackages . ,(vconcat vc-entries))
                      (lastChecked . ,(or hyalo-package--last-checked :null)))))
      (hyalo-update-package-status (json-encode payload)))))

(defun hyalo-package--refresh ()
  "Refresh package archive contents asynchronously, then push status."
  (unless hyalo-package--refreshing
    (setq hyalo-package--refreshing t)
    (hyalo-package--push-status)
    (hyalo-package--push-activity "Refreshing package archives…")
    ;; Run in a timer to not block the Emacs event loop
    (run-with-timer
     0.1 nil
     (lambda ()
       (unwind-protect
           (progn
             (package-refresh-contents)
             (setq hyalo-package--last-checked
                   (format-time-string "%FT%T%z"))
             (message "Hyalo: Package archives refreshed"))
         (setq hyalo-package--refreshing nil)
         (hyalo-package--push-status)
         (hyalo-package--finish-activity "Archives refreshed"))))))

(defun hyalo-package--upgrade-all ()
  "Upgrade all installed packages, then push status."
  (unless hyalo-package--upgrading
    (setq hyalo-package--upgrading t)
    (hyalo-package--push-status)
    (hyalo-package--push-activity "Upgrading all packages…")
    (run-with-timer
     0.1 nil
     (lambda ()
       (unwind-protect
           (let ((upgradable (hyalo-package--upgradable)))
             (if (null upgradable)
                 (message "Hyalo: All packages up to date")
               (let ((count (length upgradable)))
                 (if (fboundp 'package-upgrade-all)
                     (package-upgrade-all t)
                   (dolist (pkg upgradable)
                     (let* ((name (intern (car pkg)))
                            (desc (cadr (assq name package-archive-contents))))
                       (when desc
                         (package-install desc t)))))
                 (message "Hyalo: Upgraded %d package%s"
                          count (if (= count 1) "" "s")))))
         (setq hyalo-package--upgrading nil)
         (hyalo-package--push-status)
         (hyalo-package--finish-activity "Packages up to date"))))))

(defun hyalo-package--upgrade-single (name-str)
  "Upgrade a single package by NAME-STR.
Handles both archive packages (via `package-install') and VC
packages (via `package-vc-upgrade')."
  (unless hyalo-package--upgrading
    (setq hyalo-package--upgrading t)
    (hyalo-package--push-status)
    (hyalo-package--push-activity (format "Upgrading %s…" name-str))
    (run-with-timer
     0.1 nil
     (lambda ()
       (unwind-protect
           (let* ((name (intern name-str))
                  (installed-desc (cadr (assq name package-alist))))
             (cond
              ;; VC package — use package-vc-upgrade
              ((and installed-desc
                    (eq (package-desc-kind installed-desc) 'vc)
                    (fboundp 'package-vc-upgrade))
               (package-vc-upgrade installed-desc)
               (message "Hyalo: Upgraded VC package %s" name-str))
              ;; Archive package — use package-install
              ((let ((archive-desc (cadr (assq name package-archive-contents))))
                 (when archive-desc
                   (package-install archive-desc t)
                   (message "Hyalo: Upgraded %s" name-str)
                   t)))
              (t
               (message "Hyalo: Package %s not found" name-str))))
         (setq hyalo-package--upgrading nil)
         (hyalo-package--push-status)
         (hyalo-package--finish-activity (format "Upgraded %s" name-str)))))))

(defun hyalo-package--list-packages ()
  "Open the Emacs *Packages* buffer and force a redisplay.
The redisplay + force-redisplay pattern ensures the buffer is
visible even when the Emacs view is not the key window
\(e.g. after a toolbar popover click\)."
  (list-packages)
  (redisplay t)
  (when (fboundp 'hyalo-force-redisplay)
    (hyalo-force-redisplay)))

;;; Activity integration

(defun hyalo-package--push-activity (title)
  "Push a package installation activity with TITLE to the activity viewer."
  (when (fboundp 'hyalo-activity-upsert)
    (hyalo-activity-upsert
     hyalo-package--activity-id
     "package-installation"
     title nil nil t)))

(defun hyalo-package--finish-activity (message)
  "Finish the package installation activity with MESSAGE."
  (when (fboundp 'hyalo-activity-finish)
    (hyalo-activity-finish hyalo-package--activity-id message))
  (when (fboundp 'hyalo-activity-remove-after-delay)
    (hyalo-activity-remove-after-delay hyalo-package--activity-id 5.0)))

;;; Setup

(defun hyalo-package-setup ()
  "Setup package management toolbar integration."
  ;; Ensure package system is fully initialized and activated.
  ;; `package-initialize' without the NO-ACTIVATE argument ensures
  ;; `package-alist' is populated (including :vc packages).
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  ;; Advise package-install to push activity during startup installs.
  ;; The advice shows which package is being installed in the activity
  ;; viewer, including packages triggered by use-package :ensure.
  (advice-add 'package-install :before #'hyalo-package--on-install-start)
  (advice-add 'package-install :after #'hyalo-package--on-install-done)
  ;; Push initial status after a delay so init completes first.
  (run-with-timer 3 nil #'hyalo-package--push-status))

(defun hyalo-package--on-install-start (pkg &rest _)
  "Before-advice on `package-install' to push activity.
PKG is a package-desc or symbol."
  (let ((name (if (package-desc-p pkg)
                  (symbol-name (package-desc-name pkg))
                (symbol-name pkg))))
    (hyalo-package--push-activity (format "Installing %s…" name))))

(defun hyalo-package--on-install-done (pkg &rest _)
  "After-advice on `package-install' to finish activity.
PKG is a package-desc or symbol."
  (let ((name (if (package-desc-p pkg)
                  (symbol-name (package-desc-name pkg))
                (symbol-name pkg))))
    (hyalo-package--finish-activity (format "Installed %s" name))))

(provide 'hyalo-package)
;;; hyalo-package.el ends here
