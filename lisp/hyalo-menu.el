;;; hyalo-menu.el --- Hyalo macOS menu bar menu -*- lexical-binding: t; -*-

;; Defines the "Hyalo" top-level menu in the macOS menu bar.
;; Uses easy-menu for declarative menu definition.

;;; Code:

(require 'easymenu)

(easy-menu-define hyalo-menu global-map "Hyalo"
  '("Hyalo"
    ["Open Quickly..." hyalo/open-quickly
     :visible (fboundp 'hyalo-show-open-quickly)
     :keys "s-o"]
    ["Command Palette..." hyalo/command-palette
     :visible (fboundp 'hyalo-show-command-palette)
     :keys "s-p"]
    "---"
    ["Toggle Navigator" hyalo-toggle-navigator :keys "s-0"]
    ["Toggle Inspector" hyalo-toggle-inspector :keys "M-s-0"]
    ["Toggle Utility Area" hyalo-toggle-utility-area]
    ["Toggle Minimap" demap-toggle
     :visible (fboundp 'demap-toggle)]
    "---"
    ("Appearance"
     ["Dark Mode" hyalo-appearance-set-dark
      :visible (featurep 'hyalo-appearance)]
     ["Light Mode" hyalo-appearance-set-light
      :visible (featurep 'hyalo-appearance)]
     ["Auto (System)" hyalo-appearance-set-auto
      :visible (featurep 'hyalo-appearance)]
     "---"
     ["Appearance Panel..." hyalo-appearance-panel
      :visible (featurep 'hyalo-appearance)])
    "---"
    ["Rebuild & Reload" hyalo-rebuild-and-reload]
    ["Version Info" hyalo-version-check]))

(provide 'hyalo-menu)
;;; hyalo-menu.el ends here
