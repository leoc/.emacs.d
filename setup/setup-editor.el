;; Ensure packages that enhance editing severely
(ensure-package-and-require 'ace-jump-mode)
(ensure-package-and-require 'smart-forward)
(ensure-package-and-require 'volatile-highlights)
(ensure-package-and-require 'frame-cmds)
(ensure-package-and-require 'expand-region)
(ensure-package-and-require 'multiple-cursors)
(ensure-package-and-require 'key-chord)
(key-chord-mode 1)

;; Browse kill ring
(ensure-package-and-require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

(provide 'setup-editor)
