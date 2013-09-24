(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes (quote ("7e7d2c96a31b82e5865eef1e35c8be3b3f7ef32bb5b52a0b6bdf91f41c8924b1" default)))
 '(eclim-eclipse-dirs (quote ("~/.eclipse")))
 '(ido-vertical-mode t)
 '(org-agenda-cmp-user-defined (quote oh/agenda-sort))
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-repeating-timestamp-show-all t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep) (todo category-up priority-down effort-up) (tags category-up priority-down effort-up) (search category-up))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-tags-column -102)
 '(org-agenda-time-grid (quote ((daily today remove-match) #("----------------" 0 16 (org-heading t)) (830 1000 1200 1300 1500 1700))))
 '(org-agenda-todo-ignore-deadlines nil)
 '(org-agenda-todo-ignore-scheduled nil)
 '(org-agenda-todo-ignore-timestamp nil)
 '(org-agenda-todo-ignore-with-date nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-clock-history-length 36)
 '(org-clock-into-drawer t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-completion-use-ido t)
 '(org-drawers (quote ("PROPERTIES" "LOGBOOK")))
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-done t)
 '(org-startup-indented t)
 '(send-mail-function (quote smtpmail-send-it))
 '(speedbar-use-images nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
