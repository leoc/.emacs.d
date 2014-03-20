(require 'org)
(require 'org-habit)
(require 'org-helpers)
;; (require 'org-export)

;; POMODORO SETTINGS
(require 'org-pomodoro)

;; sets the default workflow keywords and their faces
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(o)" "|" "CANCELLED(c@/!)")))

(setq org-priority-faces
      '((65 :foreground "#ff7000" :weight bold)
        (66 :foreground "#ffa060" :weight bold)
        (67 :foreground "#ffcca8" :weight bold)))

(setq org-todo-keyword-faces
      '(("SOMEDAY"   :foreground "#808080" :weight bold)
        ("NEXT"      :foreground "#e9c062" :weight bold)
        ("STARTED"   :foreground "#ffff63" :weight bold)
        ("WAITING"   :foreground "#fd9b3b" :weight bold)
        ("HOLD"      :foreground "#9b859d" :weight bold)
        ("CANCELLED" :foreground "#9eb9a7" :weight bold)))

(setq org-tag-alist '((:startgroup . nil)
                      ("business" . ?b)
                      ("personal" . ?p)
                      (:endgroup . nil)))

;; The default agenda files. inbox.org is used only in custom agenda.
(setq org-agenda-files (list "~/.org/tasks.org"
                             "~/.org/tasks.org_archive"
                             "~/.org/projects.org"
                             "~/.org/projects.org_archive"
                             "~/.org/business.org"
                             "~/.org/business.org_archive"
                             "~/.org/calendar.org"))

;; my org settings
(custom-set-variables
 '(org-startup-indented t)
 '(org-log-done t)
 '(org-completion-use-ido t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-ndays 1)
 '(org-agenda-include-diary t)
 '(org-agenda-window-setup 'current-window)
 '(org-agenda-repeating-timestamp-show-all t)
 ;; Show all agenda dates - even if they are empty
 '(org-agenda-show-all-dates t)
 ;; Sorting order for tasks on the agenda
 '(org-agenda-sorting-strategy
   (quote ((agenda habit-down time-up user-defined-up
            priority-down effort-up category-keep)
           (todo category-up priority-down effort-up)
           (tags category-up priority-down effort-up)
           (search category-up))))
 '(org-agenda-cmp-user-defined 'oh/agenda-sort)
 ;; Keep tasks with dates on the global todo lists
 '(org-agenda-todo-ignore-with-date nil)
 ;; Keep tasks with deadlines on the global todo lists
 '(org-agenda-todo-ignore-deadlines nil)
 ;; Keep tasks with scheduled dates on the global todo lists
 '(org-agenda-todo-ignore-scheduled nil)
 ;; Keep tasks with timestamps on the global todo lists
 '(org-agenda-todo-ignore-timestamp nil)
 ;; Remove completed deadline tasks from the agenda view
 '(org-agenda-skip-deadline-if-done t)
 ;; Remove completed scheduled tasks from the agenda view
 '(org-agenda-skip-scheduled-if-done t)
 ;; Remove completed items from search results
 '(org-agenda-skip-timestamp-if-done t)
 ;; Display tags farther right
 '(org-agenda-tags-column -102)
 '(org-agenda-persistent-filter t)
 ;; Enable display of the time grid
 ;; so we can see the marker for the current time
 '(org-agenda-time-grid (quote ((daily today remove-match)
                                #("----------------" 0 16 (org-heading t))
                                (830 1000 1200 1300 1500 1700))))
 ;; Do not dim blocked tasks
 '(org-agenda-dim-blocked-tasks nil)

 ;; Show lot sof clocking history so it's easy to pick items off the C-F11 list
 '(org-clock-history-length 36)
 ;; Separate drawers for clocking and logs
 '(org-drawers (quote ("PROPERTIES" "LOGBOOK")))
 ;; Save clock data and state changes and notes in the LOGBOOK drawer
 '(org-clock-into-drawer t)
 ;; Sometimes I change tasks I'm clocking quickly
 ;; this removes clocked tasks with 0:00 duration
 '(org-clock-out-remove-zero-time-clocks t)
 ;; Do not prompt to resume an active clock
 '(org-clock-persist-query-resume nil)
 ;; Include current clocking task in clock reports
 '(org-clock-report-include-clocking-task t)
 '(org-fast-tag-selection-single-key 'expert))

(setq org-habit-show-habits nil)
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq org-refile-targets '(("~/.org/tasks.org" :level . 1)
                           ("~/.org/projects.org" :level . 1)
                           ("~/.org/thoughts.org" :level . 1)
                           ("~/.org/calendar.org" :level . 1)
                           ("~/.org/business.org" :level . 1)))

(setq org-capture-templates '(("r" "Remember" entry (file+headline "~/.org/inbox.org" "Inbox") "* TODO %?")
                              ("j" "Journal Entry" plain (file+datetree "~/.org/journal.org") (file "~/.org/templates/review"))))
(define-key global-map "\C-cr" (lambda () (interactive) (org-capture nil "r")))
(define-key global-map "\C-cj" (lambda () (interactive) (org-capture nil "j")))

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(setq require-final-newline t)

(custom-set-faces
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

(setq org-habit-graph-column 102)
(setq org-habit-following-days 3)
(setq org-habit-preceding-days 12)

;; Some keybindings that should be activated in org-mode
(defun custom-org-agenda-mode-defaults ()
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer))
(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

;; configure org remember functions and hooks
(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("a" "Agenda"
         ((agenda "" ((org-agenda-sorting-strategy '(habit-down timestamp-up time-up priority-down category-keep user-defined-up))))
          (tags-todo "-CANCELLED/!-HOLD-WAITING"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(non-project)
                                        :subtree-if '(non-stuck-project inactive-project habit scheduled deadline)))
                        (org-tags-match-list-sublevels 'intended)))
          (tags-todo "-WAITING-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(priority-down todo-state-down effort-up category-keep))))
          (tags-todo "-CANCELLED/!-NEXT-HOLD-WAITING"
                     ((org-agenda-overriding-header "Available Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(priority-down category-keep))
                      (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Currently Active Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive-project habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(priority-down category-keep))))
          (tags-todo "-CANCELLED/!WAITING|HOLD"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit)))
                      (org-tags-match-list-sublevels nil))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/.org/inbox.org"))))
        ("#" "Stuck Projects" tags-todo "-CANCELLED/!-HOLD-WAITING"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                          habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(priority-down todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-CANCELLED/!-NEXT-HOLD-WAITING"
         ((org-agenda-overriding-header "Available Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(priority-down category-keep))))
        ("p" "Projects" tags-todo "-CANCELLED/!"
         ((org-agenda-overriding-header "Currently Active Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
          (org-agenda-sorting-strategy '(priority-down category-keep))
          (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
          (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))


(defun custom-org-mode-defaults ()
  (electric-indent-mode -1)
  (org-defkey org-mode-map (kbd "M-p") 'org-metaup)
  (org-defkey org-mode-map (kbd "M-n") 'org-metadown)
  (org-shifttab 2))
(add-hook 'org-mode-hook 'custom-org-mode-defaults)

(setq org-ditaa-jar-path (concat vendor-dir "/ditaa0_9.jar"))
(setq org-plantuml-jar-path (concat vendor-dir "/plantuml.jar"))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 '((emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (R . t)
   (python . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)
   (ledger . t)
   (org . t)
   (plantuml . t)
   (latex . t)))


;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Use pygments for syntax coloring
;; (require 'org-latex)
;; (setq org-export-latex-listings 'minted)
;; (add-to-list 'org-export-latex-packages-alist '("" "minted"))


(provide 'setup-org)
