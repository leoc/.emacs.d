;;; rinari-defuns.el --- Some additional functions to enhance rinari.
;;; Commentary:
;;; Code:
(defun leoc/shell-buffer-name (name)
  (concat "*" name "*"))

(defun rinari-guard (&optional group)
  "Run guard in the rinari root."
  (interactive)
  (let ((rinari-root (rinari-root))
        (new-buffer-name (leoc/shell-buffer-name
                          (if group (concat "guard-" group) "guard"))))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name new-buffer-name)))
    (shell (current-buffer))
    (process-send-string nil (concat "cd " rinari-root "\n"))
    (process-send-string nil (if group (concat "guard -g " group "\n") "guard\n"))))

(defun rinari-m4c (&optional app)
  "Run the rails server inside a shell."
  (interactive)
  (let* ((rinari-root (rinari-root))
         (new-buffer-name (leoc/shell-buffer-name "m4c-server"))
         (rails-command (concat
                         (when rinari-rails-env (concat "RAILS_ENV=" rinari-rails-env " "))
                         (when app (concat "APP_IDENTIFIER=" app " "))
                         "rails s\n")))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name new-buffer-name)))
    (shell (current-buffer))
    (process-send-string nil (format "cd %s\n" rinari-root))
    (process-send-string nil rails-command)))

(defun rinari-sunspot (&optional rails-env)
  "Run sunspot solr in the right environment."
  (interactive)
  (let* ((rinari-root (rinari-root))
         (new-buffer-name (leoc/shell-buffer-name (if rails-env (concat "sunspot-" rails-env) "sunspot")))
         (rake-command (concat
                         (when rails-env (concat "RAILS_ENV=" rails-env " "))
                         "bundle exec rake sunspot:solr:run\n")))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name new-buffer-name)))
    (shell (current-buffer))
    (process-send-string nil (format "cd %s\n" rinari-root))
    (process-send-string nil rake-command)))

(defun rinari-frame-m4c ()
  "Creates a new frame and starts the required processes for the current rinari-root."
  (interactive)
  (rinari-launch)
  (rvm-activate-corresponding-ruby)
  (rinari-m4c "easd")
  (rinari-console)
  (rinari-sunspot "development")
  (rinari-sunspot "test")
  (rinari-guard "frontend")
  (rinari-guard "tests")
  (rinari-guard "livereload")
  (robe-start))
