;;
;; Golden Ratio Mode helps with window sizing, and is a default feature
;;
(when (memq 'base-dashboard-toggle base-layers)
	(use-package dashboard
		:straight t
		:ensure t)
	(dashboard-setup-startup-hook)
	(setq dashboard-banner-logo-title (base/dashboard-build-logo-title base-help-lines))
	(setq dashboard-startup-banner
	      (base/random-string-from-list
	       (base/list-files-with-extension "~/.emacs.d/logos" "png")))
	(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
	(setq dashboard-center-content t)
	(setq dashboard-vertically-center-content t)
	(setq dashboard-show-shortcuts t)
	(setq dashboard-items '((recents . 5)
				(agenda . 5)))
	(setq dashboard-startupify-list '(dashboard-insert-banner
					  dashboard-insert-newline
					  dashboard-insert-banner-title
					  dashboard-insert-newline
					  dashboard-insert-navigator
					  dashboard-insert-newline
					  dashboard-insert-init-info
					  dashboard-insert-items
					  dashboard-insert-newline
					  dashboard-insert-footer)))
	(message "T.H.W.A.P. Loaded dashboard layer"))

