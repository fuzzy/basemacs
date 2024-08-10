;;
;; Golden Ratio Mode helps with window sizing, and is a default feature
;;
(when (memq 'thwap-dashboard-toggle thwap-layers)
	(use-package dashboard
		:straight t
		:ensure t
		:config
		(dashboard-setup-startup-hook)
		(setq dashboard-banner-logo-title (thwap/dashboard-build-logo-title thwap-help-lines))
		;; "Welcome to the T.H.W.A.P. Emacs Dashboard")
		(setq dashboard-startup-banner (thwap/random-string-from-list (thwap/list-files-with-extension "~/.emacs.d/logos" "png")))
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
																			dashboard-insert-footer))
		(message "T.H.W.A.P. Loaded dashboard layer")))

