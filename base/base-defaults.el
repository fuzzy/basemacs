
(unless (boundp 'base-help-tag)
	(setq base-help-tag "BaseMacs"))

(unless (boundp 'base-user-key-base)
	(setq base-user-key-base "b"))

;; Set the default help lines
(setq base-help-lines '("--------------"
												(format "All %s commands will start with 'C-c %s'" base-help-tag base-user-key-base)
												""
												(format "Welcome to the %s Emacs Dashboard" base-help-tag)))

;; This is the base starting point for the BaseMacs keybindings
(define-prefix-command 'base-map)
(global-set-key (kbd (format "C-c %s" base-user-key-base)) 'base-map)

;; Add a command to customize the BaseMacs group
(base/add-key-binding
 "C-c"
 (lambda () (interactive) (customize-group 'base-config))
 (format "Customize this %s Emacs Installation" base-help-tag))

(message "BaseMacs: Defaults Loaded")
;; our provide statement
(provide 'base-defaults)
