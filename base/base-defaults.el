;; Set the default help lines for the BaseMacs dashboard. In reverse order.
(setq base-help-lines '("--------------"
												(format "All %s commands will start with 'C-c %s'" base-help-tag base-user-key-base)
												""
												(format "Welcome to the %s Emacs Dashboard" base-help-tag)))

;; This is the base starting point for the BaseMacs keybindings
(define-prefix-command 'base-map)
(global-set-key (kbd (format "%s %s" base-user-key-prefix base-user-key-base)) 'base-map)

;; Add a command to customize the BaseMacs group
(base/add-key-binding
 "C-c"
 (lambda () (interactive) (customize-group 'base-config))
 (format "Customize this %s Emacs Installation" base-help-tag))

;; success message
(message (format "%s Defaults Loaded" base-help-tag))

;; our provide statement
(provide 'base-defaults)
