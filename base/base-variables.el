;; This will get used for all the message labels
(setq base-help-tag "BaseMacs")

;; This will be the prefix for all the user keys, by default it is C-c as this is reserved for user keys
(setq base-user-key-prefix "C-c")

;; This will be the base for all the user keys, by default it is b
(setq base-user-key-base "b")

;; Container for the layer toggle options, if you are adding a layer, add
;; the toggle for it to this list
(setq base-layer-toggles '(("dashboard" base-dashboard-toggle)))

;; Set the default help lines variable for the dashboard
(setq base-help-lines '())

(message (format "%s Variables initialized" base-help-tag))
(provide 'base-variables)
