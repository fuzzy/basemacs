;;
;; Golden Ratio Mode helps with window sizing, and is a default feature
;;

(when (memq 'thwap-golden-ratio-toggle thwap-layers)
	;; golden-ratio
	(use-package golden-ratio
		:straight t
		:demand t
		:config
		(golden-ratio-mode 1)
		(setq golden-ratio-auto-scale t))

	(message "T.H.W.A.P. Loaded golden-ratio layer"))

