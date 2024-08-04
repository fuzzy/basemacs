
(when (memq 'thwap-yasnippet-toggle thwap-layers)
	(use-package yasnippet
		:straight t
		:demand t
		:config
		(require 'yasnippet)
		(setq yas-snippet-dirs '("~/.emacs.d/snippets")))

	(use-package yasnippet-snippets
		:straight t
		:defer t
		:config
		(require 'yasnippet-snippets)
		(yas-reload-all)
		(yas-global-mode 1))

	(thwap/add-key-binding "y n" 'yas-new-snippet "Create a new yasnippet")
	;; TODO Add hydra mappings that follow this
	
	(message "T.H.W.A.P. Loaded yasnippet layer"))
	
