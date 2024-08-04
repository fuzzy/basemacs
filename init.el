;; thwap helpers, functions the layers use
(require 'thwap-helpers)
;; thwap variables, empty variables for menu/hydra/keybinding/etc generation
(require 'thwap-variables)
;; thwap config, customization group
(require 'thwap-customizer)

(setq custom-init "~/.emacs.d/custom-init.el")

(unless (file-exists-p custom-init)
  (thwap/touch-file custom-init))
(load-file custom-init)

(setq custom-file "~/.emacs.d/emacs-custom.el")

(unless (file-exists-p custom-file)
  (thwap/touch-file custom-file))
(load custom-file)

;; thwap config
;; (require 'thwap-configuration)
;; thwap defaults
(require 'thwap-defaults)
(require 'thwap-layers)
;; theme configuration
;; (require 'thwap-interface)
;; file browser configuration
;; (require 'thwap-file-browser)
;; company configuration
;; (require 'thwap-completion)
;; development configuration
;; (require 'thwap-development)
;; org configuration
;; (require 'thwap-orgmode)

;; now that everything is loaded, let's load all the user configurations
(let ((user-config (thwap/list-files-with-extension "~/.emacs.d/thwap.d" "el")))
	(dolist (config user-config)
		(load-file config)))

;; dashboard
;; (require 'thwap-dashboard)
;; hydras
;; (require 'thwap-hydra)

(when (memq 'thwap-dashboard-toggle thwap-layers)
	(load-file "~/.emacs.d/thwap/layers/dashboard.el"))
