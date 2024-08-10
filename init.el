;; thwap helpers, functions the layers use
(require 'base-helpers)
;; thwap variables, empty variables for menu/hydra/keybinding/etc generation
(require 'base-variables)
;; thwap config, customization group
(require 'base-customizer)

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
(require 'base-defaults)
(require 'base-layers)
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
(let ((user-config (base/list-files-with-extension "~/.emacs.d/base.d" "el")))
	(dolist (config user-config)
		(load-file config)))

;; dashboard
;; (require 'thwap-dashboard)
;; hydras
;; (require 'thwap-hydra)

(when (memq 'base-dashboard-toggle base-layers)
	(load-file "~/.emacs.d/base/layers/dashboard.el"))
