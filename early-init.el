;; Add the lib directory to the load path
(add-to-list 'load-path (expand-file-name "base" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "base/layers" user-emacs-directory))

;; And disable package at startup
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up straight.el to use use-package
(straight-use-package 'use-package)

(require 'base-variables)

;; We do this after init.el is processed, for multiple reasons.
;; 1. We want to load the layers after the user configurations, so that user configurations get reflected in the layers
;; 2. We want to load the dashboard after the layers, so that the dashboard can reflect the layers custom keybindings if any
(add-hook 'after-init-hook
	  (lambda ()
	    ;; load the helper functions
	    (require 'base-helpers)
	    ;; load the customizer
	    (require 'base-customizer)
	    ;; load the defaults here to give the user a chance to override them
	    (require 'base-defaults)
	    ;; load the layers
	    (require 'base-layers)
	    ;; now that everything is loaded, let's load all the user configurations
	    (let ((user-config (base/list-files-with-extension "~/.emacs.d/base.d" "el")))
	      (dolist (config user-config)
		(load-file config)))
	    ;; load the dashboard if it's enabled
	    (when (memq 'base-dashboard-toggle base-layers)
	      (load-file (expand-file-name "base/layers/dashboard.el" user-emacs-directory)))))
