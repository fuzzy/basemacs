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

(require 'base-helpers)
(require 'base-varibles)
(require 'base-customizer)
(require 'base-defaults)
(require 'base-layers)

(add-hook 'after-init-hook
					(lambda ()
						(when (memq 'base-dashboard-toggle base-layers)
							(load-file (expand-file-name "base/layers/dashboard.el" user-emacs-directory)))))
