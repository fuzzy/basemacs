
;;
;; Set some defaults
;;

(when (display-graphic-p)
	;; These things should be turned on, when in GUI mode
	(tool-bar-mode -1))

;; mouse interaction options
(setq mouse-autoselect-window t
			mouse-drag-and-drop-region t
			mouse-drag-and-drop-region-cross-program t
			xterm-mouse-mode t
			;; bell
			visible-bell 1
			;; encrypted credential store
			auth-sources '("~/.authinfo.gpg")
			warning-minimum-level :error
			tab-width 2
			)

;; automatically revert buffers when they change on disk
(global-auto-revert-mode t)

;; display line numbers, TODO make this a config option for line number style
(global-display-line-numbers-mode 1)

;; tab indent mode
(indent-tabs-mode 1)

;; set the default tab width to 2 spaces
(setq-default tab-width 2)



;; Set the default help lines
(setq thwap-help-lines '("--------------"
												 "All T.H.W.A.P. commands will start with 'C-c t'"
												 ""
												 "Welcome to the T.H.W.A.P. Emacs Dashboard"))



;;
;; Set up the T.H.W.A.P. keybindings and hydra
;;

;; (use-package hydra
;; 	:straight t
;; 	:demand t)

;; The T.H.W.A.P. Map
;; This is the base starting point for the T.H.W.A.P. keybindings
(define-prefix-command 'thwap-map)
(global-set-key (kbd "C-c t") 'thwap-map)

;; Add a command to customize the T.H.W.A.P. group
(thwap/add-key-binding
 "C-c"
 (lambda () (interactive) (customize-group 'thwap-config))
 "Customize this T.H.W.A.P. Emacs Installation")

;; quick commenting/uncommenting of a region is just too useful
(thwap/add-key-binding "c" 'comment-or-uncomment-region "Comment or uncomment region")

;; eval buffer is also very useful
(thwap/add-key-binding "e b" 'eval-buffer "Eval buffer")

(setq thwap-yasnippet-hydra-map '(("n" "New Snippet" 'yas-new-snippet)
																	("r" "Reload Snippets" 'yas-reload-all)
																	("l" "Load Snippets" 'yas-load-directory)))

(setq thwap-base-hydra-map '(("C-c" "Config Customizer" (lambda () (interactive) (customize-group 'thwap-config)))
															("c" "(Un)Comment Region" 'comment-or-uncomment-region)
															("e" "Eval Buffer" 'eval-buffer)
															("C-t" "Toggle Treemacs" '(lambda () (interactive) (treemacs)))
															("y" "YaSnippet Menu" 'thwap-yasnippet-hydra/body)))


(setq truncate-lines nil)

(message "T.H.W.A.P. Defaults Loaded")
;; our provide statement
(provide 'thwap-defaults)
