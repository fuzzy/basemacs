
(when (and thwap-fm-enable-neotree (not thwap-fm-enable-treemacs))
	(use-package neotree
		:straight t
		:config
		(global-set-key [f8] 'neotree-toggle)
		(define-key thwap-map (kbd "C-n") 'neotree-toggle)
		(add-to-list 'thwap-help-lines "C-c t C-n   : Toggle Neotree")))


(when (and thwap-fm-enable-treemacs (not thwap-fm-enable-neotree))
	(use-package treemacs
		:straight t
		:ensure t
		:demand t
		:config
		(global-set-key [f8] 'treemacs)
		(progn
			(setq treemacs-follow-after-init t
						treemacs-width 35
						treemacs-indentation 2
						treemacs-git-integration t
						treemacs-collapse-dirs 3
						treemacs-silent-refresh t
						treemacs-change-root-without-asking t
						treemacs-sorting 'alphabetic-desc
						treemacs-show-hidden-files t
						treemacs-never-persist nil
						treemacs-is-never-other-window t
						treemacs-goto-tag-strategy 'refetch-index)))
	(define-key thwap-map (kbd "C-n") 'treemacs)
	(add-to-list 'thwap-help-lines "C-c t C-n   : Toggle Treemacs"))

(when (and thwap-fm-enable-treemacs thwap-ui-enable-kaolin-themes (not thwap-fm-enable-neotree))
  (kaolin-treemacs-theme))

(provide 'thwap-nerdtree)
