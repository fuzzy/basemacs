
; Load each Emacs Lisp file specified in `base-layer-toggles` from ~/.emacs.d/base/layers/
(dolist (item base-layer-toggles)
  (let* ((layer-name (car item))
         (file-path (expand-file-name (concat layer-name ".el") "~/.emacs.d/base/layers/")))
    (when (and (file-exists-p file-path)
							 (not (equal layer-name "dashboard")))
      (load-file file-path))))

(provide 'base-layers)
