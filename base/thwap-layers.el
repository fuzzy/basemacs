
; Load each Emacs Lisp file specified in `thwap-layer-toggles` from ~/.emacs.d/thwap/layers/
(dolist (item thwap-layer-toggles)
  (let* ((layer-name (car item))
         (file-path (expand-file-name (concat layer-name ".el") "~/.emacs.d/thwap/layers/")))
    (when (and (file-exists-p file-path)
							 (not (equal layer-name "dashboard")))
      (load-file file-path))))

(provide 'thwap-layers)
