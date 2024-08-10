
;;
;; Helper functions for the customization subsystem
;;

;; define the group
(defun thwap/generate-set-data (tags-and-modes)
  "Generate set data for defcustom from TAGS-AND-MODES list."
  (mapconcat (lambda (pair)
               (format "(const :tag \"%s\" %s)" (car pair) (cadr pair)))
             tags-and-modes
             "\n                          "))

(defun thwap/generate-defcustom (custom-var-name custom-var-desc tags-and-modes custom-group)
  "Generate and evaluate a defcustom block.
CUSTOM-VAR-NAME is the name of the custom variable.
TAGS-AND-MODES is a list of pairs where each pair is (tag mode).
CUSTOM-GROUP is the group for the custom variable."
  (let* ((set-data (thwap/generate-set-data tags-and-modes))
        (code (format ";; %s layer toggles
(defcustom %s nil
  \"%s\"
  :type '(set (const :tag \"none\" nil)
                          %s)
  :group '%s)\n\n"
                      custom-var-name
                      custom-var-name
											custom-var-desc
                      set-data
                      custom-group)))
    (eval (read code))))

;;
;; Define the custom variables for the T.H.W.A.P. Emacs configuration.
;;

(defgroup thwap-config nil
	"T.H.W.A.P. Emacs configuration group."
  :group 'convenience
  :prefix "thwap-")

(thwap/generate-defcustom 'thwap-layers
													"Layers of supported features to turn on. (These will create further customization groups)"
													thwap-layer-toggles
													'thwap-config)

(message "T.H.W.A.P. Loaded thwap-config group.")
(provide 'thwap-customizer)
