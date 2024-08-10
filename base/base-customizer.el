
;;
;; Helper functions for the customization subsystem
;;

;; define the group
(defun base/generate-set-data (tags-and-modes)
  "Generate set data for defcustom from TAGS-AND-MODES list."
  (mapconcat (lambda (pair)
               (format "(const :tag \"%s\" %s)" (car pair) (cadr pair)))
             tags-and-modes
             "\n                          "))

(defun base/generate-defcustom (custom-var-name custom-var-desc tags-and-modes custom-group)
  "Generate and evaluate a defcustom block.
CUSTOM-VAR-NAME is the name of the custom variable.
TAGS-AND-MODES is a list of pairs where each pair is (tag mode).
CUSTOM-GROUP is the group for the custom variable."
  (let* ((set-data (base/generate-set-data tags-and-modes))
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
;; Define the custom variables for the BaseMacs configuration.
;;

(defgroup base-config nil
	"Emacs configuration group."
  :group 'convenience
  :prefix "base-")

(base/generate-defcustom 'base-layers
													"Layers of supported features to turn on. (These will create further customization groups)"
													base-layer-toggles
													'base-config)

(message "BaseMacs: Loaded base-config group.")
(provide 'base-customizer)
