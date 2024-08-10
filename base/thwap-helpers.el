;;
;; Helper functions to generate customization stuff



;;
;; Helper functions for keybindings
;;

(defun thwap/add-key-binding (key command help-text)
	"Add a key binding to the T.H.W.A.P. keymap"
	(define-key thwap-map (kbd key) command)
	(add-to-list 'thwap-help-lines (format "%-12s: %s" (concat "C-c t " key) help-text)))

;;
;; Helper functions for files
;;

(defun thwap/touch-file (filename)
  "Create an empty file with FILENAME, or update its modification timestamp if it exists."
  (with-temp-buffer
    (write-region (point-min) (point-min) filename)))


;;
;; Helper functions for Hydra
;;

(defun thwap/create-hydra-menu (name menu-string input-data-list)
  "Create a hydra menu with NAME, MENU-STRING, and INPUT-DATA-LIST."
  (let ((formatted-menu-items "")
        (commands '())
        (counter 0))
    ;; Format the menu items with newlines after every third item
    (dolist (item input-data-list)
      (setq formatted-menu-items
            (concat formatted-menu-items
                    (format "_%s_: %-20s" (nth 0 item) (nth 1 item))))
      (let ((cmd (nth 2 item)))
        (setq commands (append commands
                               (list (list (nth 0 item)
                                           (if (symbolp cmd)
                                               `(lambda () (interactive) (call-interactively ',cmd))
                                             `(lambda () (interactive) (funcall ,cmd))))))))
      (setq counter (1+ counter))
      (when (= (mod counter 3) 0)
        (setq formatted-menu-items (concat formatted-menu-items "\n"))))
    ;; Add the quit command to the formatted menu and commands
    (setq formatted-menu-items (concat formatted-menu-items "\n\n_q_: Quit\n"))
    (setq commands (append commands '(("q" nil :exit t))))
    ;; Ensure there are no extra spaces or newlines
    (setq formatted-menu-items (string-trim-right formatted-menu-items))
    ;; Debugging: Print formatted-menu-items and commands
    (message "Formatted Menu Items: %s" formatted-menu-items)
    (message "Commands: %s" commands)
    ;; Combine the hydra body, formatted menu items, and commands
    (eval `(defhydra ,name (:color blue :hint nil)
             ,(concat "\n" menu-string "\n"
                      "-----------------------------------------------------\n"
                      formatted-menu-items
                      "\n")
             ,@(mapcar (lambda (cmd)
                         `(,(nth 0 cmd) ,(nth 1 cmd)))
                       commands)))))

;;
;; Helper functions for the dashboard
;;

(defun thwap/dashboard-insert-logo-title (banner)
	"Insert BANNER into the dashboard buffer.
BANNER can be a single string, which will be centered, or a list of strings,
which will be displayed with line breaks between them."
	(insert "\n")
	;; if banner is a single string, display it in the center
	;; else if it is a list of strings, display them with line breaks
	(if (stringp banner)
			(insert (propertize banner 'face 'dashboard-banner-logo-title))
		(dolist (line banner)
			(insert (propertize line 'face 'dashboard-banner-logo-title))
			(insert "\n"))))

(defun thwap/dashboard-build-logo-title (lst)
	"Build a list of strings from LST to display as the banner.
LST is reversed and concatenated into a single string with line breaks."
	(mapconcat 'identity (reverse lst) "\n"))

(defun thwap/random-string-from-list (strings)
	"Return a random string from STRINGS."
  (let ((index (random (length strings))))
    (nth index strings)))

;;
;; Helper functions for general use
;;

;; Function to create a directory if it doesn't exist
(defun thwap/ensure-directory-exists (dir)
  "Ensure the directory DIR exists. If not, create it."
  (unless (file-directory-p dir)
		(message "Creating missing directory: %s" dir)
    (make-directory dir t)))

;; Function to list all files in a directory with a given extension
(defun thwap/list-files-with-extension (dir extension)
  "Recursively list all files in DIR with the given EXTENSION, suitable for org-agenda-files."
  (let ((files '()))
    (dolist (file (directory-files-recursively dir (concat "\\." extension "\\'")))
      (push file files))
    (nreverse files)))

;;
;; Helper functions for org mode
;;

;; Function to get a unique filename for Org-capture
(defun thwap/org-capture-get-unique-filename ()
  "Generate a unique filename for Org-capture."
  (let ((filename (format "~/.org-agenda/syncup__issue__%s.org" (format-time-string "%Y%m%d%H%M%S"))))
    (message "Inside function: Generated filename: %s" filename)
    filename))

;; Function to update org-agenda-files
(defun thwap/org-agenda-files-update ()
	"Update the `org-agenda-files` variable.
This function sets `org-agenda-files` and `org-timeblock-files` to the list of
all `.org` files in the `~/.org-agenda` directory, and sets
`org-timeblock-inbox-file` to `~/.org-agenda/tasks.org`."
  (setq org-agenda-files (thwap/list-files-with-extension "~/.org-agenda" "org"))
  (setq org-timeblock-files (thwap/list-files-with-extension "~/.org-agenda" "org"))
  (setq org-timeblock-inbox-file "~/.org-agenda/tasks.org"))


(message "T.H.W.A.P. Loaded helper function library.")
(provide 'thwap-helpers)
;;; thwap-helpers.el ends here
