;;; lisp/init-finders.el -*- lexical-binding: t; -*-

(defun edit-my-init ()
  "Edit my init.el."
  (interactive)
  (find-file my-init-el))

(defun edit-my-lisp ()
  "Open dired in my Lisp directory."
  (interactive)
  (find-file my-lisp-dir))

(defun load-my-init ()
  "Eval my init.el to reload Emacs config."
  (interactive)
  (load-file my-init-el))

(use-package! recentf
  ;; Keep track of recently opened files
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat my-cache-dir "recentf")
	recentf-auto-cleanup 'never
	recentf-max-menu-items 0
	recentf-max-saved-items 200
	recentf-exclude
	(list "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
	      "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
	      ;; ignore private DOOM temp files
	      (lambda (path)
		(ignore-errors (file-in-directory-p path my-local-dir)))))

  (defun my--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
	    (not (file-remote-p file)))
	(file-truename file)
      file))
  (setq recentf-filename-handlers '(my--recent-file-truename abbreviate-file-name))

  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (recentf-mode +1))


(general-create-definer file-def
  :states '(normal insert emacs)
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f"
  :prefix-command 'find-file-prefix-command
  :prefix-map 'find-file-prefix-map)

(file-def
  "r" 'counsel-recentf
  "s"   'save-buffer
  "e" '(nil :which-key "Edit")
  "e i" 'edit-my-init
  "e l" 'edit-my-lisp
  "l" '(nil :which-key "Load")
  "l i" 'load-my-init
  "x" 'delete-this-file)

(general-def
  :states 'normal
  :prefix "SPC e"
  :non-normal-prefix "M-SPC e")

(general-def :states 'normal :prefix "SPC"
  "." 'find-file)

(provide 'finders)
