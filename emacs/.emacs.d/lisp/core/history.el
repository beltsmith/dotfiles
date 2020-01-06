;;; history.el -- archeology in EMACS -*- lexical-binding: t; -*-
;;; Commentary:
;;; Sets up my history packages
;;; Code:

(use-package! savehist
  ;; persist variables across sessions
  :config
  (setq savehist-file (concat my-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1)

  (add-hook 'kill-emacs-hook
    (defun my-unpropertize-kill-ring-h ()
      "Remove text properties from `kill-ring' for a smaller savehist file."
      (setq kill-ring (cl-loop for item in kill-ring
                               if (stringp item)
                               collect (substring-no-properties item)
                               else if item collect it)))))


(use-package! saveplace
  ;; persistent point location in buffers
  :config
  (setq save-place-file (concat my-cache-dir "saveplace")
        save-place-limit 100)

  (save-place-mode +1))

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

  ;; (defun my--recent-file-truename (file)
  ;;   (if (or (file-remote-p file nil t)
  ;; 	    (not (file-remote-p file)))
  ;; 	(file-truename file)
  ;;     file))
  ;; (setq recentf-filename-handlers '(my--recent-file-truename abbreviate-file-name))

  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (recentf-mode +1))

(provide 'history)
