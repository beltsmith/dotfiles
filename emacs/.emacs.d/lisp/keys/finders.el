;;; lisp/init-finders.el -*- lexical-binding: t; -*-

(defun edit-my-init ()
  "Edit my init.el."
  (interactive)
  (find-file my-init-el))

(defun edit-my-lisp ()
  "Open dired in my Lisp directory."
  (interactive)
  (find-file my-lisp-dir))

(defun edit-my-org ()
  "Open dired in my org directory."
  (interactive)
  (find-file my-org-dir))

(defun edit-my-snippets ()
  "Edit my snippets directory."
  (interactive)
  (find-file my-snippets-dir))

(defun load-my-init ()
  "Eval my init.el to reload Emacs config."
  (interactive)
  (load-file my-init-el))

(defvar my-dotfiles-dir (expand-file-name "~/dotfiles")
  "Directory containing my dotfiles.")

(defun edit-my-dotfiles ()
  "Edit my dotfiles directory."
  (interactive)
  (find-file my-dotfiles-dir))

;; (add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun chmodx ()
  "Set current file eXecutable."
  (interactive)
  (let* ((current-mode (file-modes (buffer-file-name)))
	 (add-mode (logand ?\111 (default-file-modes)))
	 (final-mode (logior current-mode add-mode)))
    (set-file-modes (buffer-file-name) final-mode)))

(defun delete-this-file ()
  "Deletes current buffer from disk."
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer))

(defun rename-this-file ()
  "Renames file visited by current buffer and swap to it."
  (interactive)
  (let* ((old-buffer (buffer-name))
	 (current-file (buffer-file-name))
	 (new-file-name (read-file-name (format "Rename %s to: " current-file))))
    (rename-file current-file new-file-name)
    (find-file new-file-name)
    (kill-buffer old-buffer)))

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
  "e o" 'edit-my-org
  "e s" 'edit-my-snippets
  "e d" 'edit-my-dotfiles
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
