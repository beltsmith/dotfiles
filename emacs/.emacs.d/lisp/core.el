;;; init-core.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defvar my-local-dir (concat my-emacs-dir "local/")
  "Local file storage for Emacs.")

(defvar my-etc-dir (concat my-emacs-dir "etc/")
  "Etc file storage for Emacs.")

(defvar my-private-dir (concat my-emacs-dir "private/")
  "Private file storage for Emacs.")

(defvar my-cache-dir (concat my-emacs-dir "cache/")
  "Cache file storage for Emacs.")

(defvar my-lisp-dir (concat my-emacs-dir "lisp/")
  "Directory containing my Lisp files.")

(defvar my-core-dir (concat my-lisp-dir "core/")
  "Directory containing my Lisp core init files.")

(defvar my-init-el (concat my-emacs-dir "init.el")
  "My init.el file for Emacs.")

;; Add core lib to load path
(add-to-list 'load-path my-core-dir)

(require 'settings)
(require 'hooks)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun chmodx ()
  "Set current file executable."
  (interactive)
  (chmod (buffer-file-name) "+x"))

(defun delete-this-file ()
  "Deletes current buffer from disk."
  (interactive)
  (delete-file (buffer-file-name)))

(defun rename-this-file ()
  "Renames file visited by current buffer and swap to it."
  (interactive)
  (let* ((old-buffer (buffer-name))
	 (current-file (buffer-file-name))
	 (new-file-name (read-file-name (format "Rename %s to: " current-file))))
    (rename-file current-file new-file-name)
    (find-file new-file-name)
    (kill-buffer old-buffer)))

;; Prefer y/n over yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(use-package! helpful)
(use-package! general)

(require 'look-and-feel)
(require 'ivy)

(require 'smartparens)
(require 'init-flycheck)
;; (setq gc-cons-threshold most-positive-fixnum)
(require 'yasnippet)

(require 'init-dired)

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

(use-package! helpful
  ;; a better *help* buffer
  :after apropos
  :commands helpful--read-symbol
  :init
  (define-key
    [remap describe-function] #'helpful-callable
    [remap describe-command]  #'helpful-command
    [remap describe-variable] #'helpful-variable
    [remap describe-key]      #'helpful-key
    [remap describe-symbol]   #'doom/describe-symbol)

  (defun my-use-helpful-a (orig-fn &rest args)
    "Force ORIG-FN to use helpful instead of the old describe-* commands."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable))
      (apply orig-fn args)))

  ;; patch apropos buttons to call helpful instead of help
  (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
    (button-type-put
     fun-bt 'action
     (lambda (button)
       (helpful-callable (button-get button 'apropos-symbol)))))
  (dolist (var-bt '(apropos-variable apropos-user-option))
    (button-type-put
     var-bt 'action
     (lambda (button)
       (helpful-variable (button-get button 'apropos-symbol))))))

(use-package! undo-tree
  ;; Branching & persistent undo
  :config
  (setq undo-tree-auto-save-history nil ; disable because unstable
        ;; undo-in-region is known to cause undo history corruption, which can
        ;; be very destructive! Disabling it deters the error, but does not fix
        ;; it entirely!
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist
        `(("." . ,(concat my-cache-dir "undo-tree-hist/"))))

  (global-undo-tree-mode +1))

(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :config
  (ws-butler-global-mode))

(use-package! rainbow-delimiters :config (rainbow-delimiters-mode +1))

(require 'magit)

(provide 'core)
;;; init-core.el ends here
