;;; core.el --- -*- lexical-binding: t; -*-
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

(defvar my-org-dir (expand-file-name "~/org")
  "Directory containing my org files.")

(defvar my-core-dir (concat my-lisp-dir "core/")
  "Directory containing my Lisp core init files.")

(defvar my-init-el (concat my-emacs-dir "init.el")
  "My init.el file for Emacs.")

;; Add core lib to load path
(add-to-list 'load-path my-core-dir)

;; Setup straight.el
(require 'preamble)

;; setup general early
(use-package! general)

(require 'settings)
(require 'hooks)

(require 'files)

(require 'look-and-feel)
(require 'company)
(require 'ivy)

(require 'smartparens)
(require 'init-flycheck)
;; (setq gc-cons-threshold most-positive-fixnum)
(require 'yasnippet)

(require 'init-dired)

(require 'history)

(require 'help)

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

(require 'magit)

(require 'windows)
(require 'navigation)

(require 'lsp)

(require 'buffers)

(provide 'core)
;;; core.el ends here
