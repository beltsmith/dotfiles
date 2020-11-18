;;; init-preamble.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Preamble provides use-package! macro and sets up package archives
;;; Code:
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(defvar local-emacs-directory (concat user-emacs-directory "/.local"))
(defvar my-straight-directory (concat local-emacs-directory "/straight" ))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "repos/straight.el/bootstrap.el" my-straight-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defun my-emacs-purge-packages ()
  "Purge straight build directory to start from a fresh state."
  (delete-directory (concat my-straight-directory "/build")))

(defmacro use-package! (name &rest plist)
  "Use package NAME with straight passing rest PLIST to `use-package'."
  (declare (indent 1))
  `(use-package ,name
     :straight t
     ,@plist))

(use-package! auto-compile
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(use-package! auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

(provide 'preamble)
;;; preamble.el ends here
