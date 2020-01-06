;;; init-preamble.el --- -*- lexical-binding: t; -*-
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
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" local-emacs-directory))
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

(defmacro use-package! (name &rest plist)
  "Use package NAME with straight passing rest PLIST to `use-package'."
  (declare (indent 1))
  `(use-package ,name
     :straight t
     ,@plist))


(provide 'preamble)
;;; preamble.el ends here
