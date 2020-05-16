;;; tools.el --- -*- lexical-binding: t; -*-
;;; Commentary: tools for my emacs config
;;; Code:

(defvar my-tools-dir (concat my-lisp-dir "tools/"))
;; Add core lib to load path
(add-to-list 'load-path my-tools-dir)

(defvar my-enabled-tools '(searching
                           projects
                           prodigy
                           qls
                           exercism))

(require-all my-enabled-tools)

(provide 'tools)
