;;; langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar my-langs-dir (concat my-lisp-dir "/langs"))
(add-to-list 'load-path my-langs-dir)

(require 'elm)
(require 'reasonml)

(require 'ruby)
(require 'haml)

(require 'scala)

(require 'haskell)

(require 'json)

;(require 'ocaml)
(require 'docker)

(require 'markdown)

(provide 'langs)
;;; langs.el ends here
