;;; langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar my-langs-dir (concat my-lisp-dir "/langs"))
(add-to-list 'load-path my-langs-dir)

(require 'ruby)
(require 'scala)
(require 'haskell)
(require 'json)
;(require 'ocaml)

(provide 'langs)
;;; langs.el ends here
