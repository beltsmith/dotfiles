;;; langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar my-langs-dir (concat my-lisp-dir "/langs"))
(add-to-list 'load-path my-langs-dir)

;; file types
(require 'json)
(require 'csv)

;; Front end
(require 'elm)
(require 'reasonml)

(require 'ruby)
(require 'haml)

(require 'scala)

(require 'haskell)

(require 'docker)

(require 'markdown)

(require 'ocaml)
(require 'plantuml)
(require 'rust)

(provide 'langs)
;;; langs.el ends here
