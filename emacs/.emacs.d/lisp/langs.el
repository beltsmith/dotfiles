;;; langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package! elf-mode)

(defvar my-langs-dir (concat my-lisp-dir "/langs"))
(add-to-list 'load-path my-langs-dir)

(defvar my-enabled-langs
  '(json
    csv
    ccode
    elm
    reasonml
    go
    ruby
    haml
    scala
    haskell
    docker
    markdown
    ocaml
    plantuml
    rust
    graphviz-dot
    gql))

(dolist (lang my-enabled-langs) (require lang))

;; (require 'tex)

(provide 'langs)
;;; langs.el ends here
