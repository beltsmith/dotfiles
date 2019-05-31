;;; ~/dotfiles/doomemacs/.doom.d/init/emacs.el -*- lexical-binding: t; -*-

;; Emacs?
;; term              ; terminals in Emacs
(doom! :emacs (dired            ; making dired pretty [functional]
               ;;+ranger         ; bringing the goodness of ranger to dired
               ;;+icons          ; colorful icons for dired-mode
               ))
(doom! :emacs electric)          ; smarter, keyword-based electric-indent
(doom! :emacs eshell)            ; a consistent, cross-platform shell (WIP)
(doom! :emacs imenu)             ; an imenu sidebar and searchable code index
(doom! :emacs vc)                ; version-control and Emacs, sitting in a tree

(provide 'init/emacs)
