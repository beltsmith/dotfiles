;;; ~/dotfiles/doomemacs/.doom.d/init/editor.el -*- lexical-binding: t; -*-

;; Editor
;;lispy             ; vim for lisp, for people who dont like vim
;;parinfer          ; turn lisp into python, sort of
(doom! :editor (evil +everywhere)); come to the dark side, we have cookies
(doom! :editor file-templates)    ; auto-snippets for empty files
(doom! :editor fold) ; folding
(doom! :editor (format +onsave))  ; automated prettiness
(doom! :editor snippets)          ; my elves. They type so I don't have to
(doom! :editor multiple-cursors)  ; editing in many places at once
(doom! :editor rotate-text)       ; cycle region at point between text candidates

(provide 'init/editor)
