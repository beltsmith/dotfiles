;;; ~/dotfiles/doomemacs/.doom.d/init/completion.el -*- lexical-binding: t; -*-

;; Completion
;; helm              ; the *other* search engine for love and life
;;ido              ; the other *other* search engine...
(doom! :completion company)           ; the ultimate code completion backend
(doom! :completion (ivy +fuzzy))              ; a search engine for love and life

(provide 'init/completion)
