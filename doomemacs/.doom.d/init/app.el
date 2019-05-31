;;; ~/dotfiles/doomemacs/.doom.d/init/apps.el -*- lexical-binding: t; -*-

;; App
;;irc               ; how neckbeards socialize
;;twitter           ; twitter client https://twitter.com/vnought
(doom! :app (rss +org))        ; emacs as an RSS reader
(doom! :app (write            ; emacs as a word processor (latex + org + markdown)
             +wordnut         ; wordnet (wn) search
             +langtool))       ; a proofreader (grammar/style check) for Emacs

(provide 'init/app)
