;;; ~/dotfiles/doomemacs/.doom.d/init/tools.el -*- lexical-binding: t; -*-

;; Tools
;;ansible
;;ein               ; tame Jupyter notebooks with emacs
;;gist              ; interacting with github gists
;;macos             ; MacOS-specific commands
;;rgb               ; creating color strings
;;terraform         ; infrastructure as code
;;tmux              ; an API for interacting with tmux
;;upload            ; map local to remote projects via ssh/ftp
;;wakatime
;;vterm             ; another terminals in Emacs
(doom! :tools eval)              ; run code, run (also, repls)
(doom! :tools docker)
(doom! :tools editorconfig)      ; let someone else argue about tabs vs spaces
(doom! :tools flyspell)
(doom! :tools flycheck)
(doom! :tools (lookup           ; helps you navigate your code and documentation
               +docsets))        ; ...or in Dash docsets locally
(doom! :tools magit)             ; a git porcelain for Emacs
(doom! :tools make)              ; run make tasks from Emacs
(doom! :tools store)    ; password manager for nerds
(doom! :tools pdf)               ; pdf enhancements
(doom! :tools prodigy)           ; FIXME managing external services & code builders
(doom! :tools lsp)
(provide 'init/tools)
