;;; ~/dotfiles/doomemacs/.doom.d/init/ui.el -*- lexical-binding: t; -*-

;; UI
;;deft              ; notational velocity for Emacs
;;fill-column               ; a `fill-column' indicator
;;indent-guides     ; highlighted indent columns
;;neotree           ; a project drawer, like NERDTree for vim
;;pretty-code       ; replace bits of code with pretty symbols
;;tabbar            ; FIXME an (incomplete) tab bar for Emacs
;;unicode           ; extended unicode support for various languages
(doom! :ui doom)              ; what makes DOOM look the way it does
(doom! :ui doom-dashboard)    ; a nifty splash screen for Emacs
(doom! :ui doom-quit)         ; DOOM quit-message prompts when you quit Emacs
(doom! :ui ophints)      ; display visual hints when editing in evil
(doom! :ui hl-todo)           ; highlight TODO/FIXME/NOTE tags
(doom! :ui modeline)          ; snazzy, Atom-inspired modeline, plus API
(doom! :ui nav-flash)         ; blink the current line after jumping
(doom! :ui treemacs)          ; a project drawer, like neotree but cooler
(doom! :ui (popup            ; tame sudden yet inevitable temporary windows
            +all             ; catch all popups that start with an asterix
            +defaults))       ; default popup rules
(doom! :ui vc-gutter)         ; vcs diff in the fringe
(doom! :ui vi-tilde-fringe)   ; fringe tildes to mark beyond EOB
(doom! :ui window-select)     ; visually switch windows
(doom! :ui workspaces)        ; tab emulation, persistence & separate workspaces

(provide 'init/ui)
