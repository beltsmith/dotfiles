;;; ~/dotfiles/doomemacs/.doom.d/init/lang.el -*- lexical-binding: t; -*-


;; Lang
;;assembly          ; assembly for fun or debugging
(doom! :lang common-lisp)       ; if you've seen one lisp, you've seen them all
;;coq               ; proofs-as-programs
;;csharp            ; unity, .NET, and mono shenanigans
;;elm               ; care for a cup of TEA?
;; hy                ; readability of scheme w/ speed of python
;;idris             ;
;; (java +meghanada) ; the poster child for carpal tunnel syndrome
;;julia             ; a better, faster MATLAB
;;latex             ; writing papers in Emacs has never been so fun
;;ledger            ; an accounting system in Emacs
;;nim               ; python + lisp at the speed of c
;;ocaml             ; an objective camel
;;perl              ; write code no one else can comprehend
;;php               ; perl's insecure younger brother
;;purescript        ; javascript, but functional
;;python            ; beautiful is better than ugly
;;qt                ; the 'cutest' gui framework ever
;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
;;solidity          ; do you need a blockchain? No.
;;swift             ; who asked for emoji variables?
;;terra             ; Earth and Moon in alignment for performance.
;;vala              ; GObjective-C
(doom! :lang (cc +irony +rtags)); C/C++/Obj-C madness
(doom! :lang clojure)           ; java with a lisp
(doom! :lang crystal)           ; ruby at the speed of c
(doom! :lang data)              ; config/data formats
(doom! :lang erlang)            ; an elegant language for a more civilized age
(doom! :lang elixir)            ; erlang done right
(doom! :lang emacs-lisp)        ; drown in parentheses
(doom! :lang ess)               ; emacs speaks statistics
(doom! :lang go)                ; the hipster dialect
(doom! :lang (haskell +intero)) ; a language that's lazier than I am
(doom! :lang (javascript +lsp))        ; all(hope(abandon(ye(who(enter(here))))))
(doom! :lang lua)               ; one-based indices? one-based indices
(doom! :lang markdown)          ; writing docs for people to ignore
(doom! :lang nix)               ; I hereby declare "nix geht mehr!"
(doom! :lang (org              ; organize your plain life in plain text
              +attach          ; custom attachment system
              +babel           ; running code in org
              +capture         ; org-capture in and outside of Emacs
              +export          ; Exporting org to whatever you want
              +present))        ; Emacs for presentations
(doom! :lang plantuml)          ; diagrams for confusing people more
(doom! :lang racket)            ; a DSL for DSLs
(doom! :lang rest)              ; Emacs as a REST client
(doom! :lang ruby)              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
(doom! :lang scala)             ; java, but good
(doom! :lang (sh +zsh))        ; she sells (ba|z|fi)sh shells on the C xor
(doom! :lang web)               ; the tubes

(provide 'init/lang)
