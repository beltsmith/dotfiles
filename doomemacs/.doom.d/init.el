;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(add-to-list 'load-path "~/.doom.d")

(load "init/completion")
(load "init/ui")
(load "init/editor")
(load "init/emacs")
(load "init/tools")
(load "init/lang")
(load "init/email")
(load "init/app")
(load "init/collab")
(load "init/config")
(load "init/term")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d0c943c37d6f5450c6823103544e06783204342430a36ac20f6beb5c2a48abe3" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
