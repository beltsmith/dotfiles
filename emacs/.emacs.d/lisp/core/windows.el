;;; windows --- Provides windowing support for my emacs config
;;; Commentary:
;;; Emacs now supports windows
;;; Code:

(use-package! ace-window
  :after 'general
  :init
  ;; Enable homerow keys for ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  ;; Define keys on evil-window-map to use everywhere
  (define-key evil-window-map "m" 'ace-swap-window)
  (define-key evil-window-map "g" 'ace-window))

(provide 'windows)
;;; windows.el ends here
