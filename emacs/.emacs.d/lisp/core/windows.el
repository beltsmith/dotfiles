;;; windows --- Provides windowing support for my emacs config
;;; Commentary:
;;; Emacs now supports windows
;;; Code:

(use-package! ace-window
  :config
  ;; Enable homerow keys for ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (general-def
    "s-o" 'ace-window)
  ;; Define keys on evil-window-map to use everywhere
  (general-def evil-window-map
    "m" 'ace-swap-window
    "g" 'ace-window))

(use-package! shackle
  :config
  (setq shackle-default-size 0.3
        shackle-rules '((compilation-mode :noselect t :align 'below)
                        (flycheck-mode :align 'below)
                        (flycheck-error-list-mode :popup t :align 'below)
                        (magit-status-mode :same t :inhibit-window-quit t)
                        )
        shackle-default-rule '(:select t))
  :init
  (shackle-mode))

(provide 'windows)
;;; windows.el ends here
