;;; keys.el --- ~/.emacs.d/init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file sets up keybinds for core Emacs functionality.
;;; Code:
(use-package! which-key
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  ;(setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-mode +1))

(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    ivy-minibuffer-map
    ivy-switch-buffer-map)
  "A list of all the keymaps used for the minibuffer.")

(general-define-key :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-v"    #'yank
  "C-z"    (lambda (ignore-errors (call-interactively #'undo)))
  "C-a"    #'move-beginning-of-line
  "C-b"    #'backward-word
  "C-r"    #'evil-paste-from-register
  ;; Scrolling lines
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

(general-define-key :kepmaps read-expression-map
  "C-j" 'next-line-or-history-element
  "C-k" 'previous-line-or-history-element)

(general-translate-key nil 'normal
  "SPC w" "C-w")

(require 'finders)
(provide 'keys)
;;; keys.el ends here
