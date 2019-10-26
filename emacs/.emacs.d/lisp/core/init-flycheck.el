;; -*- lexical-binding: t -*-

(use-package! flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(provide 'init-flycheck)
