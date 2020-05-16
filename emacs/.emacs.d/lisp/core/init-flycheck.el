;; -*- lexical-binding: t -*-

(use-package! flycheck
  :init (progn (global-flycheck-mode)
               (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package! flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(provide 'init-flycheck)
