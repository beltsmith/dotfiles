
(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :after lsp
  :hook (rust-mode . lsp-deferred)
  :commands rustic-run-cargo-command rustic-cargo-outdated
  :config
  (setq rustic-format-on-save t)
  (evil-set-initial-state 'rustic-popup-mode 'emacs))
;; (use-package! flycheck-rust)
;; (use-package! flymake-rust)

(provide 'rust)
