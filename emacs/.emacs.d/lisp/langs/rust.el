
(use-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :after lsp
  :hook (rustic-mode-local-vars . lsp!)
  :commands rustic-run-cargo-command rustic-cargo-outdated
  :init
  (add-to-list 'org-src-lang-modes '("rust" . rustic))
  :config
  (setq rustic-format-on-save t
        rustic-indent-method-chain t)
  (evil-set-initial-state 'rustic-popup-mode 'emacs))

;; (use-package! flycheck-rust)
;; (use-package! flymake-rust)

(provide 'rust)
