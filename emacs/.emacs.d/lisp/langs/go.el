;;; go --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package! go-mode
  :after lsp
  :hook (go-mode . lsp-deferred)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package! go-eldoc)
(use-package! go-dlv)
(use-package! go-snippets)
(use-package! go-projectile)
(use-package! go-add-tags)
(use-package! go-fill-struct)

(provide 'go)
;;; go.el ends here
