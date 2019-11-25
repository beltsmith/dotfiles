;;; lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package! company :config (global-company-mode +1))
(use-package! company-dict :requires company)
(use-package! company-prescient
  :requires company
  :hook (company-mode . company-prescient-mode))

(use-package! lsp-mode :commands lsp)
(use-package! lsp-ui :after lsp :commands lsp-ui-mode)
(use-package! company-lsp :after (lsp company) :commands company-lsp)

(provide 'lsp)
;;; lsp.el ends here
