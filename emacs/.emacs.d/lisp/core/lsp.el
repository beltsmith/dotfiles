;;; lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package! lsp-mode :commands lsp)
(use-package! lsp-ui :after lsp :commands lsp-ui-mode)
(use-package! company-lsp :after (lsp company) :commands company-lsp)
(use-package! lsp-ivy :after (lsp ivy))

(provide 'lsp)
;;; lsp.el ends here
