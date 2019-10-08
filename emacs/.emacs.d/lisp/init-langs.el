;;; init-langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package! company)
(use-package! company-dict :requires company)
(use-package! company-prescient
  :requires company
  :hook (company-mode . company-prescient-mode))

(use-package! scala-mode
  :after lsp
  :hook (scala-mode . lsp))
(use-package! sbt-mode :requires scala-mode :config (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package! lsp-mode :commands lsp)
(use-package! lsp-ui :after lsp :commands lsp-ui-mode)
(use-package! company-lsp :after (lsp company) :commands company-lsp)
(use-package! ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package! markdown-mode)

(provide 'init-langs)
