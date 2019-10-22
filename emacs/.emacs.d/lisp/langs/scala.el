(use-package! scala-mode
  :after lsp
  :hook (scala-mode . lsp))
(use-package! sbt-mode :requires scala-mode :config (setq sbt:program-options '("-Dsbt.supershell=false")))

(provide 'scala)
