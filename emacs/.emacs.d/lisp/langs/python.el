;;; python --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package! python-mode)
(use-package! lsp-python-ms
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(provide 'python)
;;; python.el ends here
