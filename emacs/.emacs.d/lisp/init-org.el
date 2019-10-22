(use-package! org
  :ensure org-plus-contrib
  :config
  (org-indent-mode 1))
(use-package! org-bullets :after org)
(use-package! evil-org :after (org evil))
(use-package! orgit :after org)
(use-package! org-tempo)

(provide 'init-org)
