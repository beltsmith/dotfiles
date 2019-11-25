(use-package! elm-mode)
(use-package! flycheck-elm
  :after (flycheck elm)
  :config
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))


(provide 'elm)
