(use-package! elm-mode
  :custom
  (elm-reactor-command '("npx" "elm" "reactor"))
  (elm-interactive-command '("npx" "elm" "repl"))
  :config
  (setq elm-format-on-save t))

(use-package! flycheck-elm
  :after flycheck
  :hook (flycheck-mode . flycheck-elm-setup))

(provide 'elm)
