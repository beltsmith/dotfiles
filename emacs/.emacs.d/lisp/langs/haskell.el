(use-package! haskell-mode)
(use-package! haskell-snippets
  :after (yasnippet haskell-mode))

(use-package! intero
  :after haskell-mode)

(use-package! flymake-hlint)
(use-package! hlint-refactor :hook (haskell-mode . hlint-refactor-mode))


(projectile-register-project-type 'haskell-stack '("stack.yaml")
                                  :compile "stack build"
                                  :test "stack build --test"
                                  :run "stack ghci")

(defun stack-cmd (command)
  "Run stack COMMAND for current project."
  (interactive)
  (projectile-compile-project (concat "stack " command)))

(defun stack-ghci ()
  "Run stack ghci for current project."
  (interactive)
  (projectile-run-project))

;;; Keybinds:
(general-create-definer haskell-local-def
  :states '(normal insert emacs)
  :keymaps '(haskell-mode-map enh-ruby-mode-map)
  :prefix "SPC m"
  :non-normal-prefix "M-SPC m"
  :prefix-command 'haskell-local-prefix-command
  :prefix-map 'haskell-local-prefix-map)

(haskell-local-def
  "r" '(nil :which-key "Run")
  "r i" 'stack-ghci)

(provide 'haskell)
