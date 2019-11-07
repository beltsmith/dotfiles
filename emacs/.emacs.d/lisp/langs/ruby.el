(use-package! ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(general-create-definer ruby-local-def
  :states '(normal insert emacs)
  :keymaps 'ruby-mode-map
  :prefix "SPC m"
  :non-normal-prefix "M-SPC m"
  :prefix-command 'ruby-local-prefix-command
  :prefix-map 'ruby-local-prefix-map)

;; Rspec mappings attached to ruby-mode-map to allow executing tests from source
(ruby-local-def
  "t" '(nil :which-key "Test")
  "t v" 'rspec-verify
  "t a" 'rspec-verify-all
  "t m" 'rspec-verify-matching
  "t s" 'rspec-verify-single
  "t t" 'rspec-verify-single
  "t c" 'rspec-verify-continue
  "t d" '(nil :which-key "Dired")
  "t d v" 'rspec-dired-verify
  "t d s" 'rspec-dired-verify-single)

(general-def
  :states '(normal insert emacs)
  :keymaps 'ruby-mode-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "TAB" 'projectile-toggle-between-implementation-and-test)

(provide 'ruby)
