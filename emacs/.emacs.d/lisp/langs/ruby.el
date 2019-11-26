;;; Packages:

(use-package! ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package! bundler)

(use-package! rufo
  :after 'ruby-mode
  :hook (ruby-mode . rufo-minor-mode))

;;; Functions:
(defun rake-db-migrate ()
  "Rake db migrate this project."
  (interactive)
  (bundle-exec "rake db:migrate"))

;;; Keybinds:
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
  "t b" 'rspec-verify
  "t a" 'rspec-verify-all
  "t m" 'rspec-verify-matching
  "t s" 'rspec-verify-single
  "t t" 'rspec-verify-single
  "t c" 'rspec-verify-continue
  "t d" '(nil :which-key "Dired")
  "t d v" 'rspec-dired-verify
  "t d s" 'rspec-dired-verify-single
  "r" '(nil :which-key "Rake")
  "r m" 'rake-db-migrate
  "b" '(nil :which-key "Bundle")
  "b c" 'bundle-console
  "b e" 'bundle-exec
  "b i" 'bundle-install)

(general-def
  :states '(normal insert emacs)
  :keymaps 'ruby-mode-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "TAB" 'projectile-toggle-between-implementation-and-test)

(provide 'ruby)
