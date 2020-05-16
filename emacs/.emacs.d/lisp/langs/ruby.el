;;
;;; Commentary:
;;; Code:
;;; Packages:

(defvar rspec-matchers
  '("receive" "match")
  "List of rspec matchers.")

(defvar rspec-dsl-keywords
  (append '("describe" "context")
          '("it" "specify" "is_expected")
          '("before" "after" "around" )
          '("allow" "expect")
          '("subject" "subject!" "let" "let!"))
  "List of rspec dsl keywords.")

(defvar rspec-keywords
  (append rspec-dsl-keywords rspec-matchers)
  "List of rspec keywords to fontify with enh-ruby-mode.")

(use-package! enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :custom
  (enh-ruby-extra-keywords rspec-keywords)
  :config
  (setq display-line-numbers 'visual
        display-line-numbers-type 'visual
        display-line-numbers-grow-only t))

(use-package! bundler)

(use-package! rufo
  :after 'enh-ruby-mode
  :hook (enh-ruby-mode . rufo-minor-mode))

(use-package! yard-mode
  :after 'enh-ruby-mode
  :hook enh-ruby-mode)

(use-package! rubocop
  :after 'enh-ruby-mode
  :hook enh-ruby-mode)

(use-package! robe
  :after 'enh-ruby-mode
  :hook enh-ruby-mode)

(use-package! inf-ruby)
(use-package! company-inf-ruby)

(use-package! ruby-end
  :hook enh-ruby
  :config
  ;; Define as config block so that it takes precedence over ruby-end
  (defconst ruby-end-expand-postfix-modifiers-before-re
    "\\(?:unless\\|while\\)"
    "Regular expression matching statements before point.")
  (defconst ruby-end-expand-keywords-before-re
    "\\(?:^\\|\\s-+\\)\\(?:def\\|class\\|module\\|case\\|for\\|begin\\)"
    "Regular expression matching blocks before point, do is omitted as it is handled elsewhere."))


;;; Functions:
(defun rake-db-migrate ()
  "Rake db migrate this project."
  (interactive)
  (bundle-exec "rake db:migrate"))

(defun my-bundle-console ()
  "Run bundle exec rails c for this project."
  (interactive)
  (bundle-exec "rails c"))

;;; Keybinds:
(general-create-definer ruby-local-def
  :states '(normal insert emacs)
  :keymaps '(ruby-mode-map enh-ruby-mode-map)
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
  "b c" 'my-bundle-console
  "b e" 'bundle-exec
  "b i" 'bundle-install)

;; (ruby-local-def
;;   "g" '(nil :which-key "Goto")
;;   "g g" 'rails-goto-gemfile)

(general-def
  :states '(normal insert emacs)
  :keymaps 'enh-ruby-mode-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "TAB" 'projectile-toggle-between-implementation-and-test)

(provide 'ruby)
