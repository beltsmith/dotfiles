;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
(defun my-prog-mode-hook ()
  "Relative number lines for program modes"
  (setq display-line-numbers 'relative))
(setq display-line-numbers 'relative)
(add-hook 'prog-mode-hook #'my-prog-mode-hook)
(setq doom-font (font-spec :family "FuraMono NF" :size 14))
(setq js-indent-level 2)
(remove-hook 'window-size-change-functions #'+doom-dashboard|resize)
(evil-define-key 'normal 'global
  [(control return)] 'evil-ex
  (kbd "C-;")        'counsel-M-x
  (kbd "C-SPC")      'counsel-projectile
  ;; "zc"            'vimish-fold
  ;; "zf"            'vimish-fold-toggle
  ;; "zo"            'vimish-fold-unfold
  ;; "zd"            'vimish-fold-delete
  "zD"            'vimish-fold-delete-all
  ;; "zO"            'vimish-fold-unfold-all
  (kbd "H-i")        'evil-jump-forward
  ;; (kbd "M-j")        'move-text-down
  ;; (kbd "M-k")        'move-text-up ;; [escape]           'spacemacs/evil-search-clear-highlight
  "gt"               'evil-jump-to-tag
  (kbd "C-e")        'end-of-line ;; make end-of-line work in insert
  (kbd "C-a")        'beginning-of-line ;; make beginning-of-line work in insert
  (kbd "C-c s p")    'ripgrep-regexp
  "\/"               'swiper
  ;; "J"                'evil-join-one-space
  " sap"             'counsel-projectile-rg
  (kbd "C-'") 'toggle-quotes
  " Cl"               'org-capture-goto-last-stored
  (kbd "M-y") 'counsel-yank-pop)

(evil-define-key 'insert 'global
  (kbd "s-i")        'yas-insert-snippet
  (kbd "C-v")        'yank
  (kbd "C-S-l")      'sp-slurp-hybrid-sexp
  (kbd "C-l")        'hippie-expand
  (kbd "C-'")        'toggle-quotes)

(evil-define-key 'visual 'global
  ",er" 'eval-region
  [(control return)] 'evil-ex
  (kbd "RET")        'align-regexp
  ;; "\/"               'spacemacs/swiper-region-or-symbol
  (kbd "C-g") 'evil-normal-stateq)

(evil-define-key 'motion 'global
  [(control return)] 'evil-ex
  (kbd "C-g") 'evil-normal-state)

(global-set-key [escape] 'evil-exit-emacs-state)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Sudo shit
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(evil-define-key 'insert org-mode-map [(meta control return)] 'org-insert-subheading)
(defun evil-unimpaired-insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired-insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(evil-define-key 'normal 'global
  "[ " 'evil-unimpaired-insert-space-above
  "] " 'evil-unimpaired-insert-space-below)

(evil-define-key 'normal 'global
  (kbd "M-j") 'move-line-down
  (kbd "M-k") 'move-line-up)

(evil-define-key 'normal 'org-mode
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-h") 'org-metaleft
  (kbd "M-l") 'org-metaright)
(after! prodigy
  (prodigy-define-tag     :name 'pricing-app)
  ;; Ruby versions
  (prodigy-define-tag     :name 'rbenv :init (lambda () (global-rbenv-mode) (rbenv-use-corresponding)))
  ;; Python
  (prodigy-define-tag     :name 'python :stop-signal 'sigkill)
  ;; Prefix Commands
  (prodigy-define-tag     :name 'bundled   :command "bundle")
  (prodigy-define-tag     :name 'sprung    :command "spring")
  (prodigy-define-tag     :name 'memcached :command "memcached")
  (prodigy-define-tag     :name 'redis     :command "redis-server" :ready-message "ready to accept connections")
  (prodigy-define-tag     :name 'mysqld    :command "mysqld"       :ready-message "ready for connections" :stop-signal 'kill)
  (prodigy-define-tag     :name 'make      :command "make")
  ;; Rails servers
  (prodigy-define-tag     :name 'thin      :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")
  (prodigy-define-tag     :name 'webrick   :ready-message "WEBrick::HTTPServer#start: pid=[0-9]+ port=[0-9]+")
  (prodigy-define-tag     :name 'mongrel   :ready-message "Ctrl-C to shutdown server")
  (prodigy-define-tag     :name 'unicorn   :ready-message "master process ready")
  (prodigy-define-tag     :name 'puma      :ready-message "Use Ctrl-C to stop")
  (prodigy-define-tag     :name 'rails     :tags '(thin mongrel webrick unicorn puma))
  ;; Flask servers
  (prodigy-define-tag     :name 'flask-prod :ready-message " * Running on ")
  (prodigy-define-tag     :name 'flask-dev  :ready-message " * Debugger is active!")
  (prodigy-define-tag     :name 'flask      :tags '(flask-dev flask-prod))
  ;; Runnable commands
  (prodigy-define-tag     :name 'resque    :args '("exec" "environment" "resque:work")         :ready-message "app init time"       :tags '(bundled))
  (prodigy-define-tag     :name 'sidekiq   :args '("exec" "sidekiq" "-C" "config/sidekiq.yml") :ready-message "Starting processing" :tags '(bundled))
  (prodigy-define-tag     :name 'kafka-srv :args '("exec" "kafka" "server")                    :ready-message "Starting processing" :tags '(bundled))
  (prodigy-define-tag     :name 'yarn      :command "yarn"                                     :ready-message "Compiled successfully" :stop-signal 'sigkill)
  (prodigy-define-tag     :name 'server'   :args '("server"))
  ;; combos
  (prodigy-define-tag     :name 'make-flask :tags '(make flask server))
  (prodigy-define-tag     :name 'make-yarn  :args '("yarn")   :tags '(make yarn))
  (prodigy-define-tag     :name 'runit-flask :args '("backend") :tags '(runit flask))
  (prodigy-define-tag     :name 'runit-yarn :args '("frontend") :tags '(runit flask))
  ;; bin stubs
  (prodigy-define-tag     :name 'runit      :command "./runit")
  (prodigy-define-tag     :name 'bin-rails  :command "./bin/rails"  :tags '(rails server))
  (prodigy-define-tag     :name 'bin-rake   :command "./bin/rake")
  (prodigy-define-tag     :name 'bin-bundle :command "./bin/bundle")
  ;; environments
  (prodigy-define-tag     :name 'listings :cwd "~/dev/listings"       :path '("~/dev/flatbook")       :url "www.sonder.local"     :tags '(rbenv))
  (prodigy-define-tag     :name 'flatbook :cwd "~/dev/flatbook"       :path '("~/dev/listings")       :url "admin.sonder.local"   :tags '(rbenv))
  (prodigy-define-tag     :name 'pricing  :cwd "~/dev/sonder_pricing" :path '("~/dev/sonder_pricing") :url "pricing.sonder.local" :tags '(python))
  (prodigy-define-tag     :name 'ds-api   :cwd "~/dev/ds_api_server"  :path '("~/dev/ds_api_server")  :url "dsapi.sonder.local"   :tags '(python))
  (prodigy-define-tag     :name 'pricing-stack)
  ;; services
  ;; redis handled by systemctl for now
  (prodigy-define-service :name "Listings Rails server" :tags '(listings bin-rails server))
  (prodigy-define-service :name "Listings resque"       :tags '(listings resque))
  (prodigy-define-service :name "Listings sidekiq"      :tags '(listings sidekiq))
  (prodigy-define-service :name "Flatbook Rails server" :tags '(flatbook bin-rails pricing-stack))
  (prodigy-define-service :name "Flatbook resque"       :tags '(flatbook resque))
  (prodigy-define-service :name "Pricing flask"         :tags '(pricing runit-flask pricing-app pricing-stack))
  (prodigy-define-service :name "Pricing webpack"       :tags '(pricing make-yarn pricing-app pricing-stack))
  (prodigy-define-service :name "DS API server"         :tags '(ds-api runit flask pricing-stack))
  )

(evil-define-key 'normal 'global " ap" 'prodigy)
(set-popup-rules!
  '(("*SQL*" :ignore t)))
(add-hook 'js-mode-hook 'prettier-js-mode)
(load-theme 'gruvbox-dark-medium t)
