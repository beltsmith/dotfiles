;;; ~/dotfiles/doomemacs/.doom.d/packages/prodigy.el -*- lexical-binding: t; -*-

(package! prodigy)

(evil-define-key 'normal 'global " ap" 'prodigy)

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
  (prodigy-define-service :name "DS API server"         :tags '(ds-api runit flask pricing-stack)))

(provide 'packages/prodigy)
