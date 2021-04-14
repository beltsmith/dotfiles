;;; conf/prodigy.el -*- lexical-binding: t; -*-


(defvar my--prodigy-rails-tags
  '((:name 'thin    :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")
    (:name 'webrick :ready-message "WEBrick::HTTPServer#start: pid=[0-9]+ port=[0-9]+")
    (:name 'mongrel :ready-message "Ctrl-C to shutdown server")
    (:name 'unicorn :ready-message "master process ready")
    (:name 'puma    :ready-message "Use Ctrl-C to stop")
    (:name 'rails   :tags '(thin mongrel webrick unicorn puma)))
  "Prodigy tags for rails servers.")

(defvar my--prodigy-flask-tags
  '((:name 'flask-prod :ready-message " * Running on ")
    (:name 'flask-dev  :ready-message " * Debugger is active!")
    (:name 'python     :stop-signal 'sigkill)
    (:name 'flask      :tags '(flask-dev flask-prod python)))
  "Prodigy tags for flask servers.")

(defvar my--prodigy-version-managers
  '((:name 'rbenv :init (lambda () (global-rbenv-mode) (rbenv-use-corresponding)))))

(defvar my--prodigy-commands
  '((:name bundled     :command "bundle")
    (:name sprung      :command "spring")
    (:name memcached   :command "memcached")
    (:name redis       :command "redis-server" :ready-message "ready to accept connections")
    (:name make        :command "make")
    (:name runit       :command "./runit")
    (:name bin-rails   :command "./bin/rails"  :tags (rails server))
    (:name bin-rake    :command "./bin/rake")
    (:name bin-bundle  :command "./bin/bundle")
    (:name npm         :command "npm")
    (:name yarn        :command "yarn" :ready-message "Compiled successfully" :stop-signal sigkill)
    (:name webpack     :command "./bin/webpack" :ready-message "Compiled successfully" :stop-signal sigkill)
    (:name mailcatcher :command "mailcatcher") :ready-message "==> http://127.0.0.1:1080" :stop-signal sigkill))

(defvar my--prodigy-command-args
  '((:name resque    :args ("exec" "environment" "resque:work")         :ready-message "app init time"       :tags (bundled))
    (:name sidekiq   :args ("exec" "sidekiq" "-C" "config/sidekiq.yml") :ready-message "Starting processing" :tags (bundled))
    (:name kafka-srv :args ("exec" "kafka" "server")                    :ready-message "Starting processing" :tags (bundled))
    (:name server    :args ("server")))
  "Prodigy tags for arguments to commands.")

(defvar my--prodigy-full-commands
  '((:name make-flask  :tags (make flask server))
    (:name make-yarn   :tags (make yarn)   :args ("yarn"))
    (:name runit-flask :tags (runit flask) :args ("backend"))
    (:name runit-yarn  :tags (runit flask) :args ("frontend"))
    (:name npm-server  :tags (npm)         :args ("run" "server"))
    (:name npm-start   :tags (npm)         :args ("start"))))

(defvar my--prodigy-projects
  '((:name prevail  :cwd "~/dev/prevail"          :path ("~/dev/prevail")          :url "prevail.local"     :tags (rbenv))
    (:name the-hand :cwd "~/dev/thecat/the-hand/" :path ("~/dev/thecat/the-hand/") :url "http://localhost:8000"))
  "Prodigy tags to relate metadata to projects.")

(defvar my--prodigy-services
  '((:name "Prevail Rails server" :tags (prevail bin-rails server))
    (:name "Prevail webpack"       :tags (prevail webpack))
    (:name "Prevail mailcatcher"   :tags (prevail mailcatcher))
    (:name "The Hand server"       :tags (the-hand npm-server))
    (:name "The Hand bucklescript" :tags (the-hand npm-start))))

(defvar my--prodigy-tags
  (append my--prodigy-rails-tags
          my--prodigy-flask-tags
          my--prodigy-version-managers
          my--prodigy-commands
          my--prodigy-command-args
          my--prodigy-full-commands
          my--prodigy-projects))


(after! prodigy
  (setq prodigy-tags my--prodigy-tags
        prodigy-services my--prodigy-services)
  (add-hook 'prodigy-mode-hook (lambda () (evil-snipe-local-mode -1)))
  (general-def prodigy-mode-map
    "s" 'prodigy-start
    "S" 'prodigy-stop
    "R" 'prodigy-restart)

  (general-def 'normal 'global
    :prefix "SPC"
    "a p" 'prodigy))



(setq +popup-defaults '(:side left :height 0.50 :width 40 :quit t :select ignore :ttl 5))

(set-popup-rules! '(("^\\*prodigy\\*" :side left :height 0.05 :width 100 :vslot 1 :select 1 :ttl nil :quit nil)
                    ("^\\*prodigy-" :side right :height 0.05 :width 200 :vslot 1 :select 1 :ttl nil :quit nil)
                    ("^\\*eldoc" :side right :select 1 :ttl nil :quit nil)))
