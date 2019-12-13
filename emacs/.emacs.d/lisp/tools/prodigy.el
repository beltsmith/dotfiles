;;; prodigy.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  '((:name bundled    :command "bundle")
    (:name sprung     :command "spring")
    (:name memcached  :command "memcached")
    (:name redis      :command "redis-server" :ready-message "ready to accept connections")
    (:name make       :command "make")
    (:name runit      :command "./runit")
    (:name bin-rails  :command "./bin/rails"  :tags (rails server))
    (:name bin-rake   :command "./bin/rake")
    (:name bin-bundle :command "./bin/bundle")
    (:name npm        :command "npm")
    (:name yarn       :command "yarn" :ready-message "Compiled successfully" :stop-signal sigkill)))

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
    (:name npm-server  :tags (npm)	   :args ("run" "server"))
    (:name npm-start   :tags (npm)	   :args ("start"))))

(defvar my--prodigy-projects
  '((:name listings :cwd "~/dev/listings"       :path ("~/dev/flatbook")       :url "www.sonder.local"     :tags (rbenv))
    (:name flatbook :cwd "~/dev/flatbook"       :path ("~/dev/listings")       :url "admin.sonder.local"   :tags (rbenv))
    (:name pricing  :cwd "~/dev/sonder_pricing" :path ("~/dev/sonder_pricing") :url "pricing.sonder.local" :tags (python))
    (:name ds-api   :cwd "~/dev/ds_api_server"  :path ("~/dev/ds_api_server")  :url "dsapi.sonder.local"   :tags (python))
    (:name the-hand :cwd "~/dev/thecat/the-hand/" :path ("~/dev/thecat/the-hand/") :url "http://localhost:8000"))
  "Prodigy tags to relate metadata to projects.")

(defvar my--prodigy-services
  '((:name "Listings Rails server" :tags (listings bin-rails server))
    (:name "Listings resque"       :tags (listings resque))
    (:name "Listings sidekiq"      :tags (listings sidekiq))
    (:name "Flatbook Rails server" :tags (flatbook bin-rails pricing-stack))
    (:name "Flatbook resque"       :tags (flatbook resque))
    (:name "Pricing flask"         :tags (pricing runit-flask pricing-app pricing-stack))
    (:name "Pricing webpack"       :tags (pricing make-yarn pricing-app pricing-stack))
    (:name "DS API server"         :tags (ds-api runit flask pricing-stack))
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

(use-package! prodigy
  :config
  (setq prodigy-tags my--prodigy-tags
	prodigy-services my--prodigy-services))

(general-def 'normal 'global
  :prefix "SPC"
  "a p" 'prodigy)

(provide 'prodigy)
;;; prodigy.el ends here
