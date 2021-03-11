;;; ~/.doom.d/config.el

;; (defun my-prog-mode-hook ()
;;   "Relative number lines for program modes"
;;   (setq display-line-numbers 'relative))

;; (setq-default display-line-numbers 'relative)

;; (add-hook 'prog-mode-hook #'my-prog-mode-hook)

(setq js-indent-level 2)
(remove-hook 'window-size-change-functions #'+doom-dashboard-resize-h)

(setq org-log-into-drawer nil
      display-line-numbers-type 'visual
      doom-font (font-spec :family "FuraMono Nerd Font Mono" :size 14)
      doom-theme 'doom-dracula)

(after! toggle-quotes
  (general-def :states 'normal
    (kbd "C-'") 'toggle-quotes))

(defun org-insert-today ()
  (interactive)
  (org-insert-time-stamp (current-time)))

(evil-define-key '(normal insert) org-mode-map
  (kbd "C-c t") 'org-insert-today)

(evil-define-key 'normal 'global
  [(control return)] 'evil-ex
  ;; (kbd "C-;")        'counsel-M-x
  ;; (kbd "C-SPC")      'counsel-projectile
  ;; "zc"            'vimish-fold
  ;; "zf"            'vimish-fold-toggle
  ;; "zo"            'vimish-fold-unfold
  ;; "zd"            'vimish-fold-delete
  ;; "zD"            'vimish-fold-delete-all
  ;; "zO"            'vimish-fold-unfold-all
  ;; (kbd "H-i")        'evil-jump-forward
  ;; (kbd "M-j")        'move-text-down
  ;; (kbd "M-k")        'move-text-up
      ;; [escape]           'spacemacs/evil-search-clear-highlight
  ;; "gt"               'evil-jump-to-tag
  ;; (kbd "C-e")        'end-of-line ;; make end-of-line work in insert
  ;; (kbd "C-a")        'beginning-of-line ;; make beginning-of-line work in insert
  ;; (kbd "C-c s p")    'ripgrep-regexp
  "\/"               'swiper
  ;; "J"                'evil-join-one-space
  " sap"             '+ivy/project-search
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

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

(general-def :states 'normal
  (kbd "M-j") 'move-line-down
  (kbd "M-k") 'move-line-up)

(evil-define-key 'normal org-mode-map
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-h") 'org-metaleft
  (kbd "M-l") 'org-metaright)

(evil-define-key 'normal 'global
  (kbd "M-C-y") 'browse-at-remote
  (kbd "M-Y") 'browse-at-remote-kill)
(evil-define-key 'normal ruby-mode-map
  "gt" 'robe-jump)
(evil-define-key 'normal enh-ruby-mode-map
  "gt" 'robe-jump)

;; org
(defun my/blog-file-by-date ()
  "Create an Org file with current date as name."
  (find-file (format-time-string "~/dev/blog/mustacheriders/_posts/%Y-%m-%d--%H-%M-%S.org")))

(setq org-capture-templates
      '(("b" "Blog" entry
         (file my/blog-file-by-date)
         "
#+TITLE: %(i)%^{LAYOUT}
#+STARTED: %T
#+LAYOUT: post
#+TAGS: jekyll org-mode belt")))

;; (defun my-ruby-mode-hook ()
;;   (setq flycheck-command-wrapper-function (lambda (command) (append '("bundle" "exec") command))))
;; (add-hook 'ruby-mode-hook #'my-ruby-mode-hook)

(after! prettier-js (add-hook 'js-mode-hook 'prettier-js-mode))

;; SQL
(set-popup-rule! "*SQL*" :ignore t)

;; rust
(setq rustic-lsp-server 'rust-analyzer
      lsp-rust-analyzer-cargo-watch-command "clippy"
      rustic-format-on-save t)

(set-popup-rule! "*cargo-test*" :side 'bottom :height 30 :vslot 9)

;; lsp
(setq lsp-ui-doc-enable t
      lsp-enable-on-type-formatting t
      lsp-enable-indentation nil)

;; exercism
(defun exercism-cmd (cmd &rest args)
  "Run exercism CMD with ARGS."
  (shell-command (mapconcat 'identity (cons "exercism" (cons cmd args)) " ")))

(defun exercism-submit ()
  "Submit current file to exercism."
  (interactive)
  (exercism-cmd "submit" buffer-file-name))


;; TODO: Setup table of tracks and exercises
(defun exercism-download ()
  "Download exercise for a track."
  (interactive)
  (exercism-cmd "download"
                (concat "--exercise=" (read-string "Exercise: "))
                (concat "--track=" (read-string "Track: "))))

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
;                    (flycheck-mode :size 0.2)
;                    (interactive-haskell-mode :align 'bottom :size 0.3)
;                    (prodigy-mode :ignore t)
;                    (eww-mode :ignore t))

(defun buffer-first-line ()
  (interactive)
  (save-excursion
    (goto-line 0)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun my/ruby-insert-frozen-string-magic-comment ()
  "Insert frozen string literal magic comment at the top of the buffer if it does not exist."
  (interactive)
  (let ((frozen-string-magic-comment "# frozen_string_literal: true")
        (first-line (buffer-first-line)))
    (pcase first-line
      ((pred (string= frozen-string-magic-comment)) nil)
      (_ (save-excursion
           (goto-line 0)
           (insert frozen-string-magic-comment "\n\n"))))))

;; (add-hook 'ruby-mode 'rufo-minor-mode)
(setq rufo-minor-mode-use-bundler t)
(setq-hook! 'ruby-mode-hook +format-with-lsp nil)
(setq-hook! 'ruby-mode-hook +format-with 'rufo)

;; dap-mode
(after! dap-mode
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltips))
  (dap-auto-configure-mode 1)

  ;; enables mouse hover support
  ;; (dap-tooltip-mode 1)
  (require 'dap-ruby)
  (setq dap-ruby-debug-program `("/home/belt/.asdf/shims/node"
                                    ,(f-join dap-ruby-debug-path "extension/dist/debugger/main.js")))

  (defun my-dap-ruby--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (plist-put :dap-server-path dap-ruby-debug-program)
        (dap--put-if-absent :cwd (lsp-workspace-root))
        (dap--put-if-absent :program (concat (lsp-workspace-root) "/bin/rails"))
        (dap--put-if-absent :type "Rails")
        (dap--put-if-absent :name "Rails")))
  (dap-register-debug-provider "Rails" 'my-dap-ruby--populate-start-file-args)

  (dap-register-debug-template
   "Prevail::Rails::Server"
   (list :type "Rails"
         :request "launch"
         :args "s"
         :cwd "/home/belt/dev/prevail"
         :program "/home/belt/dev/prevail/bin/rails"
         :name "Rails Server Debug"))
  )

(require 'org-habit)
(setq org-modules '(ol-notmuch ol-bibtext ol-eww ol-gnus habits)
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-show-habits-only-for-today nil
      org-habit-show-all-today t
      org-habit-today-glyph ?‖
      org-habit-completed-glyph ?✓)

(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(platformio-mode arduino-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
