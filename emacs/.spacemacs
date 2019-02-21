;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; (load-file "/home/alex/.emacs.d/private/local/better-spec-mode.el")

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/" "~/.emacs.d/private/local/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(nginx
     d
     react
     graphviz
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     c-c++
     chrome
     colors
     csv
     docker
     ;; (dash :variables helm-dash-docset-path)
     (elfeed :variables rmh-elfeed-org-files (list "~/org/feeds.org"))
     elixir
     erlang
     emacs-lisp
     ;; erc
     ;; (evil-cleverparens :variables evil-cleverparens-use-additional-movement-keys nil)
     evil-commentary
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     ;; eyebrowse
     major-modes
     git
     github
     (go :variables gofmt-command "goimports")
     ;; gnus
     html
     (haskell :variables
              haskell-completion-backend 'intero
              haskell-enable-hindent-style "johan-tibell")
     ;; (ibuffer :variables ibuffer-group-buffers-by 'projects)
     java
     javascript
     lua
     multiple-cursors
     (markdown :variables markdown-live-preview-engine 'vmd)
     mu4e
     (org :variables
          org-enable-github-support t)
     (plantuml :variables
               plantuml-jar-path "/opt/plantuml/plantuml.jar"
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
     prodigy
     (python :variables
             python-enable-yapf-format-on-save t
             python-sort-imports-on-save t
             python-pipenv-activate t
             python-fill-column 79)
     restclient
     ruby-on-rails
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec
           ruby-insert-encoding-magic-comment nil)
     rust
     scheme
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-enable-eldoc-mode t)
     (sql :variables
          sql-capitalize-keywords t
          sql-capitalize-keywords-blacklist '("name"))
     semantic
     (shell :variables
            shell-enable-smart-eshell nil
            shell-default-full-span nil
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t
            shell-default-height 30
            shell-default-position 'bottom)
     vimscript
     (clojure :variables clojure-enable-fancify-symbols t)
     (ivy :variables ivy-enable-advanced-buffer-information t)
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "xerif")
            ("irc.gitter.im"
             :port "6697"
             :ssl t
             :nick "agirdler-lumoslabs")))
     (spacemacs-layouts :variables
                        perspective-enable-persp-projectile t)
     syntax-checking
     themes-megapack
     theming
     version-control
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(beacon pippel ess ess-R-data-view toggle-quotes protobuf-mode)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'doge

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((todos . 5)
                                (recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(sanityinc-tomorrow-eighties
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup t
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))


(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; Setup ensime
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (package-install 'use-package)
  (eval-when-compile (require 'use-package))
  (defun touch ()
    "updates mtime on the file for the current buffer"
    (interactive)
    (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
    (clear-visited-file-modtime))

  (defun my-save ()
    (interactive)
    (if (buffer-modified-p)
        (save-buffer)
      (touch)))

  (define-key evil-normal-state-map (kbd "C-s") 'my-save)

  (evil-define-operator evil-join-one-space (beg end)
    "Join the selected lines with one space between them."
    :motion evil-line
    (let ((count (count-lines beg end)))
      (when (> count 1)
        (setq count (1- count)))
      (goto-char beg)
      (dotimes (var count)
        (end-of-line)
        (join-line 1)
        ;; (backward-char)
        (when (= "(" (string (char-after)))
          (just-one-space)
          (backward-char)))))

  (evil-define-key 'normal 'global
    [(control return)] 'evil-ex
    (kbd "C-;")        'counsel-M-x
    (kbd "C-SPC")      'counsel-projectile
    (kbd "C-W C-h")    'evil-window-left
    (kbd "C-W C-l")    'evil-window-right
    (kbd "C-W C-j")    'evil-window-down
    (kbd "C-W C-k")    'evil-window-up
    "zc"               'vimish-fold
    "zf"               'vimish-fold-toggle
    "zo"               'vimish-fold-unfold
    "zd"               'vimish-fold-delete
    "zD"               'vimish-fold-delete-all
    "zO"               'vimish-fold-unfold-all
    (kbd "H-i")        'evil-jump-forward
    (kbd "M-j")        'move-text-down
    (kbd "M-k")        'move-text-up
    [escape]           'spacemacs/evil-search-clear-highlight
    "gt"               'evil-jump-to-tag
    (kbd "C-e")        'end-of-line ;; make end-of-line work in insert
    (kbd "C-a")        'beginning-of-line ;; make beginning-of-line work in insert
    (kbd "C-c s p")    'ripgrep-regexp
    "\/"               'swiper
    "J"                'evil-join-one-space
    " sap"             'counsel-projectile-rg
    (kbd "C-'")        'toggle-quotes)
  (evil-define-key 'insert 'global
    (kbd "s-i")        'yas-insert-snippet
    (kbd "C-v")        'yank
    (kbd "C-S-l")      'sp-slurp-hybrid-sexp
    (kbd "C-l")        'hippie-expand
    (kbd "C-'")        'toggle-quotes)
  (evil-define-key 'visual 'global
    [(control return)] 'evil-ex
    (kbd "RET")        'align-regexp
    "\/"               'spacemacs/swiper-region-or-symbol
    "\C-g" 'evil-normal-state)
  (evil-define-key 'motion 'global
    [(control return)] 'evil-ex
    "\C-g" 'evil-normal-state)
  (global-set-key [escape] 'evil-exit-emacs-state)

  (keyboard-translate ?\C-i ?\H-i)

  (use-package ivy
    :ensure t
    :init (setq ivy-use-virtual-buffers t
                ivy-re-builders-alist '((t . ivy--regex-plus))
                ivy-count-format "(%d/%d) "
                ivy-initial-inputs-alist nil))


  (spacemacs/set-leader-keys
    "rac" 'js2r-contract-call-args
    "rae" 'js2r-expand-call-args)

  ;; (defun my-compilation-hook ()
  ;;   (when (not (get-buffer-window "*compilation*"))
  ;;     (save-selected-window
  ;;       (save-excursion
  ;;         (let* ((w (split-window-vertically))
  ;;                (h (window-height w)))
  ;;           (select-window w)
  ;;           (switch-to-buffer "*compilation*")
  ;;           (shrink-window (- h compilation-window-height)))))))
  ;; (add-hook 'compilation-mode-hook 'my-compilation-hook)
  (setenv "PATH"
          (concat (getenv "HOME") "/.rbenv/shims:"
                  (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))

  (setq exec-path
        (cons (concat (getenv "HOME") "/.rbenv/shims")
              (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

  (setq-default compilation-scroll-output t
                compilation-window-height 25

                powerline-default-separator nil
                spaceline-minor-modes-p nil

                auto-highlight-symbol-mode t
                global-hl-line-mode nil

                indent-tabs-mode nil
                tab-width 2
                c-basic-offset 2

                js2-basic-offset 2
                js-indent-level 2
                js2-include-node-externs t

                css-indent-offset 2

                which-key-idle-delay 1
                which-key-idle-secondary-delay nil

                nxml-slash-auto-complete-flag t

                org-startup-indented t
                org-startup-folded t
                org-startup-align-all-tables t
                org-startup-with-inline-images t
                org-ellipsis " \u25bc"
                org-indent-mode t
                org-enable-reveal-js-support nil
                ;; org-hide-leading-stars

                evil-escape-key-sequence "jk"
                evil-shift-width 2)

  (eval-after-load 'rspec-mode
    '(rspec-install-snippets))

  (add-hook 'after-init-hook 'inf-ruby-switch-setup)

  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "xdg-open")


  (defun my-python-mode-hook ()
    "I can't beleive it's not butter"
    (evil-define-key 'normal python-mode-map
      "gt" 'anaconda-mode-find-definitions))
  (add-hook 'python-mode-hook 'my-python-mode-hook)


  ;; Prog mode
  (defun my-prog-mode-hook ()
    "Relative number lines for program modes"
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook 'my-prog-mode-hook)

  ;; advise eshell to use rbenv before opening
  (defun my/rbenv (orig-fun &rest args)
    (rbenv-use-corresponding)
    (apply orig-fun args))
  (advice-add 'spacemacs/default-pop-shell :around #'my/rbenv)
  (advice-add 'robe-start :around #'my/rbenv)

  (defun gem-build ()
    (interactive)
    (let (completion-read-command "gem build *.gemspec")
      (rbenv-use-corresponding)
      (projectile-compile-project)))

  (defhydra rails-alternate-files ()
    ("c" projectile-rails-find-current-controller "Controller")
    ("h" projectile-rails-find-current-helper "Helper")
    ("m" projectile-rails-find-current-model "Model")
    ("s" projectile-rails-find-current-spec "Spec")
    ("v" projectile-rails-find-current-view "View"))



  (use-package ruby-mode
    :ensure t
    :preface (progn
               (defun insert-puts-debug ()
                 "Insert puts debug string"
                 (interactive)
                 (end-of-line)
                 (newline-and-indent)
                 (insert "puts '#' * 90, \"#{__FILE__}:#{__LINE__}\".center(90, '*'), caller, '#' * 90 if $debug"))
               (defun my-ruby-mode-hook ()
                 "Ruby mode hooks"
                 (rbenv-use-corresponding)
                 (evil-define-key 'normal ruby-mode-map
                   "gf" 'projectile-rails-goto-file-at-point ;; Goto file
                   "gt" 'robe-jump                           ;; Goto tag

                   "\\m" 'projectile-rails-find-model
                   "\\c" 'projectile-rails-find-controller
                   "\\s" 'projectile-rails-find-spec
                   "\\v" 'projectile-rails-find-view
                   "\\h" 'projectile-rails-find-helper
                   "\\w" 'projectile-rails-find-worker
                   "\\f" 'projectile-rails-find-factory

                   "\\a" 'rails-alternate-files/body

                   "\\t" 'projectile-toggle-between-implementation-and-test)

                 (spacemacs/declare-prefix-for-mode 'ruby-mode "pg" "Projectile GOTO")

                 (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
                   "pgg" 'projectile-rails-goto-gemfile
                   "pgr" 'projectile-rails-goto-routes
                   "pgs" 'projectile-rails-goto-schema
                   "pgh" 'projectile-rails-goto-spec-helper
                   "ipd" 'insert-puts-debug)


                 (setq enh-ruby-deep-indent-paren t
                       enh-ruby-hanging-paren-deep-indent-level 0
                       evil-shift-width 2

                       rbenv-installation-dir "/usr/local/Cellar/rbenv/1.1.1"

                       rspec-use-spring-when-possible t
                       rspec-use-bundler-when-possible t
                       rspec-use-rake-when-possible nil)
                 (robe-mode 1)
                 (ruby-refactor-mode-launch)
                 (eldoc-mode 1)
                 (yard-mode 1)

                 (add-to-list 'write-contents-functions (lambda () (and (rbenv-use-corresponding) nil)))))
    :init (progn
            (dolist (mode-hook '(ruby-mode-hook enh-ruby-mode-hook))
              (add-hook mode-hook 'my-ruby-mode-hook))
            ))

  (use-package rspec-mode
    :ensure t
    :after 'ruby-mode
    :init (progn
            (defadvice rspec-compile (around rspec-compile-around)
              "Use BASH shell for running the specs because of ZSH issues."
              (let ((shell-command-switch "-ic"))
                ad-do-it))
            (ad-activate 'rspec-compile)))

  (use-package projectile-rails
    :ensure t
    :after 'projectile
    :preface (progn
               (defun projectile-rails-find-worker ()
                 (interactive)
                 (projectile-rails-find-resource
                  "worker: "
                  '(("app/workers/" "/workers/\\(.+\\)_worker\\.rb$"))
                  "app/worker/${filename}_worker.rb"))

               (defun projectile-rails-find-factory ()
                 (interactive)
                 (projectile-rails-find-resource
                  "factory: "
                  '(("spec/factories/" "/factories/\\(.+\\)\\.rb$"))
                  "spec/factories/${filename}.rb"))

               (defun load-rails-project (project)
                 "Opens a rails project"
                 (find-file (concat "~/dev/" project "/Gemfile"))
                 (rbenv-use-corresponding)
                 (robe-start 1))

               (defmacro define-rails-layout (project binding)
                 `(spacemacs|define-custom-layout ,project
                    :binding ,binding
                    :body (load-rails-project ,project)))

               (defmacro define-rails-layouts (layouts)
                 (dolist (layout layouts)
                   (let ((project (car layout))
                         (binding (cadr layout)))
                     `(define-rails-layout ,project ,binding))))

               (define-rails-layouts (("lumos_rails" "r") ;; monolith
                                      ("split_test" "s") ;; split tests
                                      ("shop_gem" "g") ;; abstracted shop engine
                                      ("mail_fiend" "m") ;; mailing system
                                      ("lumos-txgh" "t") ;; transifex
                                      ("gym" "w") ;; workouts
                                      ("clerk" "c") ;; workouts
                                      ))))


  (use-package js2-mode
    :ensure t
    :preface (progn
               (defun my-js2-mode-hook ()
                 "JS mode hooks"
                 (evil-define-key 'normal js2-mode-map
                   "zc" js2-mode-hide-element
                   "zo" js2-mode-show-element
                   "zt" js2-mode-toggle-element
                   "zf" js2-mode-hide-functions
                   "zC" js2-mode-hide-comments))
               (add-hook 'js2-mode-hook 'my-js2-mode-hook)))


  (defun my-minesweeper-mode-hook ()
    "Minesweeper mode hooks"
    (evil-define-key 'normal minesweeper-mode-map
      "m" 'minesweeper-toggle-mark
      "s" 'minesweeper-toggle-mark
      "c" 'minesweeper-choose
      "C" 'minesweeper-choose-around))

  (add-hook 'minesweeper-mode-hook 'my-minesweeper-mode-hook)

  (add-to-list 'write-file-functions 'delete-trailing-whitespace)

  ;; (add-to-list 'org-structure-template-alist '("t" "#+TITLE:?"))

  (show-smartparens-global-mode +1)

  ;; Gold sticker if you can tell me what this does
  (sp-local-pair 'ruby-mode "(" nil :post-handlers '((my-add-space-after-sexp-insertion "SPACE")))

  (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

  (defun my-add-sapce-after-sexp-insertion (&rest _ignored)
    "Open a new brace or bracket expression, with relevant spaces"
    (insert "  ")
    (backward-char))



  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (add-hook 'after-save-hook
            #'(lambda ()
                (and (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char (point-min))
                         (save-match-data
                           (looking-at "^#!"))))
                     (not (file-executable-p buffer-file-name))
                     (shell-command (concat "chmod u+x " buffer-file-name))
                     (message
                      (concat "Saved as script: " buffer-file-name)))))

  ;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-visual-state-map           [escape] 'keyboard-quit)
  (define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

  (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn 'cdr))

  ;; Sudo shit
  (defadvice ido-find-file (after find-file-sudo activate)
    "Find file as root if necessary."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (use-package prodigy
    :ensure t
    :config (progn
              (prodigy-define-tag     :name 'pricing-app)
              ;; Ruby versions
              (prodigy-define-tag     :name 'rbenv :init (lambda () (rbenv-use-corresponding)))
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
              ;; bin stubs
              (prodigy-define-tag     :name 'runit      :command "./runit")
              (prodigy-define-tag     :name 'bin-rails  :command "./bin/rails"  :tags '(rails server))
              (prodigy-define-tag     :name 'bin-rake   :command "./bin/rake")
              (prodigy-define-tag     :name 'bin-bundle :command "./bin/bundle")
              ;; environments
              (prodigy-define-tag     :name 'listings :cwd "~/dev/listings"       :path '("/home/alex/dev/flatbook")       :url "www.sonder.local"     :tags '(rbenv))
              (prodigy-define-tag     :name 'flatbook :cwd "~/dev/flatbook"       :path '("/home/alex/dev/listings")       :url "admin.sonder.local"   :tags '(rbenv))
              (prodigy-define-tag     :name 'pricing  :cwd "~/dev/sonder_pricing" :path '("/home/alex/dev/sonder_pricing") :url "pricing.sonder.local" :tags '(python))
              (prodigy-define-tag     :name 'ds-api   :cwd "~/dev/ds_api_server"  :path '("/home/alex/dev/ds_api_server")  :url "dsapi.sonder.local"   :tags '(python))
              ;; services
              ;; redis handled by systemctl for now
              (prodigy-define-service :name "Listings Rails server" :tags '(listings bin-rails server))
              (prodigy-define-service :name "Listings resque"       :tags '(listings resque))
              (prodigy-define-service :name "Listings sidekiq"      :tags '(listings sidekiq))
              (prodigy-define-service :name "Flatbook Rails server" :tags '(flatbook bin-rails))
              (prodigy-define-service :name "Flatbook resque"       :tags '(flatbook resque))
              (prodigy-define-service :name "Pricing flask"         :tags '(pricing make-flask pricing-app))
              (prodigy-define-service :name "Pricing webpack"       :tags '(pricing make-yarn pricing-app))
              (prodigy-define-service :name "DS API server"         :tags '(ds-api runit flask pricing-app))
              ))

  ;; eshell
  (evil-define-key 'normal-mode eshell-mode-map
    (kbd "RET") 'eshell-send-input)

  (defun evil-ret-plus ()
    (interactive)
    (if (eq evil-state 'normal)
        (eshell-send-input)
      (evil-ret)))

  (evil-define-key 'insert org-mode-map [(meta control return)] 'org-insert-subheading)

  (defun my/journal-open ()
    (interactive)
    (let ((org-journal-file "~/org/journal.org"))
      (find-file org-journal-file)))

  (defun my/agenda-open ()
    (interactive)
    (let ((org-agenda-file "~/org/agenda.org"))
      (find-file org-agenda-file)))

  (defun my/udemy-open ()
    (interactive)
    (let ((org-agenda-file "~/org/agenda.org"))
      (find-file org-agenda-file)))

  (spacemacs/set-leader-keys
    "fej" 'my/journal-open
    "fea" 'my/agenda-open
    "feu" 'my/udemy-open
    "Cl" 'org-capture-goto-last-stored)

  ;; ORG Mode
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-agenda-files '("~/org/Agenda.org" "~/org/journal.org" "~/org/calendar.org")
        org-default-notes-file "~/org/notes.org"
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-capture-templates '(("s"                        ; key
                                 "Standup"                  ; Name
                                 entry                      ; type
                                 (file+olp+datetree "~/org/standup.org") ; file
                                 "* Yesterday\n%?\n* Today")
                                ("a"
                                 "Agenda"
                                 entry
                                 (file+olp+datetree "~/org/Agenda.org")
                                 "* TODO %^{Title}  %^G\n%?")
                                ("j"
                                 "Journal"
                                 entry
                                 (file+olp+datetree "~/org/journal.org")
                                 "* %U - %^{Activity}  :TIME:\n%?")
                                ("t"
                                 "Todo"
                                 entry
                                 (file+olp+datetree "~/org/tasks.org")
                                "* TODO %^{Description}  %^g\n%?\nAdded: %U")
                                ("n"
                                 "Note"
                                 entry
                                 (file org-default-notes-file)
                                 "* %^{Description}  %^g\n%?\nAdded: %U")
                                )
        org-refile-targets '((org-agenda-files :maxlevel . 3)
                             (nil :maxlevel . 3)))

  (setq counsel-rg-base-command "rg -S -i -M 120 --no-heading --line-number --color never %s .")

  (add-hook 'flycheck-error-list-mode-hook
            (lambda ()
              (setq flycheck-error-list-format
                    `[("Line" 5 flycheck-error-list-entry-< :right-align t)
                      ("Col" 3 nil :right-align t)
                      ("Level" 8 flycheck-error-list-entry-level-<)
                      ("ID" 20 t)
                      (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]
                    "Table format for the error list.")))

  ;; (require 'haskell-prettify "~/dotfiles/emacs/haskell-prettify.el" t)
  ;; (add-hook 'haskell-mode-hook 'haskell-prettify-enable)
  ;; (add-hook 'haskell-interactive-mode-hook 'haskell-prettify-enable)
  ;; (setq prettify-symbols-unprettify-at-point t)

  (defun gem-name-from-line (line)
    ""
    (save-match-data
      (and (string-match "gem ['\"]\\([^ ]+\\)['\"]" line)
           (match-string 1 line))))

  (defun local-verion-of-gem (gem-name)
    ""
    (format "gem '%s', path: '../%s'" gem-name gem-name))

  (defun localify-gem ()
    "Use local version of the gem on the current line"
    (interactive)
    (beginning-of-line)
    (save-excursion
      (let ((gem-name (gem-name-from-line (thing-at-point 'line))))
        (end-of-line)
        (newline-and-indent)
        (insert (local-verion-of-gem gem-name))))
    (comment-line nil))

  ;; imenu
  (setq imenu-max-item-length 140)

  (setq projectile-keymap-prefix (kbd "C-c C-p"))

  (setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))

  (use-package web-mode
    :ensure t
    :config (progn
              (defun my-web-mode-hook ()
                "Web mode hooks"
                (setq web-mode-markup-indent-offset 2
                      web-mode-code-indent-offset 2
                      web-mode-css-indent-offset 2
                      web-mode-html-tag-font-lock-keywords
                      (list
                       '("\\(</?\\)\\([[:alnum:]_]+\\)"
                         (1 'web-mode-html-tag-bracket-face)
                         (2 'web-mode-html-tag-face))
                       '("\"[^\"]*\"" 0 'web-mode-html-attr-value-face)
                       '("\\([[:alnum:]]+\\)" 1 'web-mode-html-attr-name-face)
                       '("/?>" 0 'web-mode-html-tag-bracket-face))
                      web-mode-html-font-lock-keywords
                      (list
                       '("</?[[:alnum:]_]+[ >]\\|>" 0 'web-mode-html-tag-face t)
                       '(" \\([[:alnum:]-]+=\\)\\(\"[^\"]+\"\\)"
                         (1 'web-mode-html-attr-name-face)
                         (2 'web-mode-html-attr-value-face))
                       )))

              (add-hook 'web-mode-hook 'my-web-mode-hook)))

  (use-package beacon :ensure t :config (beacon-mode 1))
  ;; (use-package rufo :ensure t :different-type)
  (use-package ruby-refactor :ensure t)
  ;; (use-package yard-mode :ensure t)

  ;; (use-package rg
  ;;   :ensure t
  ;;   :config (progn
  ;;             (defun my-ripgrep-mode-hook ()
  ;;               "RIPGREP ALL THE THINGS"
  ;;               (evil-define-key 'normal ripgrep-search-mode-map
  ;;                 "r" 'recompile
  ;;                 "q" 'quit-window
  ;;                 "o" 'compilation-display-error))
  ;;             (add-hook 'ripgrep-search-mode-hook 'my-ripgrep-mode-hook)))

  (use-package dash :ensure t)
  (use-package autothemer :ensure t)
  ;; (use-package challenger-deep-theme :ensure t)
  (use-package hydra :ensure t)
  ;; (use-package multiple-cursors :ensure t)
  (use-package editorconfig :ensure t)
  ;; (use-package emojify :ensure t)
  (use-package org-journal :ensure t :disabled)

  (use-package doom-modeline
    :config
    (setq doom-modeline-icon nil))

  (when (version<= "9.2" (org-version))
    (require 'org-tempo))

  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

  (use-package plantuml-mode :ensure t)
  (use-package semantic
    :config
    (setq-mode-local emacs-lisp-mode
                     semanticdb-find-default-throttle
                     (default-value 'semanticdb-find-default-throttle)))

  (defun et/semantic-remove-hooks ()
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-notc-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-nolongprefix-completion-at-point-function))

  (add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)

  (use-package ess :ensure t)
  (use-package ess-R-data-view :ensure t)
  (use-package toggle-quotes :ensure t)
  (use-package protobuf-mode :ensure t)

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (use-package mu4e
    :commands (mu4e)
    :config
    (setq mu4e-get-mail-command "offlineimap"
          mu4e-sent-messages-behavior 'delete

          mu4e-drafts-folder "/[Gmail].Drafts"
          mu4e-sent-folder   "/[Gmail].Sent Mail"
          mu4e-trash-folder  "/[Gmail].Trash"

          mu4e-maildir-shortcuts '(("/INBOX"               . ?i)
                                   ("/[Gmail].Sent Mail"   . ?s)
                                   ("/[Gmail].Trash"       . ?t)
                                   ("/[Gmail].All Mail"    . ?a))

          user-mail-address "alex.girdler@sonder.com"
          user-full-name  "Alex Girdler"
          mu4e-compose-signature (concat
                                  "Alex Girdler"
                                  "Senior Software Engineer : Sonder")))

  (use-package smtpmail
    :ensure t
    :config
    (setq message-send-mail-function 'smtpmail-send-it
          starttls-use-gnutls t
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
          smtpmail-auth-credentials '(("smtp.gmail.com" 587 "alex.girdler@sonder.com" nil))
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587))


  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override t)
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme zen-and-art-theme yasnippet-snippets yarn-mode yapfify yaml-mode xterm-color ws-butler writeroom-mode wolfram-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vmd-mode vimrc-mode vi-tilde-fringe vala-snippets vala-mode uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-evil toxi-theme toml-mode toggle-quotes toc-org thrift tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stickyfunc-enhance stan-mode srefactor sqlup-mode sql-indent spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shell-pop seti-theme seeing-is-believing scss-mode scad-mode sass-mode rvm rufo ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe rjsx-mode rg reverse-theme restart-emacs request rebecca-theme rbenv rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer qml-mode pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode protobuf-mode projectile-rails professional-theme prodigy prettier-js plantuml-mode planet-theme pkgbuild-mode pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode password-generator paradox ox-gfm overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http ob-elixir npm-mode noctilux-theme nginx-mode neotree naquadah-theme nameless mwim mvn mustang-theme multi-term mu4e-maildirs-extension mu4e-alert move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme meghanada maven-test-mode matlab-mode material-theme markdown-toc majapahit-theme magithub magit-svn magit-gitflow magit-gh-pulls madhat2r-theme macrostep lush-theme lorem-ipsum logcat livid-mode live-py-mode link-hint light-soap-theme kivy-mode kaolin-themes json-navigator js2-refactor js-doc jbeans-theme jazz-theme ivy-yasnippet ivy-xref ivy-rtags ivy-rich ivy-purpose ivy-hydra ir-black-theme intero inkpot-theme indent-guide importmagic impatient-mode hungry-delete hoon-mode hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation highlight heroku-theme hemisu-theme helm-make hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme groovy-mode groovy-imports graphviz-dot-mode grandshell-theme gradle-mode gotham-theme google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gmail-message-mode gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md geiser gandalf-theme fuzzy forge font-lock+ flymd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-mix flycheck-haskell flycheck-credo flx-ido flatui-theme flatland-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-org evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu ess-R-data-view espresso-theme eshell-z eshell-prompt-extras esh-help erlang erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks ensime enh-ruby-mode emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig edit-server ebuild-mode dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline dockerfile-mode docker django-theme disaster diminish diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode d-mode cython-mode cyberpunk-theme csv-mode counsel-projectile counsel-css company-web company-tern company-statistics company-rtags company-restclient company-lua company-go company-emacs-eclim company-dcd company-cabal company-c-headers company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmm-mode clues-theme clojure-snippets clean-aindent-mode clang-format cider-eval-sexp-fu cider chruby cherry-blossom-theme centered-cursor-mode cargo busybee-theme bundler bubbleberry-theme browse-at-remote birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile arduino-mode apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme ace-link ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
