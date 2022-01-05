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
      ;; doom-font (font-spec :family "FuraMono Nerd Font Mono" :size 14)
      doom-font (font-spec :family "FiraCode Nerd Font" :size 14)
      ;; doom-theme 'doom-dracula
      doom-theme 'doom-molokai)

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
  (kbd "C-;")        'counsel-M-x
  (kbd "C-SPC")      'counsel-projectile
  "gt"               'evil-jump-to-tag
  (kbd "C-e")        'end-of-line ;; make end-of-line work in insert
  (kbd "C-a")        'beginning-of-line ;; make beginning-of-line work in insert
  "\/"               'swiper
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

(evil-define-key 'normal scad-preview--image-mode-map
  "n" 'scad-preview-dist+
  "p" 'scad-preview-dist-
  "k" 'scad-preview-rotx-
  "j" 'scad-preview-rotx+
  "h" 'scad-preview-rotz-
  "l" 'scad-preview-rotz+
  (kbd "<left>") 'scad-preview-trnsx-
  (kbd "<right>") 'scad-preview-trnsx+)

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

;; (let ((conf-files '("prodigy" "exercism"))
;;       (conf-dir "config"))
;;   (dolist (file conf-files)
;;     (require-relative (concat conf-dir "/" file ".el"))))

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
    (forward-line 0)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun my/ruby-insert-frozen-string-magic-comment ()
  "Insert frozen string literal magic comment at the top of the buffer if it does not exist."
  (interactive)
  (let ((frozen-string-magic-comment "# frozen_string_literal: true")
        (first-line (buffer-first-line)))
    (pcase first-line
      ((pred (string= frozen-string-magic-comment)) nil)
      (_ (save-excursion
           (forward-line 0)
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
         :name "Rails Server Debug")))

(defun chmod-this-file ()
  (interactive)
  (let ((file-modes (read-file-modes)))
    (set-file-modes (buffer-file-name) file-modes)))

(defun chmodx-this-file ()
  (interactive)
  (set-file-modes (buffer-file-name) "+x"))

(require-relative "config/org.el")
(require-relative "config/prolog.el")

;(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(after! direnv-mode (direnv-mode))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

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
