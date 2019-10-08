;;; init-core.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defvar my-local-dir (concat my-emacs-dir "local/")
  "Local file storage for Emacs.")

(defvar my-etc-dir (concat my-emacs-dir "etc/")
  "Etc file storage for Emacs.")

(defvar my-private-dir (concat my-emacs-dir "private/")
  "Private file storage for Emacs.")

(defvar my-cache-dir (concat my-emacs-dir "cache/")
  "Cache file storage for Emacs.")

(defvar my-lisp-dir (concat my-emacs-dir "/lisp/")
  "Directory containing my Lisp files.")

(defvar my-core-dir (concat my-lisp-dir "/core/")
  "Directory containing my Lisp core init files.")

(defvar my-init-el (concat my-emacs-dir "/init.el")
  "My init.el file for Emacs.")

;; Disable all the window chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Add core lib to load path
(add-to-list 'load-path my-core-dir)

;;
;;; File handling

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
;; (add-hook 'find-file-not-found-functions
;;   (defun my--create-missing-directories-h ()
;;     "Automatically create missing directories when creating new files."
;;     (let ((parent-directory (file-name-directory buffer-file-name)))
;;       (when (and (not (file-exists-p parent-directory))
;;                  (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
;;         (make-directory parent-directory t)))))


;; Don't autosave files or create lock/history/backup files. The
;; editor doesn't need to hold our hands so much. We'll rely on git
;; and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name (concat my-cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat my-cache-dir "backup/"))))

;;
;;; Formatting

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t)  ; for :retab

;;
;;; Clipboard / kill-ring

 ;; Eliminate duplicates in the kill ring. That is, if you kill the
 ;; same thing twice, you won't have to use M-y twice to get past it
 ;; to older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

;; Save clipboard contents into kill-ring before replacing them
(setq save-interprogram-paste-before-kill t)

;; Bunch of shit I lifted from doom
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;; Emacs is a huge security vulnerability, what with all the dependencies it
;; pulls in from all corners of the globe. Let's at least try to be more
;; discerning.
(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program '("gnutls-cli --x509cafile %t -p %p %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"
                    "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; Emacs stores authinfo in HOME and in plaintext. Let's not do that, mkay? This
;; file usually stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
(setq auth-sources (list (expand-file-name "authinfo.gpg" my-etc-dir)
                         "~/.authinfo.gpg"))

(setq abbrev-file-name             (concat my-local-dir "abbrev.el")
      async-byte-compile-log-file  (concat my-etc-dir "async-bytecomp.log")
      bookmark-default-file        (concat my-etc-dir "bookmarks")
      custom-file                  (concat my-private-dir "init.el")
      custom-theme-directory       (concat my-private-dir "themes/")
      desktop-dirname              (concat my-etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat my-cache-dir "pcache/")
      request-storage-directory    (concat my-cache-dir "request")
      server-auth-dir              (concat my-cache-dir "server/")
      shared-game-score-directory  (concat my-etc-dir "shared-game-score/")
      tramp-auto-save-directory    (concat my-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat my-cache-dir "tramp-persistency.el")
      url-cache-directory          (concat my-cache-dir "url/")
      url-configuration-directory  (concat my-etc-dir "url/")
      gamegrid-user-score-file-directory (concat my-etc-dir "games/"))

;;
;;; Optimizations

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(defun my-prog-mode-hook ()
  "Relative number lines for program modes"
  (setq display-line-numbers 'relative))
(setq display-line-numbers 'relative)
(add-hook 'prog-mode-hook #'my-prog-mode-hook)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun chmodx ()
  (interactive)
  (chmod (buffer-file-name) "+x"))

(defun delete-this-file ()
  "Deletes current buffer from disk."
  (interactive)
  (delete-file (buffer-file-name)))

(use-package! helpful)
(use-package! general)

(use-package! gruvbox-theme
  :load-path "themes"
  :config (load-theme 'gruvbox t))

(require 'init-ivy)

(use-package! smartparens
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :init
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already, so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; But if someone does want overlays enabled, evil users will be stricken with
  ;; an off-by-one issue where smartparens assumes you're outside the pair when
  ;; you're really at the last character in insert mode. We must correct this
  ;; vile injustice.
  (setq sp-show-pair-from-inside t)
  ;; ...and stay highlighted until we've truly escaped the pair!
  (setq sp-cancel-autoskip-on-backward-movement nil)
  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for somoe modes), we halve it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 50)
  ;; This speeds up smartparens. No pair has any business being longer than 4
  ;; characters; if they must, the modes that need it set it buffer-locally.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)
  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)
  (smartparens-global-mode +1))

(use-package! paren
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (show-paren-mode +1))

;; (setq gc-cons-threshold most-positive-fixnum)
(use-package! flycheck :init (global-flycheck-mode))

(use-package! yasnippet)
;; (use-package! yasnippet-classic-snippets)
(use-package! auto-yasnippet)

(use-package dired
  :config
  (setq ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Instantly revert Dired buffers on re-visiting them, with no message.
        ;; (A message is shown if insta-revert is either disabled or determined
        ;; dynamically by setting this variable to a function.)
        dired-auto-revert-buffer t
        ;; Auto refresh dired, but be quiet about it
        dired-hide-details-hide-symlink-targets nil
        ;; files
        image-dired-dir (concat my-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

(use-package! diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil)
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil))

(use-package! diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))

(use-package! savehist
  ;; persist variables across sessions
  :config
  (setq savehist-file (concat my-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1)

  (add-hook 'kill-emacs-hook
    (defun my-unpropertize-kill-ring-h ()
      "Remove text properties from `kill-ring' for a smaller savehist file."
      (setq kill-ring (cl-loop for item in kill-ring
                               if (stringp item)
                               collect (substring-no-properties item)
                               else if item collect it)))))


(use-package! saveplace
  ;; persistent point location in buffers
  :config
  (setq save-place-file (concat my-cache-dir "saveplace")
        save-place-limit 100)

  (save-place-mode +1))

(use-package! helpful
  ;; a better *help* buffer
  :after apropos
  :commands helpful--read-symbol
  :init
  (define-key
    [remap describe-function] #'helpful-callable
    [remap describe-command]  #'helpful-command
    [remap describe-variable] #'helpful-variable
    [remap describe-key]      #'helpful-key
    [remap describe-symbol]   #'doom/describe-symbol)

  (defun my-use-helpful-a (orig-fn &rest args)
    "Force ORIG-FN to use helpful instead of the old describe-* commands."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable))
      (apply orig-fn args)))

  ;; patch apropos buttons to call helpful instead of help
  (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
    (button-type-put
     fun-bt 'action
     (lambda (button)
       (helpful-callable (button-get button 'apropos-symbol)))))
  (dolist (var-bt '(apropos-variable apropos-user-option))
    (button-type-put
     var-bt 'action
     (lambda (button)
       (helpful-variable (button-get button 'apropos-symbol))))))

(use-package! undo-tree
  ;; Branching & persistent undo
  :config
  (setq undo-tree-auto-save-history nil ; disable because unstable
        ;; undo-in-region is known to cause undo history corruption, which can
        ;; be very destructive! Disabling it deters the error, but does not fix
        ;; it entirely!
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist
        `(("." . ,(concat my-cache-dir "undo-tree-hist/"))))

  (global-undo-tree-mode +1))

(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :config
  (ws-butler-global-mode))

(use-package! rainbow-delimiters :config (rainbow-delimiters-mode))

(require 'init-magit)

(provide 'init-core)
;;; init-core.el ends here
