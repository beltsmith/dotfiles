;; Disable all the window chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

(setq user-emacs-directory my-local-dir)

(provide 'settings)
