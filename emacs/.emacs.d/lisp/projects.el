;;; init-projects.el  -*- lexical-binding: t; -*-
(defvar doom-projectile-cache-limit 25000
  "If any project cache surpasses this many files it is purged when quitting
Emacs.")

(defvar doom-projectile-cache-blacklist '("~" "/tmp" "/")
  "Directories that should never be cached.")

(defvar doom-projectile-cache-purge-non-projects nil
  "If non-nil, non-projects are purged from the cache on `kill-emacs-hook'.")

(defvar my-projectile-fd-binary
  (or (cl-find-if #'executable-find '("fd" "fdfind"))
      "fd")
  "name of `fd-find' executable binary")

(use-package! projectile
  :after ivy
  :init
  (setq projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-known-projects-file (concat my-cache-dir "projectile.projects")
        projectile-require-project-root t
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-files-cache-expire 604800 ; expire after a week
        projectile-sort-order 'recentf
        projectile-use-git-grep t ; use git-grep for text searches
        projectile-completion-system 'ivy
	projectile-git-command (concat
				my-projectile-fd-binary
				" . --color=never --type f -0 -H -E .git")
	projectile-generic-command projectile-git-command
	projectile-indexing-method 'alien)

  :config
  (projectile-mode +1)

  ;; a more generic project root file
  (push ".project" projectile-project-root-files-bottom-up)
  (push (abbreviate-file-name my-local-dir) projectile-globally-ignored-directories)

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +{ivy,helm}/project-search or +{ivy,helm}/ag instead")
  (put 'projectile-ripgrep 'disabled "Use +{ivy,helm}/project-search or +{ivy,helm}/rg instead")

  ;; Treat current directory in dired as a "file in a project" and track it
  ;;(add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (add-hook 'kill-emacs-hook
    (defun my-cleanup-project-cache-h ()
      "Purge projectile cache entries that:

a) have too many files (see `my-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private. (see `my-projectile-cache-blacklist'),
c) are not valid projectile projects."
      (when (and (bound-and-true-p projectile-projects-cache)
                 my-interactive-mode)
        (cl-loop with blacklist = (mapcar #'file-truename my-projectile-cache-blacklist)
                 for proot in (hash-table-keys projectile-projects-cache)
                 if (or (not (stringp proot))
                        (>= (length (gethash proot projectile-projects-cache))
                            my-projectile-cache-limit)
                        (member (substring proot 0 -1) blacklist)
                        (and my-projectile-cache-purge-non-projects
                             (not (my-project-p proot))))
                 do (remhash proot projectile-projects-cache)
                 and do (remhash proot projectile-projects-cache-time)
                 and do (remhash proot projectile-project-type-cache))
        (projectile-serialize-cache)))))

(use-package! counsel-projectile
  :after (evil ivy)
  :config
  (evil-define-key 'normal 'global
    (kbd "C-SPC")   'counsel-projectile
    " sap"          'counsel-projectile-rg
    (kbd "SPC SPC") 'projectile-find-file))

(use-package! persp-mode)
(use-package! persp-projectile)

;; setup up snippets/templates

(provide 'projects)
