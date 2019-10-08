;;; init-core.el --- -*- lexical-binding: t; -*-

(use-package! magit
  :config
  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  ;; (setq transient-display-buffer-action '(display-buffer-below-selected)
  ;;       magit-display-buffer-function #'+magit-display-buffer-fn)

  ;; properly kill leftover magit buffers on quit
  ;; (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))

(use-package! forge
  :init
  (setq forge-database-file (concat my-etc-dir "forge/forge-database.sqlite")))
  ;; :config
  ;; (general-def :keymaps forge-topic-list-mode-map :states 'normal "q" #'kill-current-buffer)

(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil)
  ;; Warns that jT isn't bound. Well, yeah, you don't need to tell me, that was
  ;; on purpose ya goose.
  ;; (advice-add #'magit-todos-mode :around #'doom-shut-up-a)
  (magit-todos-mode +1))

(use-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package! evil-magit
  :after '(magit evil)
  :init
  (setq evil-magit-state 'normal
	evil-magit-use-z-for-folds t)
  :config
  ;; (unmap! magit-mode-map "M-1" "M-2" "M-3" "M-4") ; replaced by z1, z2, z3, etc
  (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
  (evil-define-key* '(normal visual) magit-mode-map
		    "zz" #'evil-scroll-line-to-center
		    "%"  #'magit-gitflow-popup)
  (general-define-key :states 'normal
		      :keymaps '(magit-status-mode-map
				 magit-stash-mode-map
				 magit-revision-mode-map
				 magit-diff-mode-map)
		      [tab] #'magit-section-toggle))

;; (use-package! git-rebase
;;   :after evil-magit
;;   :config
;;   (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
;;     (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
;;       (setcar desc (cdr key))))
;;   (evil-define-key* evil-magit-state git-rebase-mode-map
;; 		    "gj" #'git-rebase-move-line-down
;; 		    "gk" #'git-rebase-move-line-up))

(provide 'init-magit)
