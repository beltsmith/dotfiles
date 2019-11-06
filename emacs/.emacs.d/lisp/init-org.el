(use-package! org
  :ensure org-plus-contrib
  :init
  (setq org-blank-before-new-entry '((heading . nil) (plan-list-item . auto)))
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (org-indent-mode 1))

(use-package! org-bullets :after org)
(use-package! evil-org :after (org evil))
(use-package! orgit :after org)
(use-package  org-tempo)

(defun org-insert-item-below ()
  "Insert an item below the current."
  (interactive)
  (move-end-of-line 1)
  (org-insert-item))

(defun org-insert-subheading-below ()
  "Insert a heading below current."
  (interactive)
  (move-end-of-line 1)
  (org-insert-subheading ""))


(defun org-insert-item-or-heading-below ()
  "Insert an item or heading below the current."
  (interactive)
  (cond ((org-at-heading-p) (org-insert-subheading-below))
	((org-at-item-p) (org-insert-item-below))))

(defun org-toggle-subtree-or-block ()
  "Toggle current subtree or block quote."
  (interactive)
  (cond ((org-in-src-block-p) (org-hide-block-toggle))
	((org-at-heading-p) (outline-toggle-children))))

;; ctrl enter
(general-def
  :keymaps 'org-mode-map
  :states '(normal insert emacs)
  "<C-return>" 'org-insert-item-or-heading-below
  "<tab>" 'org-toggle-subtree-or-block
  "<S-return>" 'org-insert-heading-after-current)


(provide 'init-org)
