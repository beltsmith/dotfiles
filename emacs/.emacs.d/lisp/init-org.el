(use-package! org
  :ensure org-plus-contrib
  :pin org
  :init
  (setq org-blank-before-new-entry '((heading . nil) (plan-list-item . auto))
        org-return-follows-link t)
  :config
  (add-hook 'org-mode-hook 'org-indent-mode))

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

(defun org-move-item-or-heading-down ()
  "Move an item or heading down."
  (interactive)
  (cond ((org-at-heading-p) (org-move-subtree-down))
        ((org-at-item-p) (org-move-item-down))))

(defun org-move-item-or-heading-up ()
  "Move an item or heading up."
  (interactive)
  (cond ((org-at-heading-p) (org-move-subtree-up))
        ((org-at-item-p) (org-move-item-up))))

(defun org-toggle-subtree-or-block ()
  "Toggle current subtree or block quote."
  (interactive)
  (cond ((org-at-heading-p) (outline-toggle-children))
        (t (org-cycle))))

;; ctrl enter
(general-def
  :keymaps 'org-mode-map
  :states 'normal
  "<C-return>" 'org-insert-item-or-heading-below
  "<tab>" 'org-toggle-subtree-or-block
  "<S-return>" 'org-insert-heading-after-current
  "M-h" 'org-do-promote
  "M-l" 'org-do-demote
  "M-L" 'org-demote-subtree
  "M-H" 'org-promote-subtree
  "M-j" 'org-move-item-or-heading-down
  "M-k" 'org-move-item-or-heading-up)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defun org-insert-today ()
  "Insert a timestamp of today."
  (interactive)
  (org-insert-time-stamp (current-time)))

(defvar my-org-config-dir (concat my-lisp-dir "/org"))
(add-to-list 'load-path my-org-config-dir)


(require 'babel)
(require 'capture)

(provide 'init-org)
;; init-org.el ends here
