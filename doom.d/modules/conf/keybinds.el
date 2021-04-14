;;; conf/keybinds.el -*- lexical-binding: t; -*-

(after! toggle-quotes
  (general-def :states 'normal
    (kbd "C-'") 'toggle-quotes))

(general-def :states 'normal
  [(control return)] 'evil-ex
  "\/"               'swiper
  (kbd "M-y") 'counsel-yank-pop)

(general-def :states 'normal :prefix "SPC"
  "sap"              '+ivy/project-search)

(general-def :states 'insert
  (kbd "s-i")        'yas-insert-snippet
  (kbd "C-v")        'yank
  (kbd "C-S-l")      'sp-slurp-hybrid-sexp
  (kbd "C-l")        'hippie-expand
  (kbd "C-'")        'toggle-quotes)

(general-def :states 'visual
  ",er" 'eval-region
  [(control return)] 'evil-ex
  (kbd "RET")        'align-regexp
  (kbd "C-g") 'evil-normal-stateq)

(general-def :states 'motion
  [(control return)] 'evil-ex
  (kbd "C-g") 'evil-normal-state)

(general-def [escape] 'evil-exit-emacs-state)

(general-def
  :states 'insert
  :keymaps 'org-mode-map
  [(meta control return)] 'org-insert-subheading)

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
  "M-j" 'move-line-down
  "M-k" 'move-line-up
  "M-C-y" 'browse-at-remote
  "M-Y" 'browse-at-remote-kill)

(defun evil-unimpaired-insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired-insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(general-def 'normal 'global
  "[ " 'evil-unimpaired-insert-space-above
  "] " 'evil-unimpaired-insert-space-below)
