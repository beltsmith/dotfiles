;;; init-evil.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package! evil
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t ;; like vim's 'splitright'
        evil-split-window-below t ;; like vim's 'splitbelow'
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-visual-char-semi-exclusive t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        )
  :config
  (evil-mode 1)
  (defun translate-keys (&rest keys)
    (let* ((translation (car keys))
	   (from (car translation))
	   (to (cdr translation))
	   (rest (cdr keys)))
      (define-key key-translation-map (kbd from) (kbd to))
      (unless (null rest) (translate-keys rest))))
  (define-key key-translation-map
    (kbd "SPC w") (kbd "C-w"))
  (evil-define-key 'normal 'global
    [(control return)] 'evil-ex
    (kbd "/")          'swiper
    (kbd "C-'")        'toggle-quotes
    " Cl"              'org-capture-goto-last-stored
    (kbd "M-y")        'counsel-yank-pop

    (kbd "SPC b b") 'counsel-switch-buffer
    (kbd "SPC b k") 'kill-this-buffer)
  (evil-define-key 'insert 'global
    (kbd "s-i")        'yas-insert-snippet
    (kbd "C-v")        'yank
    (kbd "C-S-l")      'sp-slurp-hybrid-sexp
    (kbd "C-l")        'hippie-expand
    (kbd "C-'")        'toggle-quotes)
  (evil-define-key 'visual 'global
    ",er" 'eval-region)
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

  (evil-define-key 'normal 'global
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
  )

(use-package! evil-collection
  :config
  (evil-collection-init))

(use-package! evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-yank-line
             evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package! evil-anzu)

(use-package! evil-easymotion
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))
(provide 'init-evil)

;;; init-evil.el ends here
