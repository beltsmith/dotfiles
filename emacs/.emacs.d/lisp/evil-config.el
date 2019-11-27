;; init-evil.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar evil-want-Y-yank-to-eol t "Make Y act like y$ rather than yy.")

(use-package! evil
  :init
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-integration t ;; This is optional since it's already set to t by default.
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
		evil-ex-search-vim-style-regexp t)
  :config
  (evil-mode 1)

  (general-def 'normal 'override
    :prefix "SPC"
    "" 'nil ;; Needed to bind as prefix in some modes(e.g. dired)
    "w" 'evil-window-map)

  (general-def 'normal 'global
    [(control return)] 'evil-ex
    "/"          'swiper
    "C-'"        'toggle-quotes
    "SPC C l"    'org-capture-goto-last-stored
    "M-y"        'counsel-yank-pop)

  (general-def 'insert 'global
    "s-i"        'yas-insert-snippet
    "C-v"        'yank
    "C-S-l"      'sp-slurp-hybrid-sexp
    "C-l"        'hippie-expand
    "C-'"        'toggle-quotes)

  (general-def 'visual 'global
    :prefix ","
    "e" '(nil :message "Eval")
    "e r" 'eval-region)

  (general-def 'visual 'global
    :prefix "SPC"
    "e" '(nil :message "Eval")
    "e r" 'eval-region)

  (defun evil-unimpaired-insert-space-above (count)
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

  (defun evil-unimpaired-insert-space-below (count)
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

  (general-def 'normal 'global
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

  (general-def 'normal 'global
    (kbd "M-j") 'move-line-down
    (kbd "M-k") 'move-line-up)

  (general-def 'normal 'org-mode-map
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-h") 'org-metaleft
    (kbd "M-l") 'org-metaright)

  (general-def 'normal 'global
    (kbd "M-C-y") 'browse-at-remote
    (kbd "M-Y") 'browse-at-remote-kill)

  ;; Set no highlight on ESC
  (advice-add 'evil-force-normal-state :after #'evil-ex-nohighlight))

(use-package! evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package! evil-commentary
  :config (evil-commentary-mode 1))

(use-package! evil-surround
  :commands (global-evil-surround-mode
	     evil-surround-edit
	     evil-Surround-edit
	     evil-surround-region)
  :config (global-evil-surround-mode 1)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

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

(provide 'evil-config)

;;; evil-config.el ends here