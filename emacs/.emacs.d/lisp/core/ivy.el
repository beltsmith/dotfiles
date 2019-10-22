;;; Ivy
(use-package! ivy
  :after general
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          ;; Ignore order for non-fuzzy searches by default
          (t . ivy--regex-ignore-order)))
  (general-emacs-define-key ivy-minibuffer-map
    "C-l" #'ivy-alt-done
    "<C-return>" #'ivy-immediate-done)
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)
  (ivy-mode 1))
(use-package! swiper :after ivy)
(use-package! counsel :after ivy)

(use-package! amx :config (amx-mode))

(use-package! ivy-hydra
  :after (ivy hydra)
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body)
  :init
  (general-define-key ivy-minibuffer-map
    "C-o" #'ivy-dispatching-done-hydra
    "M-o" #'hydra-ivy/body)
  :config
  ;; ivy-hydra rebinds this, so we have to do so again
  (define-key ivy-minibuffer-map (kbd "M-o") #'hydra-ivy/body))

(use-package! ivy-posframe
  :requires ivy
  :config
  (ivy-posframe-mode)
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 5
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height))
        ivy-posframe-style 'frame-center
        ivy-posframe-display-functions-alist
        '((swiper . ivy-posframe-display-at-point)
          (t . ivy-posframe-display)))

  (dolist (fn '(swiper counsel-ag counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback)))

(use-package! ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :init
  (setq ivy-prescient-enable-filtering nil  ; we do this ourselves
        ivy-prescient-retain-classic-highlighting t
        ivy-initial-inputs-alist nil)

  :config
  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat my-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

;; Lifted from doom
(use-package! all-the-icons-ivy
  :after ivy
  :config
  ;; `all-the-icons-ivy' is incompatible with ivy-rich's switch-buffer
  ;; modifications, so we disable them and merge them ourselves
  (setq all-the-icons-ivy-buffer-commands nil)

  (all-the-icons-ivy-setup))

(use-package! avy
  :config (global-set-key (kbd "C-:") 'avy-goto-char))

(provide 'init-ivy)
