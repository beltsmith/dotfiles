;;; look-and-feel -- package
;;; Commentary:
;;; Provides that warm and fuzzy feeling
;;; Code:

(global-display-line-numbers-mode)
(setq-default display-line-numbers 'visual
              display-line-numbers-grow-only t)

(defmacro use-theme! (theme &rest plist)
  "Wrapper around use-package! to load THEME from themes dir."
  (declare (indent 1))
  `(use-package! ,theme :load-path "themes" ,@plist))

(defmacro use-theme (theme &rest plist)
  "Wrapper around use-package! to load THEME from themes dir."
  (declare (indent 1))
  `(use-package ,theme :load-path "themes" ,@plist))

;(use-theme darkplus-theme
;  :straight (darkplus-theme :host github :repo "dunstontc/darkpluc-emacs")
;  :config
;  (load-theme 'darkplus t))

(use-theme! gruvbox-theme)

(use-theme! color-theme-sanityinc-tomorrow)

;; (use-theme darkplus-theme
;;   :straight (:host github :repo "dunstontc/darkplus-emacs")
;;   :config (load-theme 'darkplus t))

(use-theme! doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package! solaire-mode
  :after doom-themes
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)

  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)

  ;; Because fringes can't be given a buffer-local face, they can look odd, so
  ;; we remove them in the minibuffer and which-key popups (they serve no
  ;; purpose there anyway).

  (add-hook 'solaire-mode-hook
	    (defun my-disable-fringes-in-minibuffer-h (&rest _)
	      "Disables fringes in mini-buffers"
	      (set-window-fringes (minibuffer-window) 0 0 nil)))

  ;; Currently not swapping BG as doom-gruvbox doesn't need it
  ;; (solaire-mode-swap-bg)
  (solaire-global-mode +1))

(use-package! smart-mode-line
  :config
  (sml/setup)
  :init
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful))

(use-package! rainbow-delimiters :config (rainbow-delimiters-mode +1))

(use-package! editorconfig)

(use-package! indent-guide :config (indent-guide-global-mode))

(provide 'look-and-feel)
