
(use-package! gruvbox-theme :load-path "themes")

(use-package! doom-themes
  :load-path "themes"
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config)
  )

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

  (message "swapping dat bg")
  ;; Currently not swapping BG as doom-gruvbox doesn't need it
  ;; (solaire-mode-swap-bg)
  (solaire-global-mode +1))

(use-package! smart-mode-line
  :config
  (sml/setup)
  :init
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  )

(provide 'look-and-feel)
