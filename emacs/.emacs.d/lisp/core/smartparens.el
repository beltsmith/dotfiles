(use-package! smartparens
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :init
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already, so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; But if someone does want overlays enabled, evil users will be stricken with
  ;; an off-by-one issue where smartparens assumes you're outside the pair when
  ;; you're really at the last character in insert mode. We must correct this
  ;; vile injustice.
  (setq sp-show-pair-from-inside t)
  ;; ...and stay highlighted until we've truly escaped the pair!
  (setq sp-cancel-autoskip-on-backward-movement nil)
  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for somoe modes), we halve it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 50)
  ;; This speeds up smartparens. No pair has any business being longer than 4
  ;; characters; if they must, the modes that need it set it buffer-locally.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)
  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)
  (smartparens-global-mode +1))

(use-package! paren
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (show-paren-mode +1))

(provide 'smartparens)
