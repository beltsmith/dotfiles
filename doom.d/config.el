;;; ~/.doom.d/config.el

;; (defun my-prog-mode-hook ()
;;   "Relative number lines for program modes"
;;   (setq display-line-numbers 'relative))

;; (setq-default display-line-numbers 'relative)

;; (add-hook 'prog-mode-hook #'my-prog-mode-hook)

(remove-hook 'window-size-change-functions #'+doom-dashboard-resize-h)

(setq doom-font (font-spec :family "FuraMono Nerd Font Mono" :size 14)
      doom-theme 'doom-dracula)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Sudo shit
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; SQL
(set-popup-rule! "*SQL*" :ignore t)

;; lsp
(setq lsp-ui-doc-enable t
      lsp-enable-on-type-formatting t
      lsp-enable-indentation nil)

(defun buffer-first-line ()
  (interactive)
  (save-excursion
    (goto-line 0)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(platformio-mode arduino-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
