(use-package smex
    :defer t
    :init
    (setq-default smex-history-length 32
                  smex-save-file (concat spacemacs-cache-directory ".smex-items")))

(keyboard-translate ?\C-i ?\H-i)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")
