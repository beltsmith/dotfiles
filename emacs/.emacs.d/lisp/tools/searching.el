(use-package! ripgrep
  :init
  (general-define-key "C-c s p" 'ripgrep-regexp))

(provide 'searching)
