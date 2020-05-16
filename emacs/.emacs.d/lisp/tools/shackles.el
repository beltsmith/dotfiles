(use-package! shackle
  :init

  (setq shackle-default-rule '(:align t :size 0.4)
        shackle-rules '((compilation-mode :align 'left :size 0.2)
                        (flycheck-mode :size 0.2)
                        (interactive-haskell-mode :align 'bottom :size 0.3)))

  )

(provide 'shackles)
