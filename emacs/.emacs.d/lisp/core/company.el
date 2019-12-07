(use-package! company
  :config
  (global-company-mode +1)
  :init
  (setq company-dabbrev-downcase 0
	company-idle-delay 0))

(use-package! company-dict :requires company)

(use-package! company-prescient
  :requires company
  :hook (company-mode . company-prescient-mode))

(provide 'company)
