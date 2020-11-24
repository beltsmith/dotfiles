;;; ~/dotfiles/doomemacs/.doom.d/packages/toggle-quotes.el -*- lexical-binding: t; -*-

(package! toggle-quotes)

(evil-define-key 'normal 'global
  (kbd "C-'") 'toggle-quotes)

(evil-define-key 'insert 'global
  (kbd "C-'")        'toggle-quotes)

(provide 'packages/toggle-quotes)
