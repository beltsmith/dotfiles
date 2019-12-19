;;; docker.el  -- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package! docker)

(use-package! dockerfile-mode
  :config
  (setq tab-width 4))

(provide 'docker)
;;; docker.el ends here
