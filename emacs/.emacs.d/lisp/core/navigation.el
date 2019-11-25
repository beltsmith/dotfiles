;;; navigation -- My navigation module
;;; Commentary:
;;;
;;; Code:

(use-package! dumb-jump
  :config
  (general-def 'normal "g d" 'dumb-jump-go))

(provide 'navigation)
;;; navigation.el ends here
