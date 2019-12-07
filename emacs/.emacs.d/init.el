;;; init -- My EMACS init file
;;; Commentary:
;;; Code:

(defvar my-emacs-dir (file-name-directory load-file-name))
(add-to-list 'load-path (concat my-emacs-dir "/lisp"))

(require 'core)
(require 'evil-config)
(require 'keys)
(require 'init-org)
(require 'langs)
(require 'tools)
;; (require 'after)
