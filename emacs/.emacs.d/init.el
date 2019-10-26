;;; ~/.emacs.d/init.el

(defvar my-emacs-dir (file-name-directory load-file-name))
(add-to-list 'load-path (concat my-emacs-dir "/lisp"))

(require 'preamble)
(require 'core)
(require 'evil-config)
(require 'keys)
(require 'init-org)
(require 'langs)
(require 'tools)
