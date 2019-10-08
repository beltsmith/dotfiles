;;; ~/.emacs.d/init.el

(defvar my-emacs-dir (file-name-directory load-file-name))
(add-to-list 'load-path (concat my-emacs-dir "/lisp"))

(require 'init-preamble)
(require 'init-core)
(require 'init-evil)
(require 'init-keys)
(require 'init-langs)
(require 'init-finders)
