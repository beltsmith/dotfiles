;;; json.el -- My json configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Various configuartions I use when working with JSON
;;; Code:

(use-package! json-mode)
(use-package! jq-mode)

(general-def
  :states '(normal insert emacs)
  :keymaps 'json-mode-map
  :prefix "C-c"
  "C-f" 'json-pretty-print
  "C-j" 'jq-interactively)

(provide 'json)
;;; json.el ends here
