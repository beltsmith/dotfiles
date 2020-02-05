;;; editor --- Config
;;; Commentary:
;;; Code:
(use-package! toggle-quotes)

(general-def 'normal 'global
  "C-'"        'toggle-quotes)

(general-def 'normal 'global
  "SPC s i" '(nil :which-key "String Inflect")
  "SPC s i r" 'string-inflection-ruby-style-cycle
  "SPC s i c" 'string-inflection-camelcase
  "SPC s i j" 'string-inflection-java-style-cycle
  "SPC s i p" 'string-inflection-python-style-cycle)


(provide 'editor)
;;; editor.el ends here
