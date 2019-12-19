;;; capture.el -- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; (defcapture "Work" "j" :datetree "~/org/work/journal.org" )
;;;
(defmacro defcapture (name ))

(defvar work-capture-template
  (string-join '("* %?"
                 ":PROPERTIES:"
                 ":RECORDED: %U"
                 ":END:"
                 "%i"
                 "%a")
               "\n")
  "Template to capture entry for work journal.")

(defvar global-todo-capture-template
  (string-join '("* TODO %?"
                 "%i"
                 "%a")
               "\n")
  "Template to capture entry for global todo.")


(setq org-capture-templates
      `(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks") ,global-todo-capture-template)
        ("w" "Work" entry (file+datetree "~/org/work/journal.org") ,work-capture-template)))

(general-def
  :states 'normal
  :prefix "SPC"
  "c" '(nil :which-key "Org Capture")
  "c c" 'org-capture
  "c l" 'org-capture-goto-last-stored)

(provide 'capture)
;;; capture.el ends here
