;;; capture.el -- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; (defcapture "Work" "j" :datetree "~/org/work/journal.org" )
;;;
;; (defmacro defcapture (name ))

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

(defvar standup-capture-template
  (string-join '("* Yesterday"
                 "- %?"
                 "* Today"
                 "-"
                 "* Blockers"
                 "-")
               "\n")
  "Template to capture standup notes.")


(setq org-capture-templates
      `(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks") ,global-todo-capture-template)
        ("w" "Work" entry (file+datetree "~/org/work/journal.org") ,work-capture-template)
        ("s" "Standup" entry (file+datetree "~/org/work/standup.org") ,standup-capture-template)))

(general-def
  :states 'normal
  :prefix "SPC"
  "c" '(nil :which-key "Org Capture")
  "c c" 'org-capture
  "c l" 'org-capture-goto-last-stored)

(provide 'capture)
;;; capture.el ends here
