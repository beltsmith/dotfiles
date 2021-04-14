;;; conf/org.el -*- lexical-binding: t; -*-

(defun org-insert-today ()
  (interactive)
  (org-insert-time-stamp (current-time)))

(evil-define-key '(normal insert) org-mode-map
  (kbd "C-c t") 'org-insert-today)

(general-def :states 'normal
  " Cl" 'org-capture-goto-last-stored)

(evil-define-key 'normal org-mode-map
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-h") 'org-metaleft
  (kbd "M-l") 'org-metaright)

(defun my/blog-file-by-date ()
  "Create an Org file with current date as name."
  (find-file (format-time-string "~/dev/blog/mustacheriders/_posts/%Y-%m-%d--%H-%M-%S.org")))

(setq org-log-into-drawer nil
      display-line-numbers-type 'visual
      org-capture-templates
      '(("b" "Blog" entry
         (file my/blog-file-by-date)
         "
#+TITLE: %(i)%^{LAYOUT}
#+STARTED: %T
#+LAYOUT: post
#+TAGS: jekyll org-mode belt")))

(require 'org-habit)
(setq org-modules '(ol-notmuch ol-bibtext ol-eww ol-gnus habits)
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-show-habits-only-for-today nil
      org-habit-show-all-today t
      org-habit-today-glyph ?‖
      org-habit-completed-glyph ?✓)
