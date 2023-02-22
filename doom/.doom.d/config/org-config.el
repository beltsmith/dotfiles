;;; ../dotfiles/doom/.doom.d/config/org-config.el -*- lexical-binding: t; -*-


(require 'org-habit)
(setq org-modules '(ol-notmuch ol-bibtext ol-eww ol-gnus habits)
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-show-habits-only-for-today nil
      org-habit-show-all-today t
      org-habit-today-glyph ?‖
      org-habit-completed-glyph ?✓)

(setq org-log-into-drawer t
      org-log-done t
      org-agenda-files '("~/org" "~/org/roam"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-nightly"
      org-roam-graph-viewer "firefox-nightly")

(require 'org-roam-protocol)

(org-roam-db-autosync-mode)

(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
         (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
         (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                 ":END:"
                 "%?\n")                ;Place the cursor here finally
               "\n")))

 (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "Blog.org" "Posts")
                 (function org-hugo-new-subtree-post-capture-template)))

(provide 'org-config)
