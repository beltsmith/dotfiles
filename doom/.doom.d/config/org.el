;;; ../dotfiles/doom/.doom.d/config/org.el -*- lexical-binding: t; -*-


(require 'org-habit)
(setq org-modules '(ol-notmuch ol-bibtext ol-eww ol-gnus habits)
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-show-habits-only-for-today nil
      org-habit-show-all-today t
      org-habit-today-glyph ?‖
      org-habit-completed-glyph ?✓)
