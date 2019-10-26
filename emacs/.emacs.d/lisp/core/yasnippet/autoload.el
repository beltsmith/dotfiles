;;; core/yasnippet/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(cl-defun file-template--expand (pred &key project mode trigger ignore _when)
  "Auto insert a yasnippet snippet into current file and enter insert mode (if
evil is loaded and enabled)."
  (when (and pred (not ignore))
    (when (if project (my-project-p) t)
      (unless mode
        (setq mode (if (symbolp pred) pred major-mode)))
      (unless mode
        (user-error "Couldn't determine mode for %s file template" pred))
      (unless trigger
        (setq trigger templates-default-trigger))
      (if (functionp trigger)
          (funcall trigger)
        (require 'yasnippet)
        (unless yas-minor-mode
          (yas-minor-mode-on))
        (when (and yas-minor-mode
                   (when-let
                       (template (cl-find trigger (yas--all-templates (yas--get-snippet-tables mode))
                                          :key #'yas--template-key :test #'equal))
                     (yas-expand-snippet (yas--template-content template)))
                   (and (featurep 'evil) evil-local-mode)
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))
          (evil-initialize-state 'insert))))))

;;;###autoload
(defun file-templates-get-short-path ()
  "Fetches a short file path for the header in Doom module templates."
  (let ((path (file-truename (or buffer-file-name default-directory))))
    (save-match-data
      (cond ((string-match "/modules/\\(.+\\)$" path)
             (match-string 1 path))
            ((file-in-directory-p path my-emacs-dir)
             (file-relative-name path doom-emacs-dir))
            ((abbreviate-file-name path))))))

;;;###autoload
(defun file-template-p (rule)
  "Return t if given RULE applies to the current BUFFER."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (cond ((and (stringp pred) buffer-file-name)
                (string-match-p pred buffer-file-name))
               ((symbolp pred) (eq major-mode pred)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when) buffer-file-name))
         rule)))
