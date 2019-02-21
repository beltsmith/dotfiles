(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))

(defun my-save ()
  (interactive)
  (if (buffer-modified-p)
      (save-buffer)
    (touch)))

(defun insert-puts-debug ()
  "Insert puts debug string"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (insert "puts '#' * 90,")
  (end-of-line)
  (newline-and-indent)
  (insert "     caller,")
  (end-of-line)
  (newline-and-indent)
  (insert "     '#' * 90"))
