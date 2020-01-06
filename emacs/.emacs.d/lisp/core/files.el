;;; files --- File functions
;;; Commentary:
;;; Code:
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun chmodx ()
  "Set current file eXecutable."
  (interactive)
  (let* ((current-mode (file-modes (buffer-file-name)))
	 (add-mode (logand ?\111 (default-file-modes)))
	 (final-mode (logior current-mode add-mode)))
    (set-file-modes (buffer-file-name) final-mode)))

(defun delete-this-file ()
  "Deletes current buffer from disk."
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer))

(defun rename-this-file ()
  "Renames file visited by current buffer and swap to it."
  (interactive)
  (let* ((old-buffer (buffer-name))
	 (current-file (buffer-file-name))
	 (new-file-name (read-file-name (format "Rename %s to: " current-file))))
    (rename-file current-file new-file-name)
    (find-file new-file-name)
    (kill-buffer old-buffer)))

(provide 'files)
;;; files.el ends here
