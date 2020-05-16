;; (use-package! company-irony)
;; (use-package! irony-eldoc)
;; (use-package! flycheck-irony)

;; (use-package! ccls
;;   :config (setq ccls-executable "/usr/bin/ccls")
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

(defun my-c-mode-hook ()
  "My c mode hook."
  (lsp)
  (setq gdb-many-windows t
        gdb-show-main t))

(defun my-buffer-line ()
  (format-mode-line "%l"))

(defun my-buffer-is-gdb (buffer)
  ""
  (let ((buff-name (substring (buffer-name buffer) 0 4)))
    (if buff-name
        (string= "*gud" buff-name))))

(defun my-get-gdb-buffer ()
  "Return gdb buffer."
  (seq-find 'my-buffer-is-gdb (buffer-list)))

(defun my-gdb-send (str)
  "Send STR to gdb buffer."
  (with-current-buffer (my-get-gdb-buffer)
    (insert str)
    (comint-send-input)))

(defun my-gdb-add-breakpoint ()
  "Add breakpoint at current line."
  (interactive)
  (my-gdb-send (concat "b " (my-buffer-line))))

(defun my-gdb-remove-breakpoint ()
  "Remove breakpoint at current line."
  (interactive)
  (my-gdb-send (concat "clear " (my-buffer-line))))

(defun my-gdb-quit ()
  "Quit gdb."
  (interactive)
  (my-gdb-send "quit")
  (kill-buffer (my-get-gdb-buffer)))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(provide 'ccode)
