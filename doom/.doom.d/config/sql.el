;;; sql.el ---                                       -*- lexical-binding: t; -*-

;; (defun sql-beautify-region (beg end)
;;   "Beautify SQL in region between beg and END."
;;   (interactive "r")
;;   (save-excursion
;;     (shell-command-on-region beg end "/home/belt/gits/SqlBeautify/bin/beautify-sql" nil t)))

;; (defun sql-beautify-region (beg end)
;;   "Beautify SQL in region between beg and END."
;;   (interactive "r")
;;   (save-excursion
;;     (shell-command-on-region beg end "sqlformat -r --indent_after_first -a -s -k lower --comma_first true -" nil t)))

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "npx sql-formatter -c /home/belt/sql-formatter.json" nil t)))

;; ;; (defun sql-beautify-region (beg end)
;; ;;   "Beautify SQL in region between beg and END."
;; ;;   (interactive "r")
;; ;;   (save-excursion
;; ;;     (shell-command-on-region beg end "sqlfluff fix --dialect mysql -f -" nil t)))

(defun sql-beautify-buffer ()
 "Beautify SQL in buffer."
 (interactive)
 (sql-beautify-region (point-min) (point-max)))

(add-hook 'sql-mode-hook
          (lambda () (add-hook 'before-save-hook 'sql-beautify-buffer nil 'make-it-local)))


;; (use-package sqlformat
;;   :commands (sqlformat sqlformat-buffer sqlformat-region)
;;   :hook (sql-mode . sqlformat-on-save-mode)
;;   :init
;;   (setq sqlformat-command 'sqlformat
;;         sqlformat-args '("-s" "-k lower" "--comma_first true")))
