;;; sql.el --- My sqlish tools config
;;; Commentary:
;;; Code:
;; cassandra

(use-package! cql-mode)

;; sql

(use-package! sqlformat
  :hook (sql-mode . sql-format-on-save-mode)
  ;; :config
  ;; (setq sqlformat-command "pg_format")
  )

(provide 'qls)
;;; qls.el ends here
