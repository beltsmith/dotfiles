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

(use-package! ejc-sql
  :hook (ejc-sql-minor-mode . (lambda ()
                                (auto-complete-mode t)
                                (ejc-eldoc-setup)
                                (ejc-ac-setup))))

(defvar pg-jdbc-driver
  (concat "~/.m2/repository/org/postgresql/postgresql/42.2.10/"
          "postgresql-42.2.10.jar")
  "Location of postgres jdbc driver for ejc.")

;; (defmacro register-pg-connection )

(ejc-create-connection
 "flatbook-dev"
 :classpath pg-jdbc-driver
 :subprotocol "postgresql"
 :subname "//localhost:5432/flatbook_development"
 :user "beltsmith"
 :password "")

(ejc-create-connection
 "listings-dev"
 :classpath pg-jdbc-driver
 :subprotocol "postgresql"
 :subname "//localhost:5432/listings_development"
 :user "beltsmith"
 :password "")

(provide 'qls)
;;; qls.el ends here
