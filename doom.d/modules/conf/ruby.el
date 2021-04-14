;;; conf/ruby.el -*- lexical-binding: t; -*-

;; (defun my-ruby-mode-hook ()
;;   (setq flycheck-command-wrapper-function (lambda (command) (append '("bundle" "exec") command))))
;; (add-hook 'ruby-mode-hook #'my-ruby-mode-hook)

(evil-define-key 'normal ruby-mode-map
  "gt" 'robe-jump)
(evil-define-key 'normal enh-ruby-mode-map
  "gt" 'robe-jump)

(defun my/ruby-insert-frozen-string-magic-comment ()
  "Insert frozen string literal magic comment at the top of the buffer if it does not exist."
  (interactive)
  (let ((frozen-string-magic-comment "# frozen_string_literal: true")
        (first-line (buffer-first-line)))
    (pcase first-line
      ((pred (string= frozen-string-magic-comment)) nil)
      (_ (save-excursion
           (goto-line 0)
           (insert frozen-string-magic-comment "\n\n"))))))

;; (add-hook 'ruby-mode 'rufo-minor-mode)
(setq rufo-minor-mode-use-bundler t)
(setq-hook! 'ruby-mode-hook +format-with-lsp nil)
(setq-hook! 'ruby-mode-hook +format-with 'rufo)

(after! dap-mode
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltips))
  (dap-auto-configure-mode 1)

  ;; enables mouse hover support
  ;; (dap-tooltip-mode 1)
  (require 'dap-ruby)
  (setq dap-ruby-debug-program `("/home/belt/.asdf/shims/node"
                                    ,(f-join dap-ruby-debug-path "extension/dist/debugger/main.js")))

  (defun my-dap-ruby--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (plist-put :dap-server-path dap-ruby-debug-program)
        (dap--put-if-absent :cwd (lsp-workspace-root))
        (dap--put-if-absent :program (concat (lsp-workspace-root) "/bin/rails"))
        (dap--put-if-absent :type "Rails")
        (dap--put-if-absent :name "Rails")))
  (dap-register-debug-provider "Rails" 'my-dap-ruby--populate-start-file-args)

  (dap-register-debug-template
   "Prevail::Rails::Server"
   (list :type "Rails"
         :request "launch"
         :args "s"
         :cwd "/home/belt/dev/prevail"
         :program "/home/belt/dev/prevail/bin/rails"
         :name "Rails Server Debug")))
