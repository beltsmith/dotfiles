;;; yasnippet.el -- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'preamble)

(defvar my-snippets-dir (expand-file-name "snippets/" my-emacs-dir)
  "Directory containing my snippets.")

(defvar +private-snippets-dir (expand-file-name "snippets/" my-private-dir)
  "Directory where `yasnippet' will search for your private snippets.")

(use-package! yasnippet
  :after (smartparens evil)
  :commands (yas-minor-mode-on
	     yas-expand
	     yas-expand-snippet
	     yas-lookup-snippet
	     yas-insert-snippet
	     yas-new-snippet
	     yas-visit-snippet-file
	     yas-load-directory)
  :init
  ;; Ensure `yas-reload-all' is called as late as possible. Other modules could
  ;; have additional configuration for yasnippet. For example, file-templates.
  ;; (add-transient-hook 'yas-minor-mode-hook (yas-reload-all))
  (mapcar (lambda (hook) (add-hook hook #'yas-minor-mode-on))
	  '(text-mode-hook prog-mode-hook conf-mode-hook snippet-mode-hook))
  :config
  (setq yas-verbosity 0
	yas-also-auto-indent-first-line t
	yas-snippet-dirs '(my-snippets-dir +private-snippets-dir))
  ;; default snippets library, if available
  (require 'my-snippets nil t)

  ;; Remove GUI dropdown prompt (prefer ivy/helm)
  ;; (delq! 'yas-dropdown-prompt yas-prompt-functions)
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)

  ;; ;; Enable `read-only-mode' for built-in snippets (in `doom-local-dir')
  ;; (add-hook 'snippet-mode-hook #'+snippets-read-only-maybe-h)

  (defun +snippets-expand-on-region-a (orig-fn &optional no-condition)
    "Fix off-by-one issue with expanding snippets on an evil visual region, and
switches to insert mode.

If evil-local-mode isn't enabled, run ORIG-FN as is."
    (if (not (and (bound-and-true-p evil-local-mode)
		  (evil-visual-state-p)))
	(funcall orig-fn no-condition)
      (evil-visual-select evil-visual-beginning evil-visual-end 'inclusive)
      (cl-letf (((symbol-function 'region-beginning) (lambda () evil-visual-beginning))
		((symbol-function 'region-end)       (lambda () evil-visual-end)))
	(funcall orig-fn no-condition)))
    (when (and (bound-and-true-p evil-local-mode)
	       (yas-active-snippets))
      (evil-insert-state +1)))

  ;; (Evil only) fix off-by-one issue with line-wise visual selections in
  ;; `yas-insert-snippet', and switches to insert mode afterwards.
  (advice-add #'yas-insert-snippet :around #'+snippets-expand-on-region-a)
  )
(use-package! yasnippet-snippets)

(use-package! auto-yasnippet
  :defer t
  :init (setq aya-persist-snippets-dir (concat my-etc-dir "auto-snippets/"))
;;   :config
;;   (defadvice! +snippets--inhibit-yas-global-mode-a (orig-fn &rest args)
;;     "auto-yasnippet enables `yas-global-mode'. This is obnoxious for folks like
;; us who use yas-minor-mode and enable yasnippet more selectively. This advice
;; swaps `yas-global-mode' with `yas-minor-mode'."
;;     :around '(aya-expand aya-open-line)
;;     (cl-letf (((symbol-function #'yas-global-mode) #'yas-minor-mode)
;; 	      (yas-global-mode yas-minor-mode))
;;       (apply orig-fn args)))
  )

(defvar file-templates-default-trigger "__"
  "The default yasnippet trigger key (a string) for file template rules that don't
have a :trigger property in `+file-templates-alist'.")

(defvar file-templates-alist
  `(
    ;; core
    (emacs-lisp-mode :trigger "__initfile")
    (snippet-mode)
    ;; Org
    ("/README\\.org$"
     :when file-template-in-emacs-dirs-p
     :trigger "__my-readme"
     :mode org-mode)
    ("\\.org$" :trigger "__" :mode org-mode)
    )
  "Alist mapping conditions to templates.")

(defun file-template-in-emacs-dirs-p (file)
  "Return t if FILE is in my or your private directory."
  (or (file-in-directory-p file my-private-dir)
      (file-in-directory-p file my-emacs-dir)))

(defun empty-buffer-p ()
  "Any BUFFER where the cursor is on both the beginning and end is implicitly empty."
  (and (bobp) (eobp)))

(defun file-templates-check-h ()
  "Check if current buffer is applicable for template expansion.
Must be non-read-only, empty, and have corresponding rule in `templates-alist'."
  (when (and (not (file-exists-p (or (buffer-file-name) "")))
	     (not buffer-read-only)
	     (empty-buffer-p)
	     (not (string-match-p "^ *\\*" (buffer-name))))
    (when-let (rule (cl-find-if #'file-template-p file-templates-alist))
      (apply #'file-template--expand rule))))

(add-hook 'find-file-hook #'file-templates-check-h)

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

(defun file-templates-get-short-path ()
  "Fetches a short file path for the header in Doom module templates."
  (let ((path (file-truename (or buffer-file-name default-directory))))
    (save-match-data
      (cond ((string-match "/modules/\\(.+\\)$" path)
             (match-string 1 path))
            ((file-in-directory-p path my-emacs-dir)
             (file-relative-name path doom-emacs-dir))
            ((abbreviate-file-name path))))))

(defun file-template-p (rule)
  "Return t if given RULE apply to the current BUFFER."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (cond ((and (stringp pred) buffer-file-name)
                (string-match-p pred buffer-file-name))
               ((symbolp pred) (eq major-mode pred)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when) buffer-file-name))
         rule)))

(provide 'yasnippet)
;;; yasnippet.el ends here
