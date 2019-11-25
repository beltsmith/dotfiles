;; (defmacro add-hook! (hooks &rest functions)
;;   "Convenient macro to add FUNCTIONS to HOOKS."
;;   (declare (indent (lambda (indent-point state)
;;                      (goto-char indent-point)
;;                      (when (looking-at-p "\\s-*(")
;;                        (lisp-indent-defform state indent-point)))))
;;   (macroexp-progn
;;    (hooks--forms-for (hook-forms-for hooks) functions)))

;; (defun hooks--forms-for (hook-forms functions)
;;   "Build forms for each permutation of HOOK-FORMS and FUNCTIONS."
;;   (-flatten-n 1
;;    (mapcar (lambda (hook-form)
;; 	     (mapcar (lambda (func)
;; 		       `(add-hook ',hook-form ',func))
;; 		     functions))
;; 	   hook-forms)))

;; (defun hookish-to-hook (hookish)
;;   "Convert HOOKISH into a HOOK symbol."
;;   (let* ((hook (symbol-name hookish))
;; 	 (end (car-safe (last (split-string hook "-"))))
;; 	 (hook-name (cond ((string= "hook" end) hook)
;; 			  ((string= "mode" end) (concat (symbol-name hookish) "-hook"))
;; 			  (t (concat (symbol-name hookish) "-mode-hook")))))
;;     (intern hook-name)))

;; (defun hook-forms-for (hooks)
;;   "Convert HOOKS into HOOK symbols.
;; i.e. 'emacs-lisp, 'emacs-lisp-mode, 'emacs-lisp-mode-hook all convert to 'emacs-lisp-mode-hook."
;;   (declare (pure t) (side-effect-free t))
;;   (mapcar 'hookish-to-hook hooks))

;; (defun add-hooks! (hooks &rest functions)
;;   "Convenience method to allow adding FUNCTIONS to HOOKS."
;;   (when hooks
;;     (cond ((listp hooks)
;; 	   (let ((hook (car hooks))
;; 		 (rest (cdr hooks)))
;; 	     (add-hook! hook functions)
;; 	     (add-hooks! rest functions)))
;; 	  ((symbolp hooks)
;; 	   (add-hook! hooks functions)))))

;; (defun add-hook! (hook &rest functions)
;;   "Add multiple FUNCTIONS to a single HOOK."
;;   (dolist (fun functions)
;;     (add-hook hook #'fun)))

;; (defun my-prog-mode-hook ()
;;   "Relative number lines for program modes."
;;   (setq display-line-numbers 'relative))
;; (setq-default display-line-numbers 'relative)
;; (add-hook 'prog-mode-hook #'my-prog-mode-hook)


(provide 'hooks)
