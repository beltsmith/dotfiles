(defmacro add-hook! (hooks &rest functions)
  "Convenient macro to add FUNCTIONS to HOOKS."
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point)))))
  (let* ((hook-forms (hook-forms-for hooks))
	 (func-forms (func-forms-for functions))
	 (forms (hooks--forms-for hook-forms func-forms)))
    (macroexp-progn forms)))

(defun hook-forms-for (hooks)
  "Convert HOOKS into HOOK symbols.
i.e. 'emacs-lisp, 'emacs-lisp-mode, 'emacs-lisp-mode-hook all convert to 'emacs-lisp-mode-hook."
  (declare (pure t) (side-effect-free t))
  (mapc (lambda (hook) ) hooks))

(defun hookish-to-hook (hookish)
  "Convert HOOKISH into a HOOK symbol."
  )

(defun add-hooks! (hooks &rest functions)
  "Convenience method to allow adding FUNCTIONS to HOOKS."
  (when hooks
    (cond ((listp hooks)
	   (let ((hook (car hooks))
		 (rest (cdr hooks)))
	     (add-hook! hook functions)
	     (add-hooks! rest functions)))
	  ((symbolp hooks)
	   (add-hook! hooks functions)))))

(defun add-hook! (hook &rest functions)
  "Add multiple FUNCTIONS to a single HOOK."
  (dolist (fun functions)
    (add-hook hook #'fun)))

(defun my-prog-mode-hook ()
  "Relative number lines for program modes."
  (setq display-line-numbers 'relative))
(setq-default display-line-numbers 'relative)
(add-hook 'prog-mode-hook #'my-prog-mode-hook)

(provide 'hooks)
