- [List](#sec-1)
  - [Package dependency management](#sec-1-1)
- [agirdler literatemacs configuration](#sec-2)
  - [Example configuration](#sec-2-1)
    - [Variable configuration](#sec-2-1-1)
    - [Bindings](#sec-2-1-2)
    - [Use package](#sec-2-1-3)
- [Base configuration](#sec-3)
  - [Dependencies](#sec-3-1)
    - [smex](#sec-3-1-1)
  - [Keyboard translates](#sec-3-2)
  - [Utility functions](#sec-3-3)
  - [Keybindings](#sec-3-4)
  - [Variables](#sec-3-5)
    - [URL Browser](#sec-3-5-1)
- [Ivy configuration](#sec-4)
  - [Use package](#sec-4-1)
  - [Variables](#sec-4-2)
- [Evil configuration](#sec-5)
  - [Functions](#sec-5-1)
  - [Bindings](#sec-5-2)
    - [Window bindings](#sec-5-2-1)
    - [Helper bindings](#sec-5-2-2)
- [Ruby](#sec-6)

# TODO List<a id="sec-1"></a>

## TODO Package dependency management<a id="sec-1-1"></a>

How to load evil-mc after evil and still configure it.

# agirdler literatemacs configuration<a id="sec-2"></a>

This is a literate programming style configuration file for emacs. Configuration code is seperated into different heading and organized according to language. Source blocks look like this

```emacs-lisp
(message "This is a source block")
```

Most of the source blocks you will see are "tangled" into files that are then loaded together as the configuration.

Tangling source blocks looks like as follows:

```org
#+BEGIN_SRC emacs-lisp :tangle repl.el
(read (eval (print (loop))))
#+END_SRC
```

## Example configuration<a id="sec-2-1"></a>

The structure I use for package configuration has a few steps.

### Variable configuration<a id="sec-2-1-1"></a>

The first step is to setup the variables we will be modifying.

```emacs-lisp
(defun my/evil-init ()
  (setq evil-shift-width 2
        evil-escape-key-sequence "jk")))
```

### Bindings<a id="sec-2-1-2"></a>

The next step is to define the keybindings we will be using. Since we are using \`use-package' we will be using conses to bind keys as opposed to \`bind-key'

```emacs-lisp
(defvar my/evil-bind
  (:map evil-normal-state map
   (("J" . evil-join-one-space))
   :map evil-insert-state-map
   (([C-v] . yank)
   ([C-l] . hippie-expand)
   ([C-S-l] . sp-slurp-hybrid-sexp))))
```

### Use package<a id="sec-2-1-3"></a>

The last step is to use \`use-package' with the hooks and bindings we setup before in order to ensure the packages are loaded and using the correct configuration.

```emacs-lisp
(use-package evil
  :bind (my/evil-bind)
  :init
  (my/evil-init)
  (use-package evil-mc))
```

# Base configuration<a id="sec-3"></a>

This section contains most of the base configuration code that is mode agnostic or globally affecting.

## Dependencies<a id="sec-3-1"></a>

### smex<a id="sec-3-1-1"></a>

```emacs-lisp
(use-package smex
    :defer t
    :init (setq-default smex-history-length 32
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items")))
```

## Keyboard translates<a id="sec-3-2"></a>

```emacs-lisp
(keyboard-translate ?\C-i ?\H-i)
```

## Utility functions<a id="sec-3-3"></a>

```emacs-lisp
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
  (insert "puts '#' * 90")
  (end-of-line)
  (newline-and-indent)
  (insert "puts caller")
  (end-of-line)
  (newline-and-indent)
  (insert "puts '#' * 90"))
```

## Keybindings<a id="sec-3-4"></a>

```

```

## Variables<a id="sec-3-5"></a>

### URL Browser<a id="sec-3-5-1"></a>

This is a fix for opening URLs on OS X using \`open' rather than creating a new browser instance

```emacs-lisp
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")
```

# Ivy configuration<a id="sec-4"></a>

I use ivy as a replacement for helm as the fuzzy matching behaviour is closer to what I prefer.

## Use package<a id="sec-4-1"></a>

```emacs-lisp
(use-package ivy-mode
  :init (setq ivy-use-virtual-buffers t
              ivy-re-builders-alist '((t . ivy--regex-plus))
              ivy-count-format "(%d/%d) "
              ivy-initial-inputs-alist nil))
```

## Variables<a id="sec-4-2"></a>

Configures ivy to be nicer to use by giving it a count formatter, having it use virtual buffers and default to not input.

```emacs-lisp
(setq ivy-use-virtual-buffers t
      ivy-re-builders-alist '((t . ivy--regex-plus))
      ivy-count-format "(%d/%d) "
      ivy-initial-inputs-alist nil)
```

# Evil configuration<a id="sec-5"></a>

"Emacs is a great environment lacking only a decent text editor"

## Functions<a id="sec-5-1"></a>

```emacs-lisp
(evil-define-operator evil-join-one-space (beg end)
  "Join the selected lines with one space between them."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (progn
        (end-of-line)
        (join-line 1)
        ;; (backward-char)
        (just-one-space)
        (backward-char)))))
```

## Bindings<a id="sec-5-2"></a>

Give evil more bindings so it is closer to MY vim

### Window bindings<a id="sec-5-2-1"></a>

```emacs-lisp
(define-key evil-normal-state-map (kbd "C-W C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-W C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-W C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-W C-k") 'evil-window-up)
```

### Helper bindings<a id="sec-5-2-2"></a>

```emacs-lisp
(define-key evil-normal-state-map (kbd "C-s") 'my-save)
```

# Ruby<a id="sec-6"></a>

```emacs-lisp
(use-package ruby-mode
:mode "\\.rb\\'"
:interpreter "ruby"
:bind
(
    (evil-define-key 'normal ruby-mode-map
      "gf" 'projectile-rails-goto-file-at-point ;; Goto file
      "gt" 'robe-jump                           ;; Goto tag

      "\\m" 'projectile-rails-find-model
      "\\c" 'projectile-rails-find-controller
      "\\s" 'projectile-rails-find-spec
      "\\v" 'projectile-rails-find-view
      "\\h" 'projectile-rails-find-helper
      "\\w" 'projectile-rails-find-worker
      "\\f" 'projectile-rails-find-factory

      "\\a" 'rails-alternate-files/body

      "\\t" 'projectile-toggle-between-implementation-and-test)
)
:config
(defun my-ruby-mode-hook ()
    (rbenv-use-corresponding)
    (rufo-minor-mode)

    (spacemacs/declare-prefix-for-mode 'ruby-mode "pg" "Projectile GOTO")

    (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
      "pgg" 'projectile-rails-goto-gemfile
      "pgr" 'projectile-rails-goto-routes
      "pgs" 'projectile-rails-goto-schema
      "pgh" 'projectile-rails-goto-spec-helper
      "ipd" 'insert-puts-debug)


    (setq enh-ruby-deep-indent-paren t
          enh-ruby-hanging-paren-deep-indent-level 0
          evil-shift-width 2
          rbenv-installation-dir "/usr/local/Cellar/rbenv/1.1.1"
          rspec-use-spring-when-possible t)
    (robe-mode 1)
    (ruby-refactor-mode-launch)
    (eldoc-mode 1)
    (yard-mode 1)

    (add-to-list 'write-contents-functions (lambda () (and (rbenv-use-corresponding) nil)))))
```
