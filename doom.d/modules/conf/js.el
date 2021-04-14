;;; conf/js.el -*- lexical-binding: t; -*-


(setq js-indent-level 2)
(after! prettier-js (add-hook 'js-mode-hook 'prettier-js-mode))
