;;; ~/.doom.d/packages.el -*- lexical-binding: t; -*-

(package! toggle-quotes)
(package! evil-mc)
(package! prodigy)
(package! elfeed)
(package! elfeed-org)
(package! browse-at-remote)
(package! carbon-now-sh)
(package! rbenv)
(package! gruvbox-theme)
(package! prettier-js)
(package! eredis)
(package! docker-compose-mode)
(package! pkgbuild-mode)
(package! rufo)
;;(package! general)

(package! slack)
(after! slack
  (progn
    (setq slack-client-id "eb2e20e8-1567192935.406"
          slack-buffer-emojify t
          slack-prefer-current-team t)
    (slack-register-team)
    ))

(package! dumb-jump)
