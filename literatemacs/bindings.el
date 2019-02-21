(defvar my/evil-bindings
  :map evil-normal-state-map
  ("J" . evil-join-one-space)
  ([C-w C-h] . evil-window-left)
  ([C-w C-l] . evil-window-right)
  ([C-w C-j] . evil-window-down)
  ([C-w C-k] . evil-window-up))
