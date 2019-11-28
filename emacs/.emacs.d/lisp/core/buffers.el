;; Buffer keybinds

(general-def 'normal 'global
  :prefix "SPC b"
  "b" 'counsel-switch-buffer
  "k" 'kill-this-buffer
  "l" 'ivy-switch-buffer)

(provide 'buffers)
