(in-package :next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

(define-key "C-y" 'copy)

(define-key :scheme :vi-normal
  "yy" 'copy)
