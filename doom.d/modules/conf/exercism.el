;;; conf/exercism.el -*- lexical-binding: t; -*-

(defun exercism-cmd (cmd &rest args)
  "Run exercism CMD with ARGS."
  (shell-command (mapconcat 'identity (cons "exercism" (cons cmd args)) " ")))

(defun exercism-submit ()
  "Submit current file to exercism."
  (interactive)
  (exercism-cmd "submit" buffer-file-name))


;; TODO: Setup table of tracks and exercises
(defun exercism-download ()
  "Download exercise for a track."
  (interactive)
  (exercism-cmd "download"
                (concat "--exercise=" (read-string "Exercise: "))
                (concat "--track=" (read-string "Track: "))))
