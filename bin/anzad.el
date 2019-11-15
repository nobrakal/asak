;; -*- lexical-binding: t -*-
(setq asak_db "")

(defun anzad ()
  "Call asak on the file of the current buffer."
  (interactive)
  (cursor-sensor-mode 1)
  (shell-command-to-string "dune build @check")
  (setq letbinds
	(butlast
	 (split-string
	  (shell-command-to-string
	   (format "anzad -e %s %s"
		   asak_db buffer-file-truename))
	  "#")))
  (dolist (letbind letbinds)
    (let* ((infos (split-string letbind ";"))
	   (start (1+ (string-to-number (nth 0 infos))))
	   (end   (1+ (string-to-number (nth 1 infos)))))
      (progn
       (put-text-property start end 'font-lock-face '(:foreground "red"))
       (put-text-property start end 'cursor-sensor-functions
			  (list (lambda (x y z)
				  (when (eq z 'entered) (message "%s" (nth 2 infos)))))))
      )
    )
  )
