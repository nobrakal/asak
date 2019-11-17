;;; anzad.el --- asak caller                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 IRIF / OCaml Software Foundation.

;; Author: Alexandre Moine <alexandre@moine.me>
;; Keywords: ocaml redundancy
;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This simple package allows to call asak on the current buffer.

;; The variable asak_db must be set to a database made with "utils/build_assoc.ml"

;;; Code:

(defgroup anzad nil
  "Tools for running anzad."
  :group 'tools)

(defcustom asak_db ""
  "The path to the asak database."
  :type 'string
  :group 'anzad)

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

(provide 'anzad)
;;; anzad.el ends here
