
(ql:quickload '(:cl-ppcre :yason :edit-distance :serapeum))

(defun fix (obj)
  (loop for tk in (gethash "tokens" obj)
	when (equal (gethash "type" tk) "punc")
	  do (progn
	       (setf (gethash "pos" tk)
		     "punc")
	       (remhash "type" tk))
	finally (return obj)))

(defun read-jl-file (fn)
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (fix (yason:parse line)))))

(defun write-jl-file (objs stream)
  (loop for o in objs
	do (yason:encode o stream)
	do (format stream "~%")))

(defun main-0 ()
  (dolist (fn (directory "data/annotation-*.jl"))
    (with-open-file (out (make-pathname :type "new" :defaults fn) :direction :output :if-exists :supersede)
      (write-jl-file (read-jl-file fn) out))))


(defun text-from-tokens (obj)
  (with-output-to-string (s)
    (dolist (tk (gethash "tokens" obj))
      (format s "~a~a" (gethash "form" tk "") (if (gethash "form" tk) (gethash "sep" tk " ") "")))))

(defun main-1 ()
  (dolist (fn (directory "data/annotation-??.jl"))
    (dolist (obj (read-jl-file fn))
      (when (not (equal (gethash "text" obj) (text-from-tokens obj)))
	(print obj)))))

