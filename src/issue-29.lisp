
(ql:quickload '(:cl-ppcre :yason :edit-distance))


(defun fix (obj)
  (if (and (gethash "pos" obj) (gethash "type" obj))
      (remhash "pos" obj))
  obj)

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



