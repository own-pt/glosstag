
(ql:quickload '(:cl-ppcre :yason :edit-distance))


;; read WordNet DB Files

(defun proc-db-line (str dict)
  (destructuring-bind (data gloss)
      (cl-ppcre:split "\\|" str)
    (let* ((reg (cl-ppcre:split " " data))
	   (ssi (format nil "~a-~a" (car reg) (caddr reg))))
      (setf (gethash ssi dict)
	    (string-trim '(#\Space) gloss)))))

(defun read-wordnet (dict-folder)
  (let ((dict (make-hash-table :test #'equal))
	(files '("noun" "adv" "adj" "verb")))
    (dolist (fn files dict)
      (with-open-file (in (make-pathname :name "data" :type fn :defaults dict-folder))
	(loop for line = (read-line in nil nil)
	      while line
	      when (cl-ppcre:scan "^[0-9]" line)
		do (proc-db-line line dict))))))


(defun read-jl-file (fn)
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (yason:parse line))))


(defun text-from-tokens (obj)
  (with-output-to-string (s)
    (dolist (tk (gethash "tokens" obj))
      (format s "~a~a" (gethash "form" tk "") (if (gethash "form" tk) (gethash "sep" tk " ") "")))))


(defun describe-obj (obj wn)
  (let ((a (format nil "~a-~a"
		   (gethash "ofs" obj)
		   (gethash "type" obj))))
    (list :id a
	  :text-wn   (gethash a wn)
	  :text-meta (gethash "text" obj)
	  :text-toks (text-from-tokens obj))))


(defun write-jl-file (objs stream)
  (loop for o in objs
	do (yason:encode o stream)
	do (format stream "~%")))

(defun main-0 ()
  (let ((wn (read-wordnet #P"~/work/wn/WordNet-3.0/dict/")))
    (dolist (fn (directory "data/annotation-*.jl"))
      (dolist (obj (read-jl-file fn))
	(let ((res (describe-obj obj wn)))
	  (when (not (equal (getf res :text-meta) (getf res :text-toks)))
	    (format t "~a~% txt:~a~% tks:~a~%~%" (getf res :id) (getf res :text-meta) (getf res :text-toks))))))))


(defun fix-sep (obj)
  (dolist (tk (gethash "tokens" obj) obj)
    (when (and (gethash "sep" tk) (equal " " (gethash "sep" tk)))
      (remhash "sep" tk))))

(defun main-1 ()
  (dolist (fn (directory "data/annotation-*.jl"))
    (with-open-file (out (make-pathname :type "new" :defaults fn) :direction :output :if-exists :supersede)
      (write-jl-file (mapcar #'fix-sep (read-jl-file fn)) out))))

