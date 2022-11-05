
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


;; read json-lines files

(defun fix1 (tb)
  (loop for k
	  being the hash-key
	    using (hash-value v) of (gethash "meta" tb)  
	do (setf (gethash k tb) v)
	finally (remhash "meta" tb))
  (dolist (tk (gethash "tokens" tb) tb)
    (when (gethash "meta" tk)
      (loop for k
	      being the hash-key
		using (hash-value v) of (gethash "meta" tk)  
	    do (setf (gethash k tk) v)
	    finally (remhash "meta" tk)))))

(defun fix2 (tb)
  (labels ((wf (rend form sep)
	     (alexandria:alist-hash-table
	      `(("kind" . ("wf")) ("pos" . ,rend) ("type" . "punc") ("form" . ,form) ("tag" . "ignore") ("sep" . ,sep))
	      :test #'equal)))

    (let ((stack nil)
	  (res nil)
	  (topen  (alexandria:alist-hash-table '(("dq" . "“") ("sq" . "‘")) :test #'equal))
	  (tclose (alexandria:alist-hash-table '(("dq" . "”") ("sq" . "’")) :test #'equal)))
      (dolist (tk (gethash "tokens" tb)
		  (progn
		    (setf (gethash "tokens" tb)
			  (reverse res))
		    tb))
	(cond
	  ((and (equal "open"  (gethash "action" tk)) (equal "qf" (car (gethash "kind" tk))))
	   (let ((rend (gethash "rend" tk)))
	     (push rend stack)
	     (push (wf rend (gethash rend topen) "") res)))
	  
	  ((and (equal "close" (gethash "action" tk)) (equal "qf" (car (gethash "kind" tk))))
	   (let ((rend (pop stack)))
	     (push (wf rend (gethash rend tclose) " ") res)))
	  
	  (t (push tk res)))))))


(defun fix3 (obj wn)
  (let ((ofs (gethash "ofs" obj))
	(pos (gethash "pos" obj)))
    (cond
      ((and (not (gethash (format nil "~a-~a" ofs pos) wn))
	    (equal pos "a")
	    (gethash (format nil "~a-~a" ofs "s") wn))
       (setf (gethash "type" obj) "s"))

      (t (setf (gethash "type" obj)
	       (gethash "pos" obj))
	 (remhash "pos" obj)))
    obj))


(defun fix-obj (obj wn)
  (fix3 (fix2 (fix1 obj)) wn))


(defun read-jl-file (fn wn)
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (fix-obj (yason:parse line) wn))))


(defun text-from-tokens (tb)
  (with-output-to-string (s)
    (dolist (tk (gethash "tokens" tb))
      (format s "~a~a" (gethash "form" tk "") (if (gethash "form" tk) (gethash "sep" tk " ") "")))))


(defun describe-obj (obj wn)
  (let ((a (format nil "~a-~a"
		   (gethash "ofs" obj)
		   (gethash "type" obj))))
    (list :id a
	  :text-wn   (gethash a wn)
	  :text-meta (gethash "text" obj)
	  :text-toks (text-from-tokens obj))))


(defun main-1 ()
  (let ((wn (read-wordnet #P"~/work/wn/WordNet-3.0/dict/")))
    (dolist (fn (directory "data/*.jl"))
      (dolist (obj (read-jl-file fn wn))
	(let ((res (describe-obj obj wn)))
	  (when (> (edit-distance:distance (getf res :text-meta) (getf res :text-wn)) 0)
	    (format t "~a~% w:[~a]~% m:[~a]~%~%" (getf res :id) (getf res :text-wn) (getf res :text-meta))))))))

(defun main-2 ()
  (let ((wn (read-wordnet #P"~/work/wn/WordNet-3.0/dict/"))
	(ok '((:DELETION #\Space NIL)
	      (:DELETION #\; NIL)

	      (:SUBSTITUTION #\" #\LEFT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\" #\RIGHT_DOUBLE_QUOTATION_MARK)
	      
	      (:SUBSTITUTION #\` #\LEFT_SINGLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\' #\RIGHT_DOUBLE_QUOTATION_MARK)

	      (:INSERTION NIL #\;)
	      (:INSERTION NIL #\Space)

	      (:DELETION #\` NIL)
	      (:DELETION #\' NIL) )))
    
    (dolist (fn (directory "data/*.jl"))
      (dolist (obj (read-jl-file fn wn))
	(let* ((res (describe-obj obj wn))
	       (ops (remove-if (lambda (a) (equal :MATCH (car a))) (edit-distance:diff (getf res :text-meta) (getf res :text-toks)))))
	  (when (set-difference ops ok :test #'equal)
	    (format t "~a~% m:[~a]~% t:[~a]~% ~s~%~%" (getf res :id) (getf res :text-meta) (getf res :text-toks) ops)))))))



