
(ql:quickload '(:cl-ppcre :yason))


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
  (loop for tk in (gethash "tokens" tb)
	when (gethash "meta" tk)
	  do (progn
	       (loop for k
		       being the hash-key
			 using (hash-value v) of (gethash "meta" tk)  
		     do (setf (gethash k tk) v))
	       (remhash "meta" tk))
	finally (return tb)))

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

(defun fix-obj (tb)
  (fix2 (fix1 tb)))

(defun text-from-tokens (tb)
  (with-output-to-string (s)
    (dolist (tk (gethash "tokens" tb))
      (format s "~a~a" (gethash "form" tk "") (if (gethash "form" tk) (gethash "sep" tk " ") "")))))

(defun read-jl-file (fn)
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (fix-obj (yason:parse line)))))


(defun check-obj (tb)
  (list (gethash "ofs" (gethash "meta" tb)) (gethash "text" tb) (text-from-tokens tb)))


(defun main ()
  (let ((ok '((:SUBSTITUTION #\; #\Space)
	      (:SUBSTITUTION #\( #\LEFT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\) #\;)
	      (:SUBSTITUTION #\" #\()
	      (:INSERTION NIL #\LEFT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\  #\()
	      (:SUBSTITUTION #\` #\ )
	      (:SUBSTITUTION #\" #\LEFT_SINGLE_QUOTATION_MARK)
	      (:INSERTION NIL #\LEFT_SINGLE_QUOTATION_MARK)
	      (:DELETION #\; NIL) (:DELETION #\` NIL)
	      (:DELETION #\' NIL)   
	      (:INSERTION NIL #\))
	      (:DELETION #\  NIL) (:DELETION #\( NIL) 
	      (:SUBSTITUTION #\) #\ ) (:DELETION #\" NIL)
	      (:SUBSTITUTION #\  #\LEFT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\' #\;)
	      (:SUBSTITUTION #\' #\ ) (:INSERTION NIL #\()
	      (:SUBSTITUTION #\: #\))
	      (:INSERTION NIL #\RIGHT_SINGLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\' #\RIGHT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\` #\LEFT_SINGLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\' #\RIGHT_SINGLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\" #\RIGHT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\" #\LEFT_DOUBLE_QUOTATION_MARK)
	      (:INSERTION NIL #\RIGHT_DOUBLE_QUOTATION_MARK)
	      (:INSERTION NIL #\ )
	      (:INSERTION NIL #\;) (:SUBSTITUTION #\" #\ ))))
    (loop for fn in (directory "data/*.jl")
	do (loop for an in (mapcar (lambda (r) 
				     (cons r
					   (remove-if (lambda (a) (equal :MATCH (car a)))
						      (edit-distance:diff (cadr r) (caddr r))))) 
				   (mapcar #'check-obj (read-jl-file fn)))
		 when (set-difference (cdr an) ok :test #'equal)
		   do (format t "~a~%~{ ~a~%~} ~a~%" fn (car an) (cdr an))))))



