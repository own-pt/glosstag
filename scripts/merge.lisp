
(in-package :glosstag.merge)

(defmacro with-open-files (args &body body)
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
	  (with-open-files
	      ,(rest args) ,@body)))))



(defun combine (ss-i ss-a)
  (setf (gethash "meta" ss-a) (gethash "meta" ss-i))
  (let ((globs-a (remove-if-not (lambda (tk)
				  (and (equal "glob" (car (gethash "kind" tk)))
				       (equal "man" (gethash "tag" tk))
				       (null (gethash "senses" tk))))
				(gethash "tokens" ss-a)))
	(globs-i (remove-if-not (lambda (tk)
				  (equal "glob" (car (gethash "kind" tk))))
				(gethash "tokens" ss-i))))
    (if (not (null globs-a)) (warn "1: ~a" (mapcar #'alexandria:hash-table-plist globs-a)))
    (dolist (g globs-a)
      (let ((gi (find (cadr (gethash "kind" g)) globs-i
		      :test (lambda (coll tk)
			      (equal coll (cadr (gethash "kind" tk)))))))
	  (setf (gethash "senses" g)
		(gethash "senses" gi))))
    (if (not (null globs-a)) (warn "2: ~a" (mapcar #'alexandria:hash-table-plist globs-a)))
    ss-a))


(defun merge-files (file1 file2 file-out)
  (with-open-files ((in1 file1)
		    (in2 file2)
		    (out file-out :direction :output :if-exists :supersede))
    (loop for line-1 = (read-line in1 nil nil)
	  for line-2 = (read-line in2 nil nil)
	  while (and line-1 line-2)
	  do (let ((ss-1 (parse line-1))
		   (ss-2 (parse line-2)))
	       (assert (equal (gethash "_id" ss-1) (gethash "_id" ss-2)))
	       (yason:encode (combine ss-1 ss-2) out)
	       (fresh-line out)))))




(defun senses (dict-file index-file outfile)
  (let ((data (make-hash-table :test #'equal)))
    (with-open-files ((din dict-file)
		      (iin index-file)
		      (out outfile :direction :output :if-exists :supersede))
	    (loop for line = (read-line din nil nil)
		  while line
		  do (let ((entry (parse line)))
		       (mapc (lambda (k)
			      (if (gethash k data)
				  (warn "Key already loaded: ~a" k)
				  (push 'd (gethash k data))))
			    (gethash "keys" entry))))
	    (loop for line = (read-line iin nil nil)
		  while line
		  do (let ((entry (cl-ppcre:split " " line)))
		       (if (gethash (car entry) data)
			   (push 'i (gethash (car entry) data))
			   (warn "Key from index missing: ~a" (car entry)))))
	    (maphash (lambda (k d) (format out "~a,~a~%" k d)) data))
    data))


(defun annotated-senses (dict-file corpus)
  "all annotated senses must be valid sense keys"
  (let ((data (make-hash-table :test #'equal)))
    (with-open-files ((din dict-file)
		      (cin corpus))
      ;; reading the dict
      (loop for line = (read-line din nil nil)
	    while line
	    do (let ((entry (parse line)))
		 (mapc (lambda (k te)
			 (push te (gethash k data)))
		       (gethash "keys" entry)
		       (gethash "terms" entry))))
      ;; reading the corpus
      (loop for line = (read-line cin nil nil)
	    while line
	    do (let ((ss (parse line)))
		 (dolist (tk (gethash "tokens" ss))
		   (if (equal "man" (gethash "tag" tk))
		       (if (gethash "senses" tk)
			   (dolist (s (gethash "senses" tk))
			     (when (null (nth-value 1 (gethash s data)))
			       (warn "Key does not exist: ~a ~a" s (gethash "_id" ss))))
			   (warn "Tagged man without sense: ~a ~a"
				 (gethash "_id" ss)
				 (alexandria:hash-table-alist tk))))))))))
