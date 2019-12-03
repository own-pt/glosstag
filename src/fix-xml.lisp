
(in-package :glosstag.initial)


(defun fix-sense-key (str)
  (let ((lemma+lex_sense (serapeum:split-sequence-if
			  (lambda (char)
			    (or (char-equal #\: char)
				(char-equal #\% char)))
			  str)))
    (if (and
	 (string-equal (nth 1 lemma+lex_sense) "3")
	 (not (string-equal (nth 4 lemma+lex_sense) "")))
	(progn
	  (setf (nth 1 lemma+lex_sense) "5")
	  (format nil "~a%~a:~{~a~^:~}" (first lemma+lex_sense)
                  (second lemma+lex_sense)
                  (cddr lemma+lex_sense)))
	str)))


(defun fix-sense-key-xml (in-file)
  (with-open-file (in in-file)
    (loop
       for line = (read-line in nil nil)
       while line do
	 (format t "~a~%"
		 (cl-ppcre:regex-replace "<sk>(.*)</sk>|sk=\"(.*)\"" line
					 #'(lambda (match &rest registers)
					     (fix-sense-key match))
					 :simple-calls t)))))


(defun fix-adjective-satellites (glosstag-dir out-dir)
  (let ((in-files (directory (make-pathname :defaults glosstag-dir :name :wild :type "xml")))
	(out-path (ensure-directories-exist out-dir)))
    (format t "Input Files: ~a~%Output Directory: ~a~%" in-files out-path)
    (mapcar
     #'(lambda (in-file)
	 (let ((out-file (merge-pathnames out-dir in-file)))
	   (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
	     (fix-sense-key-xml in-file))
	   (format t "Writing ~a~%" out-file)))
     in-files)))


(defun fix-mismatch (in-file)
  (with-open-file (in in-file)
    (loop
       with last-line-globp = nil
       with glob-coll = ""
       for line = (read-line in nil nil)
       while line do
	 (cl-ppcre:do-register-groups (coll)
	     ("<glob coll=\"(.)\"" line)
	   (setf last-line-globp t
		 glob-coll coll))

	 (if last-line-globp
	     (progn
	       (if (cl-ppcre:scan (format nil "<id coll=\"(~a)\"" glob-coll) line)
		   (format t "~a~%" line)
		   (format t "~a~%" (cl-ppcre:regex-replace "<id coll=\"(.)\""
							    line (format nil "<id coll=\"~a\"" glob-coll)))))
	     (progn
	       (when (cl-ppcre:scan "</glob>" line)
		(setf last-line-globp nil
		      glob-coll ""))
	       (when (and (equal glob-coll "") (null last-line-globp))
		 (format t "~a~%" line)))))))


(defun fix-glob@coll-id@coll-mismatch (glosstag-dir out-dir)
  (let ((in-files (directory (make-pathname :defaults glosstag-dir :name :wild :type "xml")))
	(out-path (ensure-directories-exist out-dir)))
    (format t "Input Files: ~a~%Output Directory: ~a~%" in-files out-path)
    (mapcar
     #'(lambda (in-file)
	 (let ((out-file (merge-pathnames out-dir in-file)))
	  (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
	    (fix-mismatch in-file))
	  (format t "Writing ~a~%" out-file)))
     in-files)))
