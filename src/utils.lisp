
(in-package :glosstag)

(defclass line ()
  ((id     :initarg :id     :initform "_" :accessor line-id)
   (kind   :initarg :kind   :initform "_" :accessor line-kind)
   (type   :initarg :type   :initform "_" :accessor line-type)
   (form   :initarg :form   :initform "_" :accessor line-form)
   (lemma  :initarg :lemma  :initform "_" :accessor line-lemma)
   (pos    :initarg :pos    :initform "_" :accessor line-pos)
   (nsense :initarg :nsense :initform "_" :accessor line-nsense)
   (sense  :initarg :sense  :initform "_" :accessor line-sense)
   (tag    :initarg :tag    :initform "_" :accessor line-tag)
   (glob-i :initarg :glob-i :initform "_" :accessor line-glob-i)
   (glob-t :initarg :glob-t :initform "_" :accessor line-glob-t)
   (sep    :initarg :sep    :initform "_" :accessor line-sep)
   (rdf    :initarg :rdf    :initform "_" :accessor line-rdf)
   (unsure :initarg :unsure :initform "_" :accessor line-unsure)))


(defun process-token (id tk stream tb)
  (let ((line (token-to-line id tk))
	(sep #\Tab))
    (when line
      (lemmas-to-senses line tb) 
      (format stream "~{~a~^~a~}~%"
	      (reduce (lambda (a l) (cons (slot-value line a) (if (null l) nil (cons sep l))))
		      '(id kind type form lemma pos sense nsense tag glob-i glob-t sep rdf unsure)
		      :initial-value nil :from-end t)))))


(defun lemmas-to-senses (line tb)
  (let ((lemma (line-lemma line)))
    (if (stringp lemma)
	(setf (line-nsense line)
	      (reduce (lambda (res s)
			(+ res (gethash (subseq  s 0 (position #\% s)) tb 0)))
		      (cl-ppcre:split "\\|" lemma) :initial-value 0)))))


(defun token-to-line (id tk)
  (labels ((getc (key)
	     (if-let ((res (getf tk key)))
	       res
	       "_"))
	   (lst-to-str (lst)
	     (if (null lst)
		 "_"
		 (format nil "~{~a~^|~}" lst))))
    (let ((senses (lst-to-str (sort (mapcar #'car (getf tk :senses)) #'string<=))))
      (cond
	((member (getf tk :kind) (list :classif :def :mwf :qf :aux :ex))
	 (let ((line (make-instance 'line :id id :kind (getc :kind) :type (getc :type)
					  :form (getc :action) :lemma (getc :rend) :tag (getc :tag)))
	       (db (remove-from-plist tk :kind :type :action :rend :tag)))
	   (assert (null db))
	   line))
	
	((equal :wf (getf tk :kind))
	 (let ((line (make-instance 'line :id id :kind (getc :kind) :type (getc :type)
					  :form (getc :form) :lemma (getc :lemma) :pos (getc :pos)
					  :tag (getc :tag) :sense senses :sep (getc :sep)
					  :rdf (getc :rdf) :unsure (getc :unsure)))
	       (db (remove-from-plist tk :kind :type :form :lemma :pos :tag :senses :sep :rdf :unsure)))
	   (assert (null db))
	   line))

	((equal :cf   (car (getf tk :kind)))
	 (let ((line (make-instance 'line :id id :kind (car (getc :kind)) :type (getc :type)
					  :form (getc :form) :lemma (getc :lemma) :pos (getc :pos) :tag (getc :tag)
					  :sense senses
					  :sep (getc :sep) :glob-i (lst-to-str (cdr (getf tk :kind))) :rdf (getc :rdf)))
	       (db (remove-from-plist tk :kind :type :form :lemma :pos :tag :senses :sep :rdf)))
	   (assert (null db))
	   line))

	((equal :glob (car (getf tk :kind)))
	 (let ((line (make-instance 'line :id id :kind (car (getc :kind)) :type (getc :type)
					  :form (getc :form) :lemma (getc :lemma) :tag (getc :tag)
					  :sense senses
					  :glob-i (cdr (getc :kind)) :glob-t (getc :glob)))
	       (db (remove-from-plist tk :kind :type :form :lemma :tag :senses :glob)))
	   (assert (null db))
	   line))
       
	(t (error "invalid object ~a" tk))))))


(defun process-entry (data stream tb)
  (format stream "~%# txt = ~a~%# id = ~a-~a~%" (getf data :gloss) (getf data :ofs) (getf data :pos))
  (loop for token in (getf data :tokens)
	for id = 1 then (incf id)
	do (process-token id token stream tb)))


(defun csv-to-hash (filename)
  (let ((tb (make-hash-table :test #'equal)))
    (mapc
     (lambda (p)
       (setf (gethash (car p) tb) (parse-integer (cadr p))))
     (cdr (fare-csv:read-csv-file filename)))
    tb))

(defun process-corpus (corpus-directory output query-file)
  (let ((tb (csv-to-hash query-file)))
    (with-open-file (out output :direction :output :if-exists :supersede)
    (mapc (lambda (file) 
	    (with-open-file (in file) 
	      (loop for line = (read in nil nil)
		    while line
		    do (process-entry line out tb))))
	  (directory corpus-directory)))))

