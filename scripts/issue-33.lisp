
(ql:quickload '(:cl-ppcre :yason :edit-distance :serapeum))


;; read WordNet Files 

(defun read-index (dict-folder)
  (let ((db (make-hash-table :test #'equal)))
    (with-open-file (in (make-pathname :name "index.sense" :defaults dict-folder))
      (loop for line = (read-line in nil nil)
	    while line
	    do  (let ((reg (cl-ppcre:split " " line)))
		  (push (cdr reg) (gethash (car reg) db '())))
	    finally (return db)))))

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


; basic validations: 1) detokenization of tokens match the text; and
; 2) keys are all valid sense-keys in WN30.

(defun main-0 ()
  (let ((wn (read-index #P"~/work/wn/WordNet-3.0/dict/")))
    (dolist (fn (directory "data/ann/annotation-??.jl"))
      (dolist (obj (read-jl-file fn))
	(cond
	  ((not (equal (text-from-tokens obj) (gethash "text" obj)))
	   (format t "~a~%" obj))

	  ((not (every (lambda (s) (gethash s wn)) (gethash "keys" obj)))
	   (format t "~a not in wn~%" (gethash "keys" obj))))))))


;; for all cases where a gloss is repeated, all tokens for the
;; repetions are the same?

(defun is-same (txt tks1 tks2)
  (loop for t1 in (mapcar #'alexandria:hash-table-alist tks1)
	for t2 in (mapcar #'alexandria:hash-table-alist tks2)
	do (when (not (equal (sort t1 #'string<= :key #'car) (sort t2 #'string<= :key #'car)))
	     (format t "D ~a~% ~a~% ~a~%" txt t1 t2))))

(defun main-1 ()
  (let ((db (make-hash-table :test #'equal)))
    (dolist (fn (directory "data/ann/annotation-??.jl"))
      (loop for obj in (read-jl-file fn)
	    for txt = (gethash "text" obj)
	    do (let ((tks (gethash txt db nil)))
		 ;; (format t "processing ~a:~a~%" fn (gethash "ofs" obj))
		 (if tks
		     (is-same txt (gethash "tokens" obj) tks)
		     (setf (gethash txt db)
			   (gethash "tokens" obj))))))))

