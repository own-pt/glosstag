
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


;;;; read json-lines files

;; removes the 'meta' extra group of fields from the synsets and
;; tokens

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


;; remove the qf begin/end transform them into wf tokens with type
;; punctuation.

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

;; replace the 'pos' field in the synset with 'type' and fix the ones
;; that should be 's' not 'a'

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

;; remove the extra semi-colon from the end of the 'meta' text and
;; tokens. Considering that WordNet 3.0 constains only 72 cases,
;; exceptions.
;; rg ";[ ]*$" ../WordNet-3.0/dict/data.* | wc -l

(defun fix4 (obj)
  (if (cl-ppcre:scan ";$" (gethash "text" obj))
      (setf (gethash "text" obj)
	    (string-trim '(#\; #\Space) (gethash "text" obj))))
  (let ((tks (reverse (gethash "tokens" obj))))
    (if (and (equal :no     (gethash "form"   (car tks) :no))
	     (equal "close" (gethash "action" (car tks)))
	     (equal ";"     (gethash "form"   (cadr tks))))
	(setf (gethash "tokens" obj)
	      (reverse (cons (car tks) (cddr tks)))))
    obj))


;; fixing the 'sep' field of tokens. This solves the detokenization of
;; the tokens.

(defun fix5 (obj)
  (let* ((tks  (coerce (gethash "tokens" obj) 'vector))
	 (n    (length (gethash "tokens" obj)))
	 (quotes "“”‘’")
	 (news (do* ((i 0)
		     (j 1))
		    ((and (> i (- n 2)) (> j (- n 1))) tks)
		 (let ((a (aref tks i))
		       (b (aref tks j)))
		   (cond
		     ((not (gethash "form" a))
		      (setf i j j (1+ j)))

		     ((and (gethash "form" a) (= j (- n 1)) (not (gethash "form" b)))
		      (setf (gethash "sep" a) ""
			    i j j (1+ j)))

		     ((and (< j (- n 1)) (not (gethash "form" b)))
		      (setf j (1+ j)))

		     ((and (search (gethash "form" a) quotes)
			   (search (gethash "form" b) quotes))
		      (setf (gethash "sep" a) ""
			    i j j (1+ j)))

		     ((and (gethash "form" a) 
			   (search (gethash "form" b) ");,."))
		      (setf (gethash "sep" a) ""
			    i j j (1+ j)))

		     ((and (gethash "form" a)
			   (search (gethash "form" b) "”’"))
		      (setf (gethash "sep" a) ""
			    i j j (1+ j)))

		     ((and (search (gethash "form" a) ")")
			   (search (gethash "form" b) ";.,"))
		      (setf (gethash "sep" a) ""
			    i j j (1+ j)))
		
		     (t (setf i j j (1+ j))))))))
    (setf (gethash "tokens" obj)
	  (coerce news 'list))
    obj))


;; fixes cases of named quotations, when we don't have space between
;; the dash, quote and the name.  "the quote" - A. R. Weads

(defun fix6 (obj)
  (if (cl-ppcre:scan "\"- ([A-Z])" (gethash "text" obj))
      (setf (gethash "text" obj)
	    (cl-ppcre:regex-replace-all "\"- ([A-Z])" (gethash "text" obj) "\" - \\1")))
  obj)


(defun fix-obj (obj wn)
  (fix6 (fix5 (fix4 (fix3 (fix2 (fix1 obj)) wn)))))


(defun read-jl-file (fn wn)
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (fix-obj (yason:parse line) wn))))


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

(defun to-ascii (txt)
  (substitute #\' #\RIGHT_SINGLE_QUOTATION_MARK 
	      (substitute #\` #\LEFT_SINGLE_QUOTATION_MARK 
			  (substitute #\" #\RIGHT_DOUBLE_QUOTATION_MARK 
				      (substitute #\"  #\LEFT_DOUBLE_QUOTATION_MARK txt)))))


(defun main-0 ()
  (let ((wn (read-wordnet #P"~/work/wn/WordNet-3.0/dict/")))
    (dolist (fn (directory "data/annotation-*.jl"))
      (dolist (obj (read-jl-file fn wn))
	(let ((res (describe-obj obj wn)))
	  (when (not (equal (getf res :text-meta) (to-ascii (getf res :text-toks))))
	      (format t "~a~%  wn:[~a]~% txt:[~a]~% tks:[~a]~%"
		      (getf res :id) (getf res :text-wn) (getf res :text-meta) (getf res :text-toks)
		      ;; (mapcar #'alexandria:hash-table-alist (gethash "tokens" obj))
		      )))))))


(defun main-1 ()
  (let ((wn (read-wordnet #P"~/work/wn/WordNet-3.0/dict/")))
    (dolist (fn (directory "data/*.jl"))
      (dolist (obj (read-jl-file fn wn))
	(let ((res (describe-obj obj wn)))
	  (when (> (edit-distance:distance (getf res :text-meta) (getf res :text-wn)) 0)
	    (format t "~a~% w:[~a]~% m:[~a]~%~%" (getf res :id) (getf res :text-wn) (getf res :text-meta))))))))

(defun main-2 ()
  (let ((wn (read-wordnet #P"~/work/wn/WordNet-3.0/dict/"))
	(ok '((:SUBSTITUTION #\" #\LEFT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\" #\RIGHT_DOUBLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\` #\LEFT_SINGLE_QUOTATION_MARK)
	      (:SUBSTITUTION #\' #\RIGHT_SINGLE_QUOTATION_MARK)
	      (:INSERTION NIL #\RIGHT_SINGLE_QUOTATION_MARK)
	      (:INSERTION NIL #\RIGHT_DOUBLE_QUOTATION_MARK)
	      (:INSERTION NIL #\Space)
	      (:SUBSTITUTION #\" #\Space)
	      (:SUBSTITUTION #\` #\Space)
	      (:SUBSTITUTION #\' #\Space))))
    (dolist (fn (directory "data/*.jl"))
      (dolist (obj (read-jl-file fn wn))
	(let* ((res (describe-obj obj wn))
	       (ops (remove-if (lambda (a) (equal :MATCH (car a))) (edit-distance:diff (getf res :text-meta) (getf res :text-toks)))))
	  (when (set-difference ops ok :test #'equal)
	    (format t "~a~% m:[~a]~% t:[~a]~% ~s~%~%" (getf res :id) (getf res :text-meta) (getf res :text-toks) ops)))))))
