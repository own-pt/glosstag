
(in-package :glosstag)


;;; from json format
;;; from (legacy) plist format see utils.lisp

(defun json->sent (json-str)
  (labels
      ((->tk (tk-result)
	 (make-tk :kind (gethash "kind" tk-result)
		  :form (gethash "form" tk-result)
		  :lemmas (gethash "lemmas" tk-result)
		  :tag (gethash "tag" tk-result)
		  :n-senses nil
		  :senses (gethash "senses" tk-result)
		  :glob (gethash "glob" tk-result)
		  :unsure (gethash "unsure" tk-result)
		  :meta (gethash "meta" tk-result))))
    (let ((parse-result (jonathan:parse json-str :as :hash-table)))
      (make-sent :id (gethash "_id" parse-result)
		 :meta (gethash "meta" parse-result)
		 :text (gethash "text" parse-result)
		 :tokens (mapcar #'->tk (gethash "tokens" parse-result))))))

;; main

(defun parse-sent (line format)
  (case format
    (:json (json->sent line))))


(defun list-str (lst)
  (if (null lst)
      "_"
      (serapeum:string-join lst "|")))


(defun tk->tsv (id token)
  (match token
    ((tk kind form lemmas tag n-senses senses glob unsure meta)
     (match kind
       ((list* kind keys)
	(labels
	    ((getc (key)
	       (if meta
		   (or (gethash key meta) "_")
		   "_")))
	  (format t "~{~a~^~a~}~%"
		  (list id
			#\Tab
			kind
			#\Tab
			(getc "type")
			#\Tab
			(or form "_")
			#\Tab
			(list-str lemmas)
			#\Tab
			(getc "pos")
			#\Tab
			(list-str senses)
			#\Tab
			(or n-senses "_")
			#\Tab
			(or tag "_")
			#\Tab
			(or glob "_")
			#\Tab
			(list-str keys)
			#\Tab
			(getc "sep")
			#\Tab
			(getc "rdf")
			#\Tab
			(getc "unsure")))))))))


(defun sent->tsv (sent)
  (match sent
    ((sent id text tokens)
     (format t "# id = ~a~%" id)
     (format t "# text = ~a~%" text)
     (loop
       for token in tokens
       for ix from 1
       do (tk->tsv ix token))
     (format t "~%"))))


(defun tsv-to-indices (sense-index-file)
  (let ((lexical-form->n-senses (make-hash-table :test #'equal :size 150000))
	(sense-index (make-hash-table :test #'equal :size 210000))
	(lines (rest (fare-csv:read-csv-file sense-index-file))))
    (mapc (lambda-match
	    ((list lexical-form n-senses sense-keys)
	     (setf (gethash lexical-form lexical-form->n-senses) (parse-integer n-senses))
	     (mapc (lambda (sense-key) (setf (gethash sense-key sense-index) t))
		   (serapeum:split-sequence #\Tab sense-keys))))
	  lines)
    (values lexical-form->n-senses sense-index)))


(defun release-corpus (corpus-directory-or-file output sense-index-file
		       &key (format :json))
  "Files in CORPUS-DIRECTORY-OR-FILE are one-sentence per line in
FORMAT. Convert to TSV release format placed in
OUTPUT. SENSE-INDEX-FILE is a csv file of three fields: lexical_form,
number of senses, sense keys (separated by tabs)."
  ;; You can obtain the sense index file by running the following
  ;; sparql query:
  ;; http://wnpt.sl.res.ibm.com:10035/#/repositories/wn30/query/d/select%20?lf%20(count(?ws)%20as%20?total)%20(GROUP_CONCAT(?sk;%20SEPARATOR=%22%5Ct%22)%20AS%20?senses)%20%7B%0A%20%20?ws%20wn30:word%20?w%20.%0A%20%20?w%20wn30:lexicalForm%20?lf%20.%0A%20%20?ws%20wn30:senseKey%20?sk%20.%0A%7D%0Agroup%20by%20?lf
  ;; you may need to add the purposefully ignored sense (see
  ;; https://github.com/own-pt/sensetion.el/issues/73)
  (multiple-value-bind
	(lexical-form->n-senses sense-index) (tsv-to-indices sense-index-file)
    (ensure-directories-exist output)
    (with-open-file (*standard-output* output :direction :output :if-exists :supersede)
      (mapc (lambda (file) 
	      (with-open-file (in file) 
		(loop for line = (read-line in nil nil)
		      while line
		      do (let* ((sent (parse-sent line format))
				(sent (check-sent sent lexical-form->n-senses sense-index)))
			   (sent->tsv sent)))))
	    (directory corpus-directory-or-file)))))
