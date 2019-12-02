
(in-package :glosstag)


(defstruct token
  kind form lemmas tag n-senses senses glob unsure meta)

(defstruct sent
  id meta tokens text)


(define-condition malformed-sentence (error)
  ((text  :initarg :text  :reader ms-text)
   (sent  :initarg :sent  :reader ms-sent))
  (:report (lambda (condition stream)
	     (format stream "Sentence ~a with error [~a]: ~a.~&"
		     (ms-sent condition) (ms-text condition) condition))))

(define-condition malformed-token (error)
  ((text  :initarg :text  :reader mt-text)
   (sent  :initarg :sent  :reader mt-sent)
   (token :initarg :token :reader mt-token)
   (sense :initarg :sense :reader mt-sense))
  (:report (lambda (condition stream)
	     (format stream "Token ~a at sentence ~a with error [~a]: ~a.~&"
		     (mt-token condition) (sent-id (mt-sent condition)) (mt-text condition) condition))))


(defmacro test (assertion what)
  `(assert ,assertion nil ,what))


(defun check-token (sent lexical-form->n-senses sense-index token)
  (labels ((get-sense-candidates (lemmas)
	     (let* ((lemmas (mapcar (lambda (lemma)
				      (first (serapeum:split-sequence #\% lemma)))
				    lemmas))
		    (lemmas (serapeum:nub lemmas)))
	       (reduce (lambda (n lemma) (+ n (gethash lemma lexical-form->n-senses 0)))
		       lemmas :initial-value 0)))
	   (check-sense (sense)
	     (if (null (gethash sense sense-index))
		 (error 'malformed-token :text "Sense does not exist." :token token :sent sent))))

    (test (token-kind token)  
      (error 'malformed-token :text "Must have kind." :token token :sent sent))
    (test (member (token-unsure token) (list t nil)) 
      (error 'malformed-token :text "Unsure should be t or nil." :token token :sent sent))

    (cond
      ((equal (car (token-kind token)) "wf") 
       (test (null (cdr (token-kind token))) 
	 (error 'malformed-token :text "Token must not have any collocation key." :token token :sent sent))
       (test (token-form token) 
	 (error 'malformed-token :text "Token must have form." :token token :sent sent))
       (test (null (token-glob token)) 
	 (error 'malformed-token :text "Token must not have glob marker." :token token :sent sent))
       (mapc #'check-sense (token-senses token)))
      
      ((equal (car (token-kind token)) "cf") 
       (test (token-form token) 
	 (error 'malformed-token :text "Token must have form." :token token :sent sent))
       (test (cdr (token-kind token)) 
	 (error 'malformed-token :text "Token must have a collocation key." :token token :sent sent))
       (test (null (token-glob token)) 
	 (error 'malformed-token :text "Token must not have glob marker." :token token :sent sent))
       ;; (test (null (token-senses token)) 
       ;; 	 (error 'malformed-token :text "Token must not have senses." :token token :sent sent))
       )

      ((equal (car (token-kind token)) "glob")
       (test (token-glob token) 
	 (error 'malformed-token :text "Token must have glob marker." :token token :sent sent))
       (test (null (token-form token)) 
	 (error 'malformed-token :text "Token must not have a form." :token token :sent sent))
       (mapc #'check-sense (token-senses token))))

    (when (and (not (equal "ignore" (token-tag token)))
	       (member (car (token-kind token)) '("wf" "cf" "glob") :test #'equal))
      (setf (token-n-senses token) (get-sense-candidates (token-lemmas token))))

    token))


(defun check-sent (sent lexical-form->n-senses sense-index)
  (test (sent-id sent)
    (error 'malformed-sentence :text "Sentence must have id." :sent sent))
  (test (sent-text sent)
    (error 'malformed-sentence :text "Sentence must have an original text." :sent sent))
  (setf (sent-tokens sent)
	(mapcar (lambda (tk)
		  (handler-case (check-token sent lexical-form->n-senses sense-index tk)
		    (malformed-token (e) (progn
					   (format *error-output* "!! ~a at ~a:~% > ~a~% > ~a~%"
						   (mt-text e)
						   (sent-id (mt-sent e)) (sent-text (mt-sent e))
						   (mt-token e))
					   nil))))
		(sent-tokens sent)))
  sent)
