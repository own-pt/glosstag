
(in-package :glosstag)


(defstruct tk
  kind form lemmas tag n-senses senses glob unsure meta)


(defstruct sent
  id meta tokens text)


(defun check-token (sent-id lexical-form->n-senses sense-index token)
  (labels
      ((get-sense-candidates (lemmas)
	 (let* ((lemmas (mapcar (lambda (lemma)
				  (first (serapeum:split-sequence #\% lemma)))
				lemmas))
		(lemmas (serapeum:nub lemmas)))
	   (reduce (lambda (n lemma) (+ n (gethash lemma lexical-form->n-senses 0)))
		   lemmas :initial-value 0)))
       (check-sense (form sense)
	 (assert (gethash sense sense-index) (sent-id form sense)
		 "Token ~a at sentence ~a has inexisting sense ~a" form sent-id sense)))
    (match token
      ((tk kind form lemmas tag senses glob unsure)
       (assert kind (sent-id form) "Token ~a at sentence ~a must have kind" form sent-id)
       (assert (or (eq unsure t) (eq unsure nil)))
       (let-match (((list* kind maybe-keys) kind))
	 (match kind
	   ("wf"
	    (assert (null maybe-keys) (sent-id form)
		    "Token ~a at sentence ~a must not have any collocation keys" form sent-id)
	    (assert form (sent-id) "Token at sentence ~a must have form" sent-id)
	    (mapc (curry #'check-sense form) senses)
	    (assert (null glob) (sent-id form)
		    "Token ~a at sentence ~a must not have glob marker" form sent-id)
	    nil)
	   ("cf"
	    (assert form (sent-id) "Token at sentence ~a must have form" sent-id)
	    (assert maybe-keys (sent-id form)
		    "Token ~a at sentence ~a must have collocation keys" form sent-id)
	    (assert (null glob) (sent-id form)
		    "Token ~a at sentence ~a must not have glob marker" form sent-id)
	    ;; assert that there are no sense annotations?
	    )
	   ("glob"
	    (let-match (((list glob-key) maybe-keys))
	      (assert glob (sent-id glob-key)
		      "Glob ~a at sentence ~a must have glob marker" glob-key sent-id)
	      (assert (null form)))))
	 (match tag
	   ((not "ignore")
	    (when (member kind '("wf" "cf" "glob") :test #'equal)
	      (setf (tk-n-senses token) (get-sense-candidates lemmas))))))
       token))))


(defun check-sent (sent lexical-form->n-senses sense-index)
  (ematch sent
    ((sent id meta text tokens)
     (assert id nil "Sentence must have id")
     (assert text nil "Sentence must have original text")
     (setf (sent-tokens sent)
	   (mapcar (curry #'check-token id lexical-form->n-senses sense-index) tokens))
     sent)))

