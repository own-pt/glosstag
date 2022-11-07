
(in-package :glosstag.initial)

;; I could have used the cxml:broadcast-handler class too. Actually,
;; sax-proxy is a subclass of broadcast-handler.

(defclass pre (cxml:sax-proxy) 
  ((state :initform nil :accessor pre-state)
   (coll :initform nil  :accessor pre-coll)))


(defmethod sax:start-element ((h pre) (namespace t) (local-name t) 
			      (qname t) (attributes t))
  (if (equal "glob" local-name)
      (push (sax:attribute-value (sax:find-attribute "coll" attributes))
	    (slot-value h 'coll)))

  (if (equal "id" local-name)
      (if (and (slot-value h 'coll)
	       (sax:find-attribute "coll" attributes))
	  (let ((id-coll (sax:attribute-value (sax:find-attribute "coll" attributes))))
	    (if (not (equal id-coll (car (slot-value h 'coll))))
		(setf (sax:attribute-value (sax:find-attribute "coll" attributes))
		      (car (slot-value h 'coll)))))))
  (call-next-method))


(defmethod sax:end-element ((h pre) (namespace t) (local-name t) (qname t))
  (if (equal local-name "glob")
      (pop (slot-value h 'coll)))
  (call-next-method))


(defun prexml (input output) 
  (with-open-file (out output :if-exists :supersede :direction :output)
    (let ((h (make-instance 'pre :chained-handler (cxml:make-character-stream-sink out))))
      (cxml:parse input h))))
