;; (C) 2019 IBM Corporation
;;  Author: Alexandre Rademaker

(in-package :glosstag)

(defvar *state* nil)

(defun state-p (id)
  (member id *state*))

(defun state-on (&rest ids)
  (setf *state* (union ids *state*)))

(defun state-off (&rest ids)
  (setf *state* (if (null ids) nil
		    (set-difference *state* ids))))


(defclass synset ()
  ((id         :initform nil :initarg :id  :accessor ss-id)
   (ofs        :initform nil :initarg :ofs :accessor ss-ofs)
   (pos        :initform nil :initarg :pos :accessor ss-pos)
   (terms      :initform nil :accessor ss-terms)
   (keys       :initform nil :accessor ss-keys)
   (gloss-orig :initform nil :accessor ss-gloss-orig)
   (gloss-tok  :initform nil :accessor ss-gloss-tok)
   (tokens     :initform nil :accessor ss-tokens)))


(defclass token ()
  ((kind    :initform nil :initarg :kind   :accessor tk-kind)
   (action  :initform nil :initarg :action :accessor tk-action)
   (attrs   :initform nil :initarg :attrs  :accessor tk-attrs)
   (sform   :initform nil :initarg :sform  :accessor tk-sform)))

(defclass wordnet-handler (sax:default-handler)
  ((synsets :initform nil :accessor wh-synsets) 
   (css     :initform nil :accessor wh-css)
   (ctk     :initform nil :accessor wh-ctk)))


(defun fix-sense-key (str)
  (let ((lemma+lex_sense (serapeum:split-sequence-if
			  #'(lambda (char)
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

(defun synset->plist (ss)
  (list :ofs (ss-ofs ss) :pos (ss-pos ss)
	:keys (mapcar #'cons (mapcar #'fix-sense-key (ss-keys ss)) (ss-terms ss))
	:gloss (ss-gloss-orig ss)
	:tokens (mapcan #'token->plist (ss-tokens ss))))

(defun filter-attrs (attrs)
  (remove-if (lambda (a)
	       (member (car a) (list "id:id" "wf:id" "cf:id") :test #'equal))
	     attrs))

(defun token->plist (tk)
  (assert (or (null (tk-sform tk)) (= 1 (length (tk-sform tk)))))
  (labels
      ((make-senses (attrs)
	 (let ((ids (remove-if-not #'(lambda (atrr)
				       (equal (car atrr) "id"))
				   attrs)))
	   (mapcar #'(lambda (id)
		       (let* ((id-attr (cadr id))
			      (sk (assocadr "sk" id-attr))
			      (lemma (assocadr "lemma" id-attr)))
			 (cons (fix-sense-key sk) lemma)))
		   ids)))
       
       (str->kw (str)
	 (when str
           (intern (string-upcase str) "KEYWORD")))
       
       (opt (key val)
         (and val (list key val)))
       
       (action-kw (tk)
         (str->kw (tk-action tk)))
       
       (assocadr (item alist)
         (second (assoc item alist :test #'equal)))
       
       (ex (tk)
         (list :kind :ex :action (action-kw tk)))
       
       (aux (tk attrs)
         (let ((tag (assocadr "tag" attrs))
               (type (assocadr "type" attrs)))
           (append (list :kind :aux :action (action-kw tk))
                   (opt :tag  (str->kw tag))
                   (opt :type (str->kw type)))))
       
       (def (tk)
         (list :kind :def :action (action-kw tk)))
       
       (classif (tk attrs)
         (let ((type (assocadr "type" attrs)))
           (append (list :kind :classif :action (action-kw tk))
                   (opt :type (str->kw type)))))
       
       (mwf (tk attrs)
         (let ((type (assocadr "type" attrs)))
           (append (list :kind :mwf :action (action-kw tk))
                   (opt :type (str->kw type)))))
       
       (qf (tk attrs)
         (let ((rend (assocadr "rend" attrs)))
           (append (list :kind :qf :action (action-kw tk))
		   (opt :rend (str->kw rend)))))
       
       (wf (tk attrs)
         (let ((tag   (assocadr "wf:tag" attrs))
	       (lemma (assocadr "wf:lemma" attrs))
               (pos (assocadr "wf:pos" attrs))
               (type (assocadr "wf:type" attrs))
               (rdf (assocadr "wf:rdf" attrs))
               (sep (assocadr "wf:sep" attrs))
               (senses (make-senses attrs)))
           (append
            (list :kind :wf :form (car (tk-sform tk)))
            (opt :lemma lemma) (opt :pos pos)
	    (list :tag tag) (opt :senses senses)
            (opt :sep sep) (opt :type type) (opt :rdf rdf))))

       (globs (tk attrs)
	 (let ((globs (remove-if-not #'(lambda (atrr)
					 (equal (car atrr) "glob"))
				     attrs)))
	   (when globs
	    (mapcar
	     #'(lambda (glob)
		 (let* ((glob-attr (cadr glob))
			(lemma  (assocadr "lemma" glob-attr))
			(tag    (assocadr "tag" glob-attr))
			(glob   (assocadr "glob" glob-attr))
			(coll   (assocadr "coll" glob-attr))
			(glob-ids (remove-if-not #'(lambda (attr)
						     (and (equal (car attr) "id")
							  (equal (assocadr "coll" (cadr attr)) coll)))
						 attrs))
			(senses (make-senses glob-ids)))
		   (append
		    (list :kind `(:glob . ,coll) :lemma lemma)
		    (opt :tag tag) (opt :senses senses) (list :glob glob))))
	     globs))))
       
       (cf (tk attrs)
	 (let ((colls  (serapeum:split-sequence
			#\, (assocadr "cf:coll" attrs)))	       
	       (lemma  (assocadr "cf:lemma" attrs))
	       (pos    (assocadr "cf:pos" attrs))
	       (tag    (assocadr "cf:tag" attrs))
	       (type   (assocadr "cf:type" attrs))
               (rdf    (assocadr "cf:rdf" attrs))
               (sep    (assocadr "cf:sep" attrs)))
	   (append
	    (globs tk attrs)
	    (list
	     (append
	      (list :kind `(:cf . ,colls) :form (car (tk-sform tk)))
	      (opt :lemma lemma) (opt :pos pos) (list :tag tag)
	      (opt :sep sep) (opt :type type) (opt :rdf rdf)))))))

    (let ((kind  (str->kw (tk-kind tk)))
          (attrs (filter-attrs (tk-attrs tk))))
      (case kind
        (:EX (list (ex tk)))
        (:AUX (list (aux tk attrs)))
        (:DEF (list (def tk)))
        (:CLASSIF (list (classif tk attrs)))
        (:MWF (list (mwf tk attrs)))
        (:QF (list (qf tk attrs)))
        (:WF (list (wf tk attrs)))
	(:CF (cf tk attrs))))))


(defmethod sax:start-element ((wh wordnet-handler) (namespace t) (local-name t) (qname t) 
			      (attributes t))
  (with-slots (css ctk) wh
    (cond 
      ((equal local-name "synset")
       (setf css (make-instance 'synset
				:id  (sax:attribute-value (sax:find-attribute "id"  attributes))
				:pos (sax:attribute-value (sax:find-attribute "pos" attributes))
				:ofs (sax:attribute-value (sax:find-attribute "ofs" attributes)))))
      ((equal local-name "term")
       (state-on :reading-term))
      ((equal local-name "sk")
       (state-on :reading-key))

      ((equal local-name "gloss")
       (let* ((av (sax:attribute-value (sax:find-attribute "desc" attributes))))
	 (switch (av :test #'equal)
	   ("orig"  (state-on :reading-gloss-orig))
	   ("text"  (state-on :reading-gloss-tok)))))

      ((member local-name '("cf" "wf") :test #'equal)
       (state-on :reading-token)
       (let ((tk (make-instance 'token :kind local-name)))
	 (mapcar (lambda (at)
		   (push (list (format nil "~a:~a" local-name (sax:attribute-local-name at))
			       (sax:attribute-value at))
			 (slot-value tk 'attrs)))
		 attributes)
	 (setf ctk tk)))

      ((member local-name '("mwf" "qf" "aux" "classif" "ex" "def") :test #'equal)
       (let ((tk (make-instance 'token :kind local-name :action "open")))
	 (mapcar (lambda (at)
		   (push (list (sax:attribute-local-name at) (sax:attribute-value at))
			 (slot-value tk 'attrs)))
		 attributes)
	 (push tk (ss-tokens css))))

      ((member local-name (list "id" "glob") :test #'equal)
       (push
	(list local-name
	      (mapcar (lambda (at)
			(list (sax:attribute-local-name at) (sax:attribute-value at)))
		      attributes))
	(slot-value ctk 'attrs))))))


(defmethod sax:end-element ((wh wordnet-handler) (namespace t) (local-name t) (qname t))
  (with-slots (synsets css ctk) wh
    (cond 
      ((equal local-name "synset")
       (setf (slot-value css 'tokens)
	     (reverse (slot-value css 'tokens)))
       (dolist (slot (list 'gloss-orig 'gloss-tok))
	 (setf (slot-value css slot)
	       (format nil "~{~a~^ ~}" (reverse (slot-value css slot)))))
       (push css synsets)
       (setf css nil))
      ((equal local-name "term")
       (state-off :reading-term))
      ((equal local-name "sk")
       (state-off :reading-key))
      ((equal local-name "gloss")
       (state-off :reading-gloss-tok)
       (state-off :reading-gloss-orig))

      ((member local-name '("mwf" "qf" "aux" "classif" "def" "ex") :test #'equal)
       (push (make-instance 'token :kind local-name :action "close") (ss-tokens css)))

      ((member local-name '("cf" "wf") :test #'equal)
       (state-off :reading-token)
       (push ctk (ss-tokens css))
       (setf ctk nil)))))


(defmethod sax:characters ((wh wordnet-handler) (data t))
  (with-slots (css ctk) wh
    (cond 
     ((state-p :reading-term)
      (push data (slot-value css 'terms)))
     ((state-p :reading-key)
      (push data (slot-value css 'keys)))
     ((state-p :reading-gloss-orig)
      (if (> (length (string-trim '(#\Space #\Tab #\Newline) data)) 0)
	  (push data (slot-value css 'gloss-orig))))
     ((state-p :reading-gloss-tok)
      (if (> (length (string-trim '(#\Space #\Tab #\Newline) data)) 0)
	  (push data (slot-value css 'gloss-tok))))
     ((state-p :reading-token)
      (if (> (length (string-trim '(#\Space #\Tab #\Newline) data)) 0)
	  (push data (slot-value ctk 'sform)))))))


(defun load-xml (filename) 
  (let ((wh (make-instance 'wordnet-handler))
	(*state* nil))
    (cxml:parse filename wh)
    (slot-value wh 'synsets)))


(defun xml->plist (filein fileout)
  (with-open-file (out fileout :direction :output :if-exists :supersede) 
    (format out "~{~s~%~}" (mapcar #'synset->plist (load-xml (make-pathname :defaults filein))))))



;; saving

(defun save-plists (plists dir basename)
  (format t "Saving ~a ~a~%" dir basename)
  (with-open-file (out (make-pathname :name basename :type "plist" :defaults dir)
		       :direction :output :if-exists :supersede)
    (mapcar (lambda (pl)
	      (write pl :pretty nil :case :downcase :stream out)
	      (terpri out))
	    plists)))


(defun split-and-save (source current-group current-size group &key output-dir size files)
  (cond
    ((null source)
     (if files
	 (split-and-save (mapcar #'synset->plist (load-xml (make-pathname :defaults (car files))))
			 current-group current-size group
			 :output-dir output-dir :size size :files (cdr files))
	 (if current-group
	     (save-plists current-group output-dir (format nil "~a" group)))))

    ((and source (< current-size size))
     (split-and-save (cdr source)
		     (cons (car source) current-group) (1+ current-size) group
		     :output-dir output-dir :size size :files files))

    ((and current-group (>= current-size size))
     (save-plists current-group output-dir (format nil "~a" group))
     (split-and-save source nil 0 (1+ group)
		     :output-dir output-dir :size size :files files))

    (t (error "problem!"))))


(defun main (glosstag-dir out-dir &key (size 100))
  (let ((out-path (ensure-directories-exist out-dir))
	(in-files (directory (make-pathname :defaults glosstag-dir :name :wild :type "xml"))))
    (format t "Input Files: ~a~%Output Directory: ~a~%" in-files out-path)
    (split-and-save nil nil 0 0 :output-dir out-path :size size :files in-files)))
