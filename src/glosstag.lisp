;; (C) 2019 IBM Corporation
;;  Author: Alexandre Rademaker

(in-package :glosstag.initial)

(defparameter *lexnames*
  (alexandria:alist-hash-table
   '((0 . "adj.all")
     (1 . "adj.pert")
     (2 . "adv.all")
     (3 . "noun.Tops")
     (4 . "noun.act")
     (5 . "noun.animal")
     (6 . "noun.artifact")
     (7 . "noun.attribute")
     (8 . "noun.body")
     (9 . "noun.cognition")
     (10 . "noun.communication")
     (11 . "noun.event")
     (12 . "noun.feeling")
     (13 . "noun.food")
     (14 . "noun.group")
     (15 . "noun.location")
     (16 . "noun.motive")
     (17 . "noun.object")
     (18 . "noun.person")
     (19 . "noun.phenomenon")
     (20 . "noun.plant")
     (21 . "noun.possession")
     (22 . "noun.process")
     (23 . "noun.quantity")
     (24 . "noun.relation")
     (25 . "noun.shape")
     (26 . "noun.state")
     (27 . "noun.substance")
     (28 . "noun.time")
     (29 . "verb.body")
     (30 . "verb.change")
     (31 . "verb.cognition")
     (32 . "verb.communication")
     (33 . "verb.competition")
     (34 . "verb.consumption")
     (35 . "verb.contact")
     (36 . "verb.creation")
     (37 . "verb.emotion")
     (38 . "verb.motion")
     (39 . "verb.perception")
     (40 . "verb.possession")
     (41 . "verb.social")
     (42 . "verb.stative")
     (43 . "verb.weather")
     (44 . "adj.ppl"))
   :size 45 :test #'eql))

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

(defun assocadr (item alist)
  (second (assoc item alist :test #'equal)))

(defun synset->plist (ss)
  (list :ofs (ss-ofs ss) :pos (ss-pos ss)
	:keys (plist-keys ss)
	:gloss (ss-gloss-orig ss)
	:tokens (mapcan #'token->plist (ss-tokens ss))))

(defun plist-keys (ss)
  (let ((keys  (ss-keys  ss))
	(terms (ss-terms ss)))
    (mapcar (lambda (term) (cons (find term keys :test (lambda (term key)
							 (equal (cl-ppcre:regex-replace-all " " (string-downcase term) "_")
								(car (split-sequence:split-sequence #\% key)))))
				 term))
	    terms)))

(defun filter-attrs (attrs)
  (remove-if (lambda (a)
	       (member (car a) (list "id:id" "wf:id" "cf:id") :test #'equal))
	     attrs))

(defun token->plist (tk)
  (assert (or (null (tk-sform tk)) (= 1 (length (tk-sform tk)))))
  (labels
      ((make-senses (attrs)
	 (mapcar (lambda (id)
		   (let* ((id-attr (cadr id))
			  (sk (assocadr "sk" id-attr))
			  (lemma (assocadr "lemma" id-attr)))
		     (cons sk lemma)))
		 (remove-if-not (lambda (atrr)
				  (equal (car atrr) "id"))
				attrs)))

       (str->kw (str)
	 (when str
           (intern (string-upcase str) "KEYWORD")))

       (opt (key val)
         (and val (list key val)))

       (action-kw (tk)
         (str->kw (tk-action tk)))

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
	 (mapcar (lambda (glob)
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
		 (remove-if-not (lambda (atrr)
				  (equal (car atrr) "glob"))
				attrs)))

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

      ((equal local-name "id")
       (if (and (sax:find-attribute "coll" attributes)
		(not (state-p :reading-glob)))
	   (warn "@coll outside glob => id ~a" (sax:attribute-value (sax:find-attribute "id" attributes))))
       (push (list local-name
		   (mapcar (lambda (at)
			     (list (sax:attribute-local-name at) (sax:attribute-value at)))
			   attributes))
	     (slot-value ctk 'attrs)))

      ((equal local-name "glob")
       (state-on :reading-glob)
       (push (list local-name
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

      ((equal local-name "glob")
       (state-off :reading-glob))

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


(defun main-xml->plist (glosstag-dir out-dir &key (size 100))
  (let ((out-path (ensure-directories-exist out-dir))
	(in-files (directory (make-pathname :defaults glosstag-dir :name :wild :type "xml"))))
    (format t "Input Files: ~a~%Output Directory: ~a~%" in-files out-path)
    (split-and-save nil nil 0 0 :output-dir out-path :size size :files in-files)))

;;;; xml -> json

(defun tokens->json (tks)
  (when tks
    (let ((tk   (car tks))
	  (json (make-hash-table :test #'equal))
	  (meta (make-hash-table :test #'equal)))
      (assert (or (null (tk-sform tk)) (= 1 (length (tk-sform tk)))))
      (labels
	  ((make-senses (attrs)
	     (mapcar (lambda (id)
		       (let* ((id-attr (cadr id))
			      (sk (assocadr "sk" id-attr)))
			 sk))
		     (remove-if-not (lambda (atrr)
				      (equal (car atrr) "id"))
				    attrs)))

	   (str->kw (str)
	     (when str
	       (intern (string-upcase str) "KEYWORD")))

	   (assocadr (item alist)
	     (second (assoc item alist :test #'equal)))

	   (ex/def (tk)
	     (setf
	      (gethash "kind" json)   (list (tk-kind tk))
	      (gethash "action" meta) (tk-action tk))

	     (when (> (hash-table-count meta) 0)
	       (setf (gethash "meta" json) meta))

	     (cons json (tokens->json (cdr tks))))

	   (aux/classif/mwf (tk attrs)
	     (let ((tag (assocadr "tag" attrs))
		   (type (assocadr "type" attrs)))
	       (setf (gethash "kind" json)   (list (tk-kind tk))
		     (gethash "action" meta) (tk-action tk))
	       (and tag  (setf (gethash "tag"  json) tag))
	       (and type (setf (gethash "type" meta) type))

	       (when (> (hash-table-count meta) 0)
		 (setf (gethash "meta" json) meta))

	       (cons json (tokens->json (cdr tks)))))

	   (qf (tk attrs)
	     (let ((rend (assocadr "rend" attrs)))
	       (setf (gethash "kind" json) (list (tk-kind tk))
		     (gethash "action" meta) (tk-action tk))

	       (and rend (setf (gethash "rend" meta) rend))

	       (when (> (hash-table-count meta) 0)
		 (setf (gethash "meta" json) meta))

	       (cons json (tokens->json (cdr tks)))))

	   (wf (tk attrs)
	     (let ((tag    (assocadr "wf:tag" attrs))
		   (lemma  (assocadr "wf:lemma" attrs))
		   (pos    (assocadr "wf:pos" attrs))
		   (type   (assocadr "wf:type" attrs))
		   (rdf    (assocadr "wf:rdf" attrs))
		   (sep    (assocadr "wf:sep" attrs))
		   (senses (make-senses attrs)))
	       (setf (gethash "kind" json) (list (tk-kind tk))
		     (gethash "form" json) (car (tk-sform tk))
		     (gethash "tag"  json) tag)

	       (and  lemma  (setf (gethash "lemmas" json) (split-sequence:split-sequence #\| lemma)))
	       (and  senses (setf (gethash "senses" json) senses))
	       (and  pos    (setf (gethash "pos"    meta) pos))
	       (and  sep    (setf (gethash "sep"    meta) sep))
	       (and  type   (setf (gethash "type"   meta) type))
	       (and  rdf    (setf (gethash "rdf"    meta) rdf))

	       (when (> (hash-table-count meta) 0)
		 (setf (gethash "meta" json) meta))

	       (cons json (tokens->json (cdr tks)))))

	   (globs (attrs)
	     (mapcar (lambda (glob)
		       (let* ((json      (make-hash-table :test #'equal))
			      (glob-attr (cadr glob))
			      (lemma     (assocadr "lemma" glob-attr))
			      (tag       (assocadr "tag" glob-attr))
			      (glob      (assocadr "glob" glob-attr))
			      (coll      (assocadr "coll" glob-attr))
			      (glob-ids (remove-if-not #'(lambda (attr)
							   (and (equal (car attr) "id")
								(equal (assocadr "coll" (cadr attr)) coll)))
						       attrs))
			      (senses (make-senses glob-ids)))
			 (setf (gethash "kind"   json) (list "glob" coll)
			       (gethash "lemmas" json) (split-sequence:split-sequence #\| lemma)
			       (gethash "glob"   json) glob)

			 (and tag    (setf (gethash "tag" json)    tag))
			 (and senses (setf (gethash "senses" json) senses))
			 json))
		     (remove-if-not (lambda (atrr)
				      (equal (car atrr) "glob"))
				    attrs)))

	   (cf (tk attrs)
	     (let ((colls  (serapeum:split-sequence
			    #\, (assocadr "cf:coll" attrs)))
		   (lemma  (assocadr "cf:lemma" attrs))
		   (pos    (assocadr "cf:pos" attrs))
		   (tag    (assocadr "cf:tag" attrs))
		   (type   (assocadr "cf:type" attrs))
		   (rdf    (assocadr "cf:rdf" attrs))
		   (sep    (assocadr "cf:sep" attrs))
		   (globs  (globs attrs)))

	       (setf (gethash "kind" json) (cons (tk-kind tk) colls)
		     (gethash "form" json) (car (tk-sform tk))
		     (gethash "tag"  json) tag)

	       (and  lemma  (setf (gethash "lemmas" json) (split-sequence:split-sequence #\| lemma)))
	       (and  pos    (setf (gethash "pos"    meta) pos))
	       (and  sep    (setf (gethash "sep"    meta) sep))
	       (and  type   (setf (gethash "type"   meta) type))
	       (and  rdf    (setf (gethash "rdf"    meta) rdf))


	       (when (> (hash-table-count meta) 0)
		 (setf (gethash "meta" json) meta))

	       (if globs
		   `(,@globs ,json . ,(tokens->json (cdr tks)))
		   (cons json (tokens->json (cdr tks)))))))

	(let ((kind  (str->kw (tk-kind tk)))
	      (attrs (filter-attrs (tk-attrs tk))))
	  (case kind
	    ((:EX :DEF)           (ex/def tk))
	    ((:AUX :CLASSIF :MWF) (aux/classif/mwf tk attrs))
	    (:QF                  (qf tk attrs))
	    (:WF                  (wf tk attrs))
	    (:CF                  (cf tk attrs))))))))


(defun synset->json (synset)
  (let ((json (make-hash-table :test #'equal))
	(meta (make-hash-table :test #'equal))
	(lex_filenum (parse-integer
		      (nth 1 (serapeum:split-sequence
			      #\:
			      (car (ss-keys synset)))))))
    (setf  (gethash "sent_id" json)  (parse-integer (ss-ofs synset))
	   (gethash "text"    json)  (ss-gloss-orig synset)
	   (gethash "doc_id"  json)  (gethash lex_filenum *lexnames*)
	   (gethash "_id"     json)  (format nil "~a-~a"
					     (gethash lex_filenum *lexnames*)
					     (parse-integer (ss-ofs synset)))
	   (gethash "meta"    json)  meta
	   (gethash "keys"    meta)  (ss-keys synset)
	   (gethash "terms"   meta)  (ss-terms synset)
	   (gethash "pos"     meta)  (ss-pos synset)
	   (gethash "ofs"     meta)  (ss-ofs synset)
	   (gethash "tokens"  json)  (tokens->json (ss-tokens synset)))
    (cl-json:encode-json json)
    (format t "~%")))


(defun xml->json (filein)
  (mapcar #'synset->json (load-xml (make-pathname :defaults filein))))

(defun main-xml->json (glosstag-dir out-file)
  (let ((in-files (directory (make-pathname :defaults glosstag-dir :name :wild :type "xml"))))
    (format t "Input Files: ~a~%Output file: ~a~%" in-files out-file)
    (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
      (mapc #'xml->json in-files))))

;;; plist -> json


(defun plist-tk->json-tk (plist-tk)
  (let ((hash (make-hash-table :test #'equal)))
    (alexandria:doplist (key val plist-tk hash)
	(case key
	  (:kind
	   (cond
	     ((symbolp val)
	      (setf (gethash key hash) (list val)))
	     ((not (listp (cdr val)))
	      (setf (gethash key hash) (list (car val) (cdr val))))
	     (t
	      (setf (gethash key hash) val))))
	  ((:tag :form :glob)
	   (setf (gethash key hash) val))
	  (:senses
	   (setf (gethash key hash)
		 (mapcar #'car val)))
	  (:lemma
	   (setf (gethash :lemmas hash)
		 (split-sequence:split-sequence #\| val)))
	  (otherwise
	   (push  (cons key val)
		  (gethash :meta hash)))))))


(defun plist->json (plist stream)
  (let* ((ofs (parse-integer (getf plist :ofs)))
	 (hash (alexandria:alist-hash-table `((:sent_id . ,ofs))
					    :test #'equal)))
    (alexandria:doplist (key val plist)
	(case key
	  (:gloss   (setf (gethash :text hash)
			  val))
	  (:tokens  (setf (gethash :tokens hash)
			  (mapcar #'plist-tk->json-tk val)))
	  (:keys    (progn
		      (push (cons "terms"
				  (mapcar #'cdr val))
			    (gethash :meta hash))
		      (push (cons "keys"
				  (remove-duplicates (mapcar #'car val) :test #'equal))
			    (gethash :meta hash))))
	  (otherwise (push (cons key val)
			   (gethash :meta hash)))))

    (let ((sk (car (first (getf plist :keys)))))
      (destructuring-bind (* lexnumstr)
	  (split-sequence:split-sequence #\: sk :count 2)
	(let* ((lexnum (parse-integer lexnumstr))
	       (lexfile (gethash lexnum *lexnames*)))
	  (setf (gethash :_id hash)    (format nil "~a-~a" lexfile ofs)
		(gethash :doc_id hash) lexfile))))

    (cl-json:encode-json hash stream)
    (format stream "~%")))


(defun main-plist->json (plists out-file)
  (with-open-file (out out-file :direction :output :if-exists :supersede)
    (loop for file in plists
       do (with-open-file (in file)
	    (loop for plist = (read in nil nil)
	       while plist do
		 (plist->json plist out))))))
