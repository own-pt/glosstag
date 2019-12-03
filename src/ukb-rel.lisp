
(in-package :glosstag)

(defun sent->rel (sent index)
  (let* ((meta            (sent-meta sent))
	 (sent-ofs        (gethash "ofs" meta))
	 (sent-pos        (gethash "pos" meta))
	 (sent-synset-id  (format nil "~a-~a" sent-ofs sent-pos)))
    (mapcar
     #'(lambda (tk)
	 (match tk
	   ((token kind form lemmas tag n-senses senses glob unsure meta)
	    (match kind
	      ((list* kind _)
	       (when (member kind '("wf" "glob") :test #'equal)
		   (mapcar #'(lambda (sense)
			       (if (and (gethash sense index) (not (equal sent-synset-id (gethash sense index))))
				   (format t "u:~a v:~a t:gloss~%" sent-synset-id (gethash sense index))
				   (if (not (gethash sense index))
				       (warn sense))))
			  senses)))))))
     (sent-tokens sent))))


(defun read-index (index-files)
  (let ((map       (make-hash-table :test #'equal :size 150000))
	(num->pos  (alexandria:plist-hash-table '("1" "n" "2" "v" "3" "a" "4" "r" "5" "a") :test #'equal)))
    (with-open-file (in index-files)
      (loop for line = (read-line in nil nil)
	 while line do
	   (let* ((sline (serapeum:split-sequence #\space line))
		  (sense-key      (car sline))
		  (synset-offset  (cadr sline))
		  (pos            (gethash
				   (car
				    (serapeum:split-sequence
				     #\: (car
					  (last
					   (serapeum:split-sequence
					    #\% sense-key)))))
				   num->pos)))
	     (setf (gethash sense-key map)
		   (format nil "~a-~a" synset-offset pos)))))
    map))


(defun genarate-ukb-rels-file (corpus-directory-or-file output index-file
			       &key (format :json))
  (let ((index (read-index index-file)))
    (ensure-directories-exist output)
    (with-open-file (*standard-output* output :direction :output :if-exists :supersede)
      (mapc (lambda (file)
	      (with-open-file (in file)
		(loop for line = (read-line in nil nil)
		   while line
		   do (let* ((sent (parse-sent line format)))
			(sent->rel sent index)))))
	    (directory corpus-directory-or-file)))))
