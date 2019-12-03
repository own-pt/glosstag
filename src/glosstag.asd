;; (C) 2019 IBM Corporation
;;  Author: Alexandre Rademaker

(asdf:defsystem #:glosstag
  :serial t
  :depends-on (:cxml :cl-ppcre :cl-trie
	       :fare-csv :jonathan
	       :serapeum :trivia.balland2006)
  :components ((:file "package")
	       (:file "glosstag" :depends-on ("package"))
	       (:file "utils"    :depends-on ("package"))
	       (:file "data"     :depends-on ("package"))
	       (:file "export"   :depends-on ("data"))
	       (:file "ukb-rel"  :depends-on ("export"))
	       (:file "fix-xml"  :depends-on ("package"))))

