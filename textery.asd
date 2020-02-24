;;;; textery.asd

(asdf:defsystem :textery
  :description "tracery lisp implementation"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.2"
  :serial t
  :depends-on (:str :cl-json :uiop :cl-ppcre)
  :components ((:file "package")
	       (:file "util")
	       (:file "eng-modifiers")
	       (:file "textery")))
