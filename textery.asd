;;;; textery.asd

(asdf:defsystem :textery
  :description "tracery lisp implementation"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.1"
  :serial t
  :depends-on (:str :cl-json :uiop)
  :components ((:file "package")
	       (:file "util")
	       (:file "textery")))
