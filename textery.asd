;;;; textery.asd

(asdf:defsystem :textery
  :description "lisp tracery implementation"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (:str :cl-json :uiop)
  :components ((:file "package")
	       (:file "util")
	       (:file "textery")))
