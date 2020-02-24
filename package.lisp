;;;; package.lisp

(defpackage textery
  (:use :cl)
  (:shadow :ed)
  (:import-from :json
		:decode-json-from-string)
  (:export :with-grammar
           :expand
	   :load-grammar
	   :load-grammar-directory))
