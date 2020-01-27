;;;; package.lisp

(defpackage textery
  (:use :cl)
  (:import-from :json
		:decode-json-from-string)
  (:export :with-grammar
           :expand
	   :load-grammar
	   :load-directory))
