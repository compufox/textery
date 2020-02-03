;;;; util.lisp

(in-package :textery)

(declaim (inline parse-function agetf random-from-list))

(defun apply-arguments (text arguments)
  "applies all ARGUMENTS to TEXT

ARGUMENTS is a list of strings that refer to lisp functions"
  (if arguments
      (let ((func (str:split #\, (car arguments))))
	(apply-arguments
	 (apply (parse-function (car func)) text (cdr func))
	 (cdr arguments)))
      text))

(defun parse-function (name)
  "returns a function of name NAME"
  (symbol-function (intern (string-upcase (str:trim name)))))

(defun json-string-to-symbol (json-string &key as-string as-keyword)
  "converts camelCase JSON-STRING to a symbol

if AS-STRING is non-nil, returns a string instead of a symbol
if AS-KEYWORD is non-nil, returns a keyword"
  (let ((converted-name (with-output-to-string (out)
			  (loop
			     with index = 0
			     for c across json-string
			     if (and (upper-case-p c) (> index 0))
			      do (format out "-~a" (char-downcase c))
			     
			     else if (char= c #\_)
			      do (format out "--")
			     
			     else do (format out "~a" c)
			       
			     do (setf index (1+ index))))))
    (if as-string
	(string-upcase converted-name)
	(if as-keyword
	    (intern (string-upcase converted-name) :keyword)
	    (intern converted-name)))))

(defun random-from-list (lst)
  "returns a random element from LST"
  (nth (random (length lst)) lst))

(defun agetf (place indicator &optional default)
  "getf but for alists"
  (or (cdr (assoc indicator place :test #'equal))
      default))

(defun get-all-symbols (text)
  "retrieves all symbols from TEXT"
  (ppcre:all-matches-as-strings "#([^#]*)#" text))

(defun get-all-actions (text)
  "retrieves all actions from TEXT"
  (ppcre:all-matches-as-strings "\\[([^\\]]*)\\]" text))

(defun replace-first (old new s)
  "replaces the first occurance of OLD with NEW in S

if OLD isn't found in S, returns S as-is"
  (let ((start (search old s :test #'string=)))
    (if start
	(concatenate 'string
		     (str:replace-all old new (subseq s 0 (+ start (length old))))
		     (subseq s (+ start (length old))))
	s)))
  
