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

(defun expandable-p (word)
  "checks if WORD ends and begins with #"
  (and (str:starts-with-p "#" word)
       (str:ends-with-p "#" word)))

(defun action-p (word)
  (and (str:containsp "[" word)
       (str:containsp "]" word)))

(defun has-rule-p (word)
  (str:containsp ":" word))

(defun parse-action (word)
  (dolist (w (str:split #\] word))
    (if (has-rule-p w)
	(let ((parsed (str:split #\: (subseq w 1))))
	  (push (cons (car parsed)
		      (cadr parsed))
		*action-rules*))
	(subseq w 1))))

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
