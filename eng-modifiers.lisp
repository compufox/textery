(in-package :textery)

(defun a (word)
  "puts 'a' or 'an' in front of WORD"
  (format nil "~a ~a"
	  (if (vowel-p (aref word 0))
	      "an" "a")
	  word))

(defun s (word)
  "naively pluralizes WORD"
  (let ((length (length word))
	(last-letter (last-letter word)))
    (format nil "~a~a" (subseq word 0 (1- length))
	    (cond
	      ((c= last-letter "s") "ses")
	      ((c= last-letter "y")
	       (if (vowel-p (aref word (- length 2)))
		   (str:concat last-letter "s")
		   "ies"))
	      (t (str:concat last-letter "s"))))))

(defun ed (word)
  "naively applies 'ed' to the end of WORD"
  (let ((length (length word))
	(last-letter (last-letter word)))
    (format nil "~a~a" (subseq word 0 (1- length))
	    (cond
	      ((c= last-letter "e") "ed")
	      ((c= last-letter "y")
	       (if (vowel-p (aref word (- length 2)))
		   (str:concat last-letter "d")
		   "ied"))
	      (t (str:concat last-letter "ed"))))))
	    
