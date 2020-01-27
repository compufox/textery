;;;; textery.lisp

(in-package :textery)

(defvar *grammars* (make-hash-table :test 'equal :size 5)
  "all currently loaded grammar files")

(defvar *current-grammar* nil
  "current grammar being used")

(defun load-grammar-directory (dir)
  "loads all grammars from directory DIR"
  (when (uiop:directory-exists-p dir)
    (mapcar #'load-grammar (uiop:directory-files dir))))

(defun load-grammar (file)
  "loads the json grammar referred to by FILE"
  (when (uiop:file-exists-p file)
    (setf (gethash (pathname-name file) *grammars*)
	  (decode-json-from-string (uiop:read-file-string file)))
    (unless *current-grammar* (setf *current-grammar* (pathname-name file)))))

(defmacro with-grammar (grammar &body body)
  "executes BODY with current-grammar set to GRAMMAR"
  `(let ((*current-grammar* ,grammar))
     ,@body))

(defun grammar-value (key)
  "returns a value from the current grammar using KEY"
  (random-from-list (agetf (gethash *current-grammar* *grammars*)
			   (json-string-to-symbol key :as-keyword t))))

(defun expand (text)
  "expands TEXT"
  (let ((expanded-text (str:join
			" "
			(loop for word in (str:words text)
			      collect (if (expandable-p word)
					  (let ((word-list (str:split #\| (str:replace-all "#" "" word))))
					    (apply-arguments (grammar-value (car word-list)) (cdr word-list)))
					  word)))))
    
    ;; if we find more expandable text after expanding, we recurse
    (if (find-if #'expandable-p (str:words expanded-text))
	(expand expanded-text)
	expanded-text)))
