;;;; textery.lisp

(in-package :textery)

(defvar *grammars* (make-hash-table :test 'equal :size 5)
  "all currently loaded grammar files")

(defvar *current-grammar* nil
  "current grammar being used")

(defvar *action-rules* nil
  "rules added by actions
very volatile, only used during evaluations")

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
  (random-from-list (agetf (append (gethash *current-grammar* *grammars*) *action-rules*)
			   (json-string-to-symbol key :as-keyword t))))

(defun process-actions (actions text)
  "processes all ACTIONS, removing them from TEXT"
  (if actions
      (let ((modified-text text))
	(dolist (a actions modified-text)

	  ;; remove bracket characters
	  (let ((stripped (ppcre:regex-replace-all "(\\[|\\])" a "")))

	    ;; if it has : in it then it's defining a new rule
	    (if (str:containsp ":" stripped)
		(destructuring-bind (key value) (str:split #\: stripped)
		  (let ((key (json-string-to-symbol key :as-keyword t))
			(value (if (str:containsp "," value)
				   (str:split #\, value)
				   (list (expand value)))))

		    ;; remove keys from the action rules in case they're already there
		    (setf *action-rules* (remove key *action-rules* :key #'car))

		    ;; then push the new rule in
		    (push (cons key value) *action-rules*)))

		;; if it doesnt have a rule, then we should expand it.
		;;  going off of the Official(tm) examples, the only
		;;  valid text that can exist inside an action is
		;;  text that expands to MORE actions.
		(expand stripped)))
	  
	  ;; remove the action text from the string
	  (setf modified-text (str:replace-all a "" modified-text))))

      ;; if we didnt get any actions, then we just 
      ;;  return the text we received
      text))

(defun process-symbols (symbols text)
  "processes all SYMBOLS, evaluating and replacing them in TEXT"
  (if symbols
      (let ((modified-text text))
	(dolist (s symbols modified-text)

	  ;; replace each symbol in text with a random value
	  ;;  from our loaded grammar
	  (setf modified-text
		(replace-first s
			       (destructuring-bind (key &rest funcs)
				   (str:split #\.
					      (str:trim (str:replace-all "#" "" s)))
				 (apply-arguments (grammar-value key) funcs))
			       modified-text))))
      text))

(defun list-grammars ()
  "returns a list of loaded grammars"
  (loop for k being the hash-key of *grammars*
	collect k))

(defun expand (text)
  "expands TEXT"
  (let* ((processed-actions (process-actions (get-all-actions text) text))
	 (expanded-text (process-symbols (get-all-symbols processed-actions)
					 processed-actions)))

    ;; if we find more expandable text after expanding, we recurse
    (if (or (get-all-symbols expanded-text)
	    (get-all-actions expanded-text))
	(expand expanded-text)
	expanded-text)))
