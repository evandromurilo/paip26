(defpackage #:eliza
  (:use #:cl))

(in-package :eliza)

(defun pat-match (pat input &optional (bindings nil))
  "Does pattern match innput?"
  (if (and (null pat) (null input))
      (values t bindings)
      (if (variable-p pat)
	  (let ((prev (lookup pat bindings)))
	    (if prev
		(if (equal prev input)
		    (values t nil)
		    (values nil nil))
		(values t (list (make-binding pat input)))))
	  (if (atom pat)
	      (if (equal pat input)
		  (values t nil)
		  (values nil nil))
	      (multiple-value-bind (success new-bindings) (pat-match (first pat) (first input) bindings)
		(if success
		    (pat-match (rest pat) (rest input) (append new-bindings bindings))
		    (values nil nil)))))))

(defun variable-p (thing)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp thing)
       (equal #\? (aref (string thing) 0))))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun make-binding (var val)
  "Make a new (var . value) pair for a binding list."
  (cons var val))
