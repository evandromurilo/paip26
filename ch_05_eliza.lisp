(defpackage #:eliza
  (:use #:cl))

(in-package :eliza)

(defun pat-match (pat input &optional (matches nil))
  (if (and (null pat) (null input))
      (values t matches)
      (if (variable-p pat)
	  (let ((prev (assoc pat matches)))
	    (if prev
		(if (equal (cdr prev) input)
		    (values t nil)
		    (values nil nil))
		(values t (list (make-match pat input)))))
	  (if (atom pat)
	      (if (equal pat input)
		  (values t nil)
		  (values nil nil))
	      (multiple-value-bind (success new-matches) (pat-match (first pat) (first input) matches)
		(if success
		    (pat-match (rest pat) (rest input) (append new-matches matches))
		    (values nil nil)))))))

(defun variable-p (thing)
  (and (symbolp thing)
       (equal #\? (aref (string thing) 0))))

(defun make-match (variable match)
  (cons variable match))
