(defpackage #:eliza
  (:use #:cl))

(in-package :eliza)

(defun pat-match (pat input)
  (cond ((variable-p pat)
	 t)
	((and (atom pat) (atom input))
	 (equal pat input))
	(t
	 (and (pat-match (car pat) (car input))
	      (pat-match (cdr pat) (cdr input))))))

(defun variable-p (thing)
  (and (atom thing)
       (equal #\? (aref (string thing) 0))))
