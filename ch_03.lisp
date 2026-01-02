;; (exercise 3.1)
(equal
 ((lambda (x) ((lambda (y) (+ x y)) (* x x))) 6)
 (let* ((x 6)
	(y (* x x)))
   (+ x y)))

(defun print-dotted (exp)
  "Print an expression in dotted pair notation. (exercise 3.3)"
  (if (consp exp)
      (progn
	(princ "(")
	(print-dotted (car exp))
	(princ " . ")
	(print-dotted (cdr exp))
	(princ ")"))
      (princ exp))
  nil)

(defun print-list (exp &optional (is-head t))
  "Print an expression in dotthed pair notation when necessary, but otherwise use normal list notation. (exercise 3.4)"
  (if (consp exp)
      (progn
	(when is-head
	  (princ "("))
	(print-list (car exp))
	(cond ((null (cdr exp))
	       (princ ")"))
	      ((listp (cdr exp))
	       (princ " ")
	       (print-list (cdr exp) nil))
	      (t
	       (princ " . ")
	       (princ (cdr exp))
	       (princ ")"))))
      (princ exp)))
