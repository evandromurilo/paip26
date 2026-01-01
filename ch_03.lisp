;; (exercise 3.1)
(equal
 ((lambda (x) ((lambda (y) (+ x y)) (* x x))) 6)
 (let* ((x 6)
	(y (* x x)))
   (+ x y)))
