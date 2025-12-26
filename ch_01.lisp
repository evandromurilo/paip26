(defparameter *titles* '(jr md dr mr mrs miss ms madam admiral major general))

(defun last-name (name)
  "Select the last name from a name represented as a list. (exercise 1.1)"
  (if (null name)
      nil
      (let ((last (last-name (rest name))))
	(if (null last)
	    (if (member (first name) *titles*)
		nil
		(first name))
	    last))))

(defun power (x n)
  "Power raises x to the nth power. N must be an integer >= 0. (exercise 1.2)"
  (if (zerop x)
      1
      (* x (power x (- n 1)))))

(defun count-atoms (exp)
  "Count the total number of atoms in exp. Nil counts as an atom, but the empty list has zero atoms. (exercise 1.3)"
  (cond ((null exp) 0)
	((null (first exp)) (+ 1 (count-atoms (rest exp))))
	((listp (first exp)) (+ (count-atoms (first exp))
				(count-atoms (rest exp))))
	(t (+ 1 (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "(book solution) Return the total number of atoms in the expression, counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
	((atom exp) 1)
	(t (+ (count-all-atoms (first exp) 1)
	      (count-all-atoms (rest exp) 0)))))
	
(defun count-anywhere (sub exp &optional (if-null 0))
  "Count how many times sub occurs in exp. Handles searching for nil on non-tail position. (exercise 1.4)"
  (cond ((equal sub exp) (if (null sub) if-null 1))
	((atom exp) 0) ;; also picks empty exp
	(t (+ (count-anywhere sub (first exp) 1)
	      (count-anywhere sub (rest exp) 0)))))
	
(defun dot-product (&rest numbers)
  "Calculate the dot product of two sequences of numbers. The dot product is computed by multiplying each sequence and summing up the result. (dot-product '(10 20) '(3 4 5)) = (10 x 20) + (3 x 4 x 5). (exercise 1.5"
  (if (null numbers)
      0
      (+ (apply #'* (first numbers))
	 (apply #'dot-product (rest numbers)))))
  
