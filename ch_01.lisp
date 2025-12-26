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
	
  


