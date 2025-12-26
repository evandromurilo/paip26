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


