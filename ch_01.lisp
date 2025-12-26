(defparameter *titles* '(jr md dr mr mrs miss ms madam admiral major general))

(defun last-name (name)
  (if (null name)
      nil
      (let ((last (last-name (rest name))))
	(if (null last)
	    (if (member (first name) *titles*)
		nil
		(first name))
	    last))))
