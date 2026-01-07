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

(defun decompress (bitset vocab)
  "Decompress a bit sequence according to a reference set. E.g.
   > (decompress #*11001 #(a b c d e))
   (a b e)"
  (let ((res nil)
	(len (length bitset)))
    (dotimes (iteration len)
      (let ((index (- len iteration 1)))
	(when (= 1 (aref bitset index))
	  (push (aref vocab index) res))))
    res))

(defun decompress (bitset vocab)
  "Decompress implemented with a do loop"
  (let ((res nil))
    (do ((index (- (length bitset) 1) (decf index)))
	((< index 0))
      (when (= 1 (aref bitset index))
	(push (aref vocab index) res)))
    res))

(defun reversetimes (count fun)
  "Call function with argument n, where n goes from count-1 to 0."
  (dotimes (index count)
    (funcall fun (- count index 1))))

(defun decompress (bitset vocab)
  "Decompress implemented with reversetimes"
  (let ((res nil))
    (reversetimes (length bitset)
		  #'(lambda (index)
		      (when (= (aref bitset index) 1)
			(push (aref vocab index) res))))
    res))

(defun compress (the-set vocab)
  "Compress the set into a bit sequence according to a reference set. E.g.
   > (compress '(a b d) #(a b c d e))
   #*11010"
  (let ((bitseq (make-array (length vocab) :element-type 'bit)))
    (dotimes (index (length vocab))
      (if (member (aref vocab index) the-set)
	  (setf (aref bitseq index) 1)
	  (setf (aref bitseq index) 0)))
    bitseq))
		  

;; here begins 20questions
(defstruct question text answers)
(defvar questions (list (make-question :text "Is it an animal?")))
(defvar possible-guesses '(bee))

(defun ask-question (&optional (yes-questions nil) (question-base questions) (guess-base possible-guesses) (remaining-questions 20))
  (cond ((= 0 (+ (length question-base)
		 (length guess-base)))
	 (princ "I give up. What is it? ")
	 (let ((answer (read)))
	   (dolist (qst yes-questions) (push answer (question-answers qst))))
	 yes-questions)
	((= 0 remaining-questions)
	 (princ "I lost. What is it? ")
	 (read)
	 yes-questions)
	((> (length question-base) 0)
	 (princ (question-text (car question-base)))
	 (princ " ")
	 (when (equal 'yes (read))
	   (push (car question-base) yes-questions))
	 (ask-question yes-questions
		       (cdr question-base)
		       guess-base
		       (- remaining-questions 1)))
	((> (length guess-base) 0)
	 (princ "Is it ")
	 (princ (car guess-base))
	 (princ "? ")
	 (if (equal 'it (read))
	     (princ "Nice! ")
	     (progn
	       (princ "Bummer! ")
	       (ask-question yes-questions
			     question-base
			     (cdr guess-base)
			     (- remaining-questions 1)))))))

