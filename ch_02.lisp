;; my take on the context-free grammar before reading the chapter

(defparameter *verbs* '(hit took saw liked killed massaged hugged shutted))
(defparameter *nouns* '(woman ball table man tree pencil gun bathtub))
(defparameter *articles* '(the a))

(defun make-generator (wordlist)
  "Make a generator that returns a random symbol from wordlist as a single-element list."
  (let ((len (length wordlist)))
    (lambda () (list (nth (random len) wordlist)))))

(defun make-composite-generator (what)
  "Make a composite generator, like '(article noun)."
  (lambda () (generate what)))

(defvar *generators* (make-hash-table))

(defun add-generator (name generator)
  "Add a new generator to the *generators* table."
  (setf (gethash name *generators*) generator))

(add-generator 'verb (make-generator *verbs*))
(add-generator 'noun (make-generator *nouns*))
(add-generator 'article (make-generator *articles*))
(add-generator 'noun-phrase (make-composite-generator '(article noun)))
(add-generator 'verb-phrase (make-composite-generator '(verb noun-phrase)))
(add-generator 'sentence (make-composite-generator '(noun-phrase verb-phrase)))
	       
(defun generate (what)
  "Generate a sentence based on a list or symbol."
  (cond ((null what) nil)
	((atom what) (funcall (gethash what *generators*)))
	(t (append (generate (first what))
		   (generate (rest what))))))
			     
  
