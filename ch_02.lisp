;; my take on the context-free grammar before reading the chapter

(defparameter *verbs* '(hit took saw liked killed massaged hugged shutted))
(defparameter *nouns* '(woman ball table man tree pencil gun bathtub))
(defparameter *adjectives* '(big little blue green huge funny smart crazy magnificent red))
(defparameter *prepositions* '(to in by with on over under))
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
(add-generator 'adj (make-generator *adjectives*))
(add-generator 'prep (make-generator *prepositions*))
(add-generator 'pp (make-composite-generator '(prep noun-phrase)))
(add-generator 'noun-phrase (make-composite-generator '(article adj* noun pp*)))
(add-generator 'verb-phrase (make-composite-generator '(verb noun-phrase)))
(add-generator 'sentence (make-composite-generator '(noun-phrase verb-phrase)))

(defun butlast-str (what)
  "Returns the string separated of the last character."
  (values (subseq what 0 (- (length what) 1))
	  (char what (- (length what) 1))))

(defun parse-gen-name (what)
  "Parse a generator name to see if there are any flags, like * (zero-or-many) or + (one-or-many)."
  (multiple-value-bind (start end) (butlast-str (string what))
    (if (or (equal end #\*) (equal end #\+))
	(values (intern start) end)
	(values what nil))))

(defun zero-or-many (fun)
  "Half the time returns nil, the other half returns the result of calling the function one-or-many times. Function must return a list."
  (if (= (random 2) 0)
      nil
      (one-or-many fun)))

(defun one-or-many (fun)
  "Returns a list with the value returned by function, one or more times. Function must return a list."
  (append (funcall fun)
	  (zero-or-many fun)))
	       
(defun generate (what)
  "Generate a sentence based on a list or symbol."
  (cond ((null what) nil)
	((atom what)
	 (multiple-value-bind (what flag) (parse-gen-name what)
	   (cond ((equal flag #\*) (zero-or-many (gethash what *generators*)))
		 ((equal flag #\+) (one-or-many (gethash what *generators*)))
		 (t (funcall (gethash what *generators*))))))
	(t (append (generate (first what))
		   (generate (rest what))))))

;; follow up with the book

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
			     
  
