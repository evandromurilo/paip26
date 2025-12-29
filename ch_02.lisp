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

(defvar *grammar* (make-hash-table))

(defun add-generator (name generator)
  "Add a new generator to the *grammar* table."
  (setf (gethash name *grammar*) generator))

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
	   (cond ((equal flag #\*) (zero-or-many (gethash what *grammar*)))
		 ((equal flag #\+) (one-or-many (gethash what *grammar*)))
		 (t (funcall (gethash what *grammar*))))))
	(t (append (generate (first what))
		   (generate (rest what))))))

;; follow up with the book, straightforward way

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
			     
  
;; adjust my generator to allow multiple grammars
(defun make-grammar ()
  (make-hash-table))

(defparameter *simple-grammar* (make-grammar))
(defvar *grammar* *simple-grammar*)

(defun add-rule (grammar rule)
  "Add a new rule to the grammar. The rule consists of a list like <name> -> words..., or <name> -> (rules...)."
  (let ((name (first rule))
	(rule (rest (rest rule))))
    (setf (gethash name grammar)
	  (if (listp (first rule))
	      (make-composite-generator (first rule))
	      (make-generator rule)))))

(defun add-rules (grammar rules)
  "Add a list of rules to the given grammar."
  (if (null rules)
      nil
      (progn
	(add-rule grammar (first rules))
	(add-rules grammar (rest rules)))))

(add-rules *simple-grammar*
	   '((verb -> hit took saw liked killed massaged hugged shutted)
	     (noun -> woman ball table man tree pencil gun bathtub)
	     (article -> the a)
	     (adj -> big little blue green huge funny smart crazy magnificent red)
	     (prep -> to in by with on over under)
	     (pp -> (prep noun-phrase))
	     (noun-phrase -> (article adj* noun pp*))
	     (verb-phrase -> (verb noun-phrase))
	     (sentence -> (noun-phrase verb-phrase))))

(defparameter *gramatica* (make-grammar))

(add-rules *gramatica*
	   '((verbo -> correr nadar andar amar)
	     (adv -> sempre intensamente)
	     (prep -> na)
	     (loc -> franca italia alemanha)
	     (oracao -> (verbo adv prep loc))))
	   

;; rule-based solution from book

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase) noun-phrase)
    (noun-phrase -> (Article Noun))
    (verb-phrase - (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *buggy-grammar*
  '((noun -> verb noun))
  "This grammar never generates 'noun.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*, but we can switch to other grammars.")


(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fun list)
  (apply #'append (mapcar fun list)))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
	 (mappend #'generate phrase))
	((rewrites phrase)
	 (generate (random-elt (rewrites phrase))))
	(t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase, with a complete parse tree."
  (cond ((listp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase)
	 (cons phrase (generate-tree (random-elt (rewrites phrase)))))
	(t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
	((listp phrase)
	 (combine-all (generate-all (first phrase))
		      (generate-all (rest phrase))))
	((rewrites phrase)
	 (mappend #'generate-all (rewrites phrase)))
	(t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))"
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (append x y)) xlist))
	   ylist))	 
	
;; exercises for section 2.3

(defun generate (phrase)
  "A version of generate that uses cond but avoids calling rewrites twice. (exercise 2.1)"
  (let ((choices nil))
    (cond ((listp phrase)
	   (mappend #'generate phrase))
	  ((setf choices (rewrites phrase))
	   (generate (random-elt choices)))
	  (t (list phrase)))))

(defun generate (phrase)
  "A version of generate that explicitly differentiates between terminal symbols (those with no rewrite rules) and nonterminal symbols. (exercise 2.2) (I find this approach really ugly)"
  (cond ((listp phrase)
	 (mappend #'generate phrase))
	((terminalp phrase)
	 (list phrase))
	(t (generate (random-elt (rewrites phrase))))))

(defun terminalp (term)
  "Check if term has no rewrites in the grammar, and is therefore terminal."
  (null (rewrites term)))

;; exercises for section 2.7

(defun cross-product (fun xlist ylist)
  "Apply fun to every combination of x and y. (exercise 2.4)"
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (funcall fun x y)) xlist))
	   ylist))

(defun combine-all (xlist ylist)
  "Version of combine-all in terms of cross-product. (exercise 2.4)"
  (cross-product #'append xlist ylist))
