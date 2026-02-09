;; my attempt of building the GPS after the initial specification

(defstruct operator
  action
  preconditions
  add-list ; the effect is to add these conditions
  delete-list) ;; the effect is to delete these conditions

(defparameter *operators*
  (list
   (make-operator :action 'make-cookies
		  :preconditions '(has-ingredients has-utensils knows-how-to-cook at-home)
		  :add-list '(has-cookies)
		  :delete-list '(has-ingredients))
   (make-operator :action 'buy-cookies
		  :preconditions '(has-money at-supermarket)
		  :add-list '(has-cookies)
		  :delete-list '(has-money))
   (make-operator :action 'buy-ingredients
		  :preconditions '(has-money at-supermarket)
		  :add-list '(has-ingredients)
		  :delete-list '(has-money))
   (make-operator :action 'learn-to-cook
		  :preconditions '(has-cooking-book has-time)
		  :add-list '(knows-how-to-cook)
		  :delete-list nil)
   (make-operator :action '(buy-cooking-book)
		  :preconditions '(at-bookstore has-money)
		  :add-list '(has-cooking-book)
		  :delete-list '(has-money))
   (make-operator :action 'take-cooking-book-lending
		  :preconditions '(at-library)
		  :add-list '(has-cooking-book)
		  :delete-list nil)
   (make-operator :action 'drive-to-supermarket
		  :preconditions '(has-car)
		  :add-list '(at-supermarket)
		  :delete-list '(at-home at-bookstore at-library))
   (make-operator :action 'drive-to-library
		  :preconditions '(has-car)
		  :add-list '(at-library)
		  :delete-list '(at-home at-supermarket at-bookstore))
   (make-operator :action 'drive-to-bookstore
		  :preconditions '(has-car)
		  :add-list '(at-bookstore)
		  :delete-list '(at-home at-supermarket at-library))
   (make-operator :action 'drive-home
		  :preconditions '(has-car)
		  :add-list '(at-home)
		  :delete-list '(at-supermarket at-library at-bookstore))))

(defun gps (state goals path)
  "Find a path of actions what will lead to the satisfaction of all goals."
  (let ((remaining-goals (set-difference goals state)))
    (cond ((null remaining-goals) (values t state path))
	  (t (multiple-value-bind (is-fullfilled new-state added-path)
		 (satisfy-goal state (first remaining-goals) *operators*)
	       (if is-fullfilled
		   (gps new-state goals (append path added-path))
		   (values nil nil nil)))))))

(defun satisfy-goal (state goal operators)
  "Find a path of actions that will lead to the satisfaction of the goal."
  (if (null operators)
      (values nil nil nil)
      (let ((op (first operators)))
	(if (member goal (operator-add-list op))
	    (multiple-value-bind (is-fullfilled new-state added-path)
		(gps state (operator-preconditions op) nil)
	      (if is-fullfilled
		  (values t (union (operator-add-list op) (set-difference new-state (operator-delete-list op))) (append added-path (list (operator-action op))))
		  (satisfy-goal state goal (rest operators))))
	    (satisfy-goal state goal (rest operators))))))

  
	   
;; here is the book gps

(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
	   (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
	    (find-all goal *ops* :test #'appropriate-p))))

(defun achieve-all (goals)
  "Try to achieve each goal, then make sure they still hold."
  (and (every #'achieve goals) (subset goals *state*)))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (achieve-all (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))
	    
(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbg-on (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun dbg-off (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
		      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging iinfo if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

;; here is the second version of gps on the book

(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds
	    :add-list add-list :del-list del-list)))

(defun operator-to-op (operator)
  "Converts from the operator to the op struct."
  (op (operator-action operator)
      :preconds (operator-preconditions operator)
      :add-list (operator-add-list operator)
      :del-list (operator-delete-list operator)))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (find-all-if #'action-p
	       (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
			(setf current-state
			      (achieve current-state g goal-stack)))
		    goals)
	     (subsetp goals current-state :test #'equal))
	current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
	((member-equal goal goal-stack) nil)
	(t (some #'(lambda (op) (apply-op state goal op goal-stack))
		 (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators,
   sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
	:key #'(lambda (op)
		 (count-if #'(lambda (precond)
			       (not (member-equal precond state)))
			   (op-preconds op)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
			     (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
			     (member-equal x (op-del-list op)))
			 state2)
	      (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  "Use oplist as the default list of operators."
  (length (setf *ops* oplist)))

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

(defun mappend (fun list)
  (apply #'append (mapcar fun list)))

;; gps applied to maze domain

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
	(make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
	   '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
	     (12 11) (11 6) (11 16) (16 17) (17 22) (22 21) (22 23)
	     (23 18) (23 24) (24 19) (19 20) (20 25) (20 15) (15 10)
	     (10 5))))

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
			  (remove '(start) results
				  :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))

;; gps applied to blocks world

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
	(unless (equal a b)
	  (dolist (c blocks)
	    (unless (or (equal c a) (equal c b))
	      (push (move-op a b c) ops))) ;; from one block to another
	  (push (move-op a 'table b) ops) ;; from the table to a block
	  (push (move-op a b 'table) ops))))
    ops)) ;; from a block to the table

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))
	  
;; end-of-chapter exercies

(defun dbg (id format-string &rest args)
  "Dbg but implemented with a single call to format. (exercise 4.1)"
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~?" format-string args)))

(defun map-in-context (function list &optional (before '()))
  "Map through function with each element in list, along with the elements that came before and after it."
  (if (null list)
      nil
      (cons (funcall function (first list) before (rest list))
	    (map-in-context function
			    (rest list)
			    (append before (list (first list)))))))

(defun tree (element children)
  "Builds a tree represented as a cons cell."
  (cons element children))

(defun children (tree)
  "The children of a tree represented as a cons cell."
  (rest tree))

(defun element (tree)
  "The element value of the root of a tree represented as a cons cell."
  (first tree))

(defun permutations (list)
  "A list of all permutations of the elements of list. (exercise 4.2)"
  (let ((trees (map-in-context (lambda (element before after)
				 (permutation-tree element (append before after)))
			       list)))
    (forest-paths trees)))

(defun permutation-tree (element list)
  "Builds a permutation tree of list, with element as the root."
  (tree element (map-in-context (lambda (element before after)
				  (permutation-tree element (append before after)))
				list)))
(defun tree-paths (tree)
  "A list for each path through tree (from root to leaf)."
  (if (null tree)
      nil
      (if (null (children tree))
	  (list (list (element tree)))
	  (mapcar (lambda (subtree) (cons (element tree) subtree)) (forest-paths (children tree))))))
	  
(defun forest-paths (trees)
  "A list for each path through the forest (from root to leaf)."
  (if (null trees)
      nil
      (append (tree-paths (first trees))
	      (forest-paths (rest trees)))))

(defparameter *dessert-ops*
  (list
   (op 'eating-ice-cream
       :preconds '(has-ice-cream)
       :add-list '(ate-ice-cream ate-dessert)
       :del-list '(has-ice-cream))
   (op 'eating-cake
       :preconds '(has-cake)
       :add-list '(ate-cake ate-dessert)
       :del-list '(has-cake))
   (op 'buying-cake
       :preconds '(at-bakery has-money)
       :add-list '(has-cake ice-cream-coupon)
       :del-list '(has-money))
   (op 'bakery-ice-cream-deal
       :preconds '(at-bakery ice-cream-coupon ate-cake)
       :add-list '(has-ice-cream)
       :del-list '(ice-cream-coupon)))
  "Exercise 4.3, with eat-dessert goal GPS will eat the cake and them still eat the ice cream.")


;; new clos solver
(defparameter *dessert-ops*
  (list
   (make-op :action 'eating-ice-cream
       :preconds '(has-ice-cream)
       :add-list '(ate-ice-cream ate-dessert)
       :del-list '(has-ice-cream))
   (make-op :action 'eating-cake
       :preconds '(has-cake)
       :add-list '(ate-cake ate-dessert)
       :del-list '(has-cake))
   (make-op :action 'buying-cake
       :preconds '(at-bakery has-money)
       :add-list '(has-cake ice-cream-coupon)
       :del-list '(has-money))
   (make-op :action 'bakery-ice-cream-deal
       :preconds '(at-bakery ice-cream-coupon ate-cake)
       :add-list '(has-ice-cream)
       :del-list '(ice-cream-coupon))))


(defstruct op "An operation"
	   (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun appropriate-ops (goal state ops)
  "Return a list of appropriate operators,
   sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal ops :test #'appropriate-p)) #'<
	:key #'(lambda (op)
		 (count-if #'(lambda (precond)
			       (not (member-equal precond state)))
			   (op-preconds op)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defclass solver ()
  ((operations :accessor operations :initarg :operations)
   (working-for :accessor working-for :initform nil :initarg :parent)
   (workers :accessor workers :initform nil)
   (initial-state :accessor initial-state :initarg :initial-state)
   (working-state :accessor state :initarg :state)
   (working-op :accessor working-op :initform nil)
   (appropriate-ops :accessor options :initarg :ops)
   (goal :accessor goal :initarg :goal)))

(defun is-solved (solver)
  "Check if the solver is solved with its own state."
  (member (goal solver) (state solver)))

(defun is-solved-with (solver state)
  "Check if the solver is solved with the given state."
  (member (goal solver) state))

(defun highest-solved-with (reportee state)
  "Go up the chain and see if anyone is solved by the given state. This is to short-circuit solutions."
  (if (null reportee)
      nil
      (if (is-solved-with reportee state)
	  (progn
	    (setf (state reportee) state)
	    (or (highest-solved-with (working-for reportee) state)
		reportee))
	  (highest-solved-with (working-for reportee) state))))

(defun report-result (reportee)
  "Report result up the chain, find next solver to work. The next solver is not necessarily done."
  (let ((solver (working-for reportee)))
    (if (null solver) ;; top of chain, final result
	reportee
	(if (not (is-solved reportee))
	    (progn
	      (setf (workers solver) nil)
	      (work solver))
	    (progn
	      (setf (state solver) (state reportee))
	      (or (highest-solved-with solver (state reportee)) (work solver)))))))

(defun op-satisfied (solver)
  "Check if the solver current operation is solved by its state."
  (and (working-op solver)
       (null (set-difference (op-preconds (working-op solver)) (state solver)))))

(defun apply-op-to-solver (op solver)
  "Apply given operation to solver state."
  (setf (state solver)
	(append (remove-if #'(lambda (x)
			     (member-equal x (op-del-list op)))
			 (state solver))
		(op-add-list op))))

(defun make-subsolver (solver goal)
  "Make a new solver for a new goal based on the parent solver state."
  (make-instance 'solver
		 :goal goal
		 :parent solver
		 :state (copy-list (state solver))
		 :initial-state (copy-list (state solver))
		 :operations (operations solver)
		 :ops (appropriate-ops goal (state solver) (operations solver))))

(defun make-solver (goal state operations)
  "Make a new root solver."
  (make-instance 'solver
		 :goal goal
		 :parent nil
		 :initial-state state
		 :state (copy-list state)
		 :operations operations
		 :ops (appropriate-ops goal state operations)))

(defun work (solver)
  "Work on the solver goal."
  (cond ((is-solved solver)
	 (report-result solver))
	((op-satisfied solver)
	 (apply-op-to-solver (working-op solver) solver)
	 (report-result solver))
	((and (null (options solver)) (null (workers solver)))
	 'failure)
	((not (null (workers solver)))
	 (work (pop (workers solver))))
	(t (setf (state solver) (initial-state solver))
	   (let* ((op (pop (options solver)))
		  (missing-pieces (set-difference (op-preconds op) (state solver))))
	     (if (null missing-pieces)
		 (progn
		   (apply-op-to-solver op solver)
		   (report-result solver))
		 (progn
		   (setf (working-op solver) op)
		   (setf (workers solver) (mapcar (lambda (goal)
						    (make-subsolver solver goal))
						  missing-pieces))
		   (work solver)))))))
	     
      
      
      
