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

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
	    (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
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
	    
