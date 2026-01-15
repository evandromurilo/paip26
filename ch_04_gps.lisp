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

;; todo backtraking
(defun gps (state goals path)
  (let ((remaining-goals (set-difference goals state)))
    (cond ((null remaining-goals) (values t state path))
	  (t (multiple-value-bind (is-fullfilled new-state added-path)
		 (satisfy-goal state (first remaining-goals) *operators*)
	       (if is-fullfilled
		   (gps new-state goals (append added-path path))
		   (values nil nil nil)))))))

(defun satisfy-goal (state goal operators)
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

  
	   
