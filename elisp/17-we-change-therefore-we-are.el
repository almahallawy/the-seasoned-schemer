;;Chapter 17-  We Change, Therefore We Are!

(setq lexical-binding t);; important to include this line

;;set to nil to have no limit
(setq eval-expression-print-level nil)

(defun deep (m)
  (if (zerop m)
      (quote pizza)
    (cons (deep (1- m))
	  (quote ()))))

(deep 3)
(deep 7)


(defun atom? (x)
  (not (listp x)))

(atom? nil);;nil not atom?!

(defun find (n Ns Rs)
  (letrec ((A (lambda (ns rs)
		(cond
		 ((null ns) nil)
		 ((= (car ns) n) (car rs))
		 (t (funcall A (cdr ns) (cdr rs)))))))
    (funcall A Ns Rs)))

(setq RR)

(defun deepM () ;;This from is umimaginable, Check pg 100 & 7th commandment
  (let ((Rs (quote ())) ;;Using defun doesn't keep the value of imaginary Rs and Ns between calls
	(Ns (quote ())))
    (setq RR
	  (lambda ()
	    Rs))
    (letrec
	((D (lambda (m)
	      (if (zerop m)
		  (quote pizza)
		(cons (funcall D (1- m))
		      (quote ()))))))
      (lambda (n)
	(let ((exists (find n Ns Rs)))
	  (if (null exists)
	      (let ((result (funcall D n)))
		(print Rs)
		(setq Rs (cons result Rs))
		(print Ns)
		(setq Ns (cons n Ns))
		result)
	    exists))))))

(funcall (deepM) 5)

(funcall (deepM) 4)

(funcall (deepM) 10)


(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(letrec
	    ((D (lambda (m)
		  (if (zerop m)
		      (quote pizza)
		    (cons (funcall D (1- m))
			  (quote ()))))))
	  (lambda (n)
	    (let ((exists (find n Ns Rs)))
	      (if (null exists)
		  (let ((result (funcall D n)))
		    (setq Rs (cons result Rs))
		    (print Rs)
		    (setq Ns (cons n Ns))
		    (print Ns)
		    result)
		exists))))))

(funcall deepM 2)
(funcall deepM 5)

(funcall deepM 2)
(funcall deepM 5)

;;Help D with its work, D should refer to deepM instead of itself
(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(letrec
	    ((D (lambda (m)
		  (if (zerop m)
		      (quote pizza)
		    (cons (funcall deepM (1- m))
			  (quote ()))))))
	  (lambda (n)
	    (let ((exists (find n Ns Rs)))
	      (if (null exists)
		  (let ((result (funcall D n)))
		    (setq Rs (cons result Rs))
		    (print Rs)
		    (setq Ns (cons n Ns))
		    (print Ns)
		    result)
		exists))))))

(funcall deepM 5)
(funcall deepM 4)
(funcall deepM 7)


;;(letrec ..) is no longer needed because D  is not longer mentioned in the definition of D
(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(let
	    ((D (lambda (m)
		  (if (zerop m)
		      (quote pizza)
		    (cons (funcall deepM (1- m))
			  (quote ()))))))
	  (lambda (n)
	    (let ((exists (find n Ns Rs)))
	      (if (null exists)
		  (let ((result (funcall D n)))
		    (setq Rs (cons result Rs))
		    (print Rs)
		    (setq Ns (cons n Ns))
		    (print Ns)
		    result)
		exists))))))

(funcall deepM 4)
(funcall deepM 3)

;;Only one let is needed because Ns and Rs do not appear in the definition of D
(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ()))
	    (D (lambda (m)
		 (if (zerop m)
		     (quote pizza)
		   (cons (funcall deepM (1- m))
			 (quote ()))))))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (null exists)
		(let ((result (funcall D n)))
		  (setq Rs (cons result Rs))
		  (print Rs)
		  (setq Ns (cons n Ns))
		  (print Ns)
		  result)
	      exists)))))

(funcall deepM 5)
(funcall deepM 3)
(funcall deepM 6)

;;replace the one use of D by the experssion it names

(makunbound 'deepM)

(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (null exists)
		(let ((result
		       ((lambda (m)
			  (if (zerop m)
			      (quote pizza)
			    (cons (funcall deepM (1- m))
				  (quote ()))))
			n)))
		  (setq Rs (cons result Rs))
		  (print Rs)
		  (setq Ns (cons n Ns))
		  (print Ns)
		  result)
	      exists)))))

(funcall deepM 5)
(funcall deepM 3)
(funcall deepM 6)

;;introduce a name to name another name

(makunbound 'deepM)

(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (null exists)
		(let ((result
		       (let ((m n))
			  (if (zerop m)
			      (quote pizza)
			    (cons (funcall deepM (1- m))
				  (quote ()))))))
		  (setq Rs (cons result Rs))
		  (print Rs)
		  (setq Ns (cons n Ns))
		  (print Ns)
		  result)
	      exists)))))


(funcall deepM 5)
(funcall deepM 3)
(funcall deepM 6)


;;uname again because a name is replaced by a name

(makunbound 'deepM)

(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (null exists)
		(let ((result
		       (if (zerop n)
			   (quote pizza)
			 (cons (funcall deepM (1- n))
			       (quote ())))))
		  (setq Rs (cons result Rs))
		  (print Rs)
		  (setq Ns (cons n Ns))
		  (print Ns)
		  result)
	      exists)))))


(funcall deepM 5)
(funcall deepM 3)
(funcall deepM 6)


(setq consC
      (let ((N 0))
	(lambda (x y)
	  (setq N (1+ N))
	  (cons x y))))

;;use consC in deep

(makunbound 'deep)

(defun deep (m)
  (if (zerop m)
      (quote pizza)
    (funcall consC (deep (1- m))
	     (quote ()))))


(deep 5)
(deep 3)

;;How to read the imaginary N
(setq counter ())

(setq consC
      (let ((N 0))
	(setq counter
	      (lambda ()
		N))
	(lambda (x y)
	  (setq N (1+ N))
	  (cons x y))))

(deep 5)
(funcall counter)

(deep 7)
(funcall counter)

;;Set the following in order to be able to call (deep 1000) and (supercounter deep) without error
max-lisp-eval-depth
(setq max-lisp-eval-depth 6000)

max-specpdl-size
(setq max-specpdl-size 6000)


print-circle
(setq print-circle 6000)

(deep 1000)

(defun supercounter (f)
  (letrec
      ((S (lambda (n)
	    (if (zerop n)
		(funcall f n)
	      (let ()
		(funcall f n)
		(funcall S (1- n)))))))
    (funcall S 1000)
    (funcall counter)))

(supercounter (function deep))

;;Set the value of the imaginary name
(setq counter ())

(setq set-counter ())

(setq consC
      (let ((N 0))
	(setq counter
	      (lambda ()
		N))
	(setq set-counter
	      (lambda (x)
		(setq N x)))
	(lambda (x y)
	  (setq N (1+ N))
	  (cons x y))))

(funcall set-counter 0)

(supercounter (function deep))

(funcall counter)

;;How many conses are used for (deepM 5)
(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (null exists)
		(let ((result
		       (if (zerop n)
			   (quote pizza)
			 (funcall consC (funcall deepM (1- n))
			       (quote ())))))
		  (setq Rs (cons result Rs))
		  (setq Ns (cons n Ns))
		  result)
	      exists)))))

(funcall counter)

(funcall deepM 5)
(funcall counter)

(funcall set-counter 0)
;;re-evaluate (setq deepM..) to clear Rs & Ns
(funcall deepM 5)
(funcall counter)

(funcall deepM 7)
(funcall counter);; counter should be 7, not 12, because that is point of DeepM


;; inorder to call (supercounter deepM without error)
(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)

(supercounter deepM)
(funcall counter)

(defun rember1* (a l)
  (letrec
      ((R (lambda (l oh)
	    (cond
	     ((null l)
	      (throw oh (quote no)))
	     ((atom? (car l))
	      (if (eq (car l) a)
		  (cdr l)
		(cons (car l)
		      (funcall R (cdr l) oh))))
	     (t (let
		    ((new-car
		      (catch 'oh
			(funcall R (car l) 'oh))))
		  (if (atom? new-car)
		      (cons (car l)
			    (funcall R (cdr l) oh))
		    (cons new-car (cdr l)))))))))
    (let ((new-l
	   (catch 'oh (funcall R l 'oh))))
      (if (atom? new-l)
	  l
	new-l))))

(rember1* 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1* 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 


;;Using counting version of cons
(defun rember1*C (a l)
  (letrec
      ((R (lambda (l oh)
	    (cond
	     ((null l)
	      (throw oh (quote no)))
	     ((atom? (car l))
	      (if (eq (car l) a)
		  (cdr l)
		(funcall consC (car l)
		      (funcall R (cdr l) oh))))
	     (t (let
		    ((new-car
		      (catch 'oh
			(funcall R (car l) 'oh))))
		  (if (atom? new-car)
		      (funcall consC (car l)
			    (funcall R (cdr l) oh))
		    (funcall consC new-car (cdr l)))))))))
    (let ((new-l
	   (catch 'oh (funcall R l 'oh))))
      (if (atom? new-l)
	  l
	new-l))))

(funcall set-counter 0)

(rember1*C 'noodles '((food) more (food)))
;;=0, because we never used consC, We always used the compass needle and the North Pole
;;to get rid of pending consCes.
(funcall counter)

(rember1*C 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))
(funcall counter)

(defun eqlist? (l1 l2)
  (cond
   ((and (null l1) (null l2)) t)
   ((and (null l1) (atom? (car l2))) nil)
   ((null l1) nil)
   ((and (atom? (car l1)) (atom? (car l2)))
    (and (eq (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2))))
   ((atom? (car l1)) nil)
   ((null l2) nil)
   ((atom? (car l2)) nil)
   (t (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))))

;;Version that failed by repeatedly checking whether anything had changed
;;for the car of a list that was a list:
(defun rember1* (a l)
  (letrec
      ((R (lambda (l)
	    (cond
	     ((null l) (quote ()))
	     ((atom? (car l))
	      (if (eq (car l) a)
		  (cdr l)
		(cons (car l)
		      (funcall R (cdr l)))))
	     (t
	      (let ((av (funcall R (car l))))
		(if (eqlist? (car l) av)
		    (cons (car l)
			  (funcall R (cdr l)))
		  (cons av (cdr l)))))))))
    (funcall R l)))


(rember1* 'noodles '((food) more (food)))

(rember1* 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1* 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 

;;Using consC
(defun rember1*C1 (a l)
  (letrec
      ((R (lambda (l)
	    (cond
	     ((null l) (quote ()))
	     ((atom? (car l))
	      (if (eq (car l) a)
		  (cdr l)
		(funcall consC (car l)
		      (funcall R (cdr l)))))
	     (t
	      (let ((av (funcall R (car l))))
		(if (eqlist? (car l) av)
		    (funcall consC (car l)
			  (funcall R (cdr l)))
		  (funcall consC av (cdr l)))))))))
    (funcall R l)))

(funcall set-counter 0)
(funcall consC (funcall consC 'food (quote ()))
	 (funcall consC 'more
		  (funcall consC (funcall consC 'food (quote ()))
			   (quote ()))))
(funcall counter)

(funcall set-counter 0)
(rember1*C1 'noodles '((food) more (food)))
(funcall counter)

(funcall set-counter 0)
(rember1*C 'noodles '((food) more (food)))
(funcall counter)
