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

