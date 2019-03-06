;;Chapter 18 - We Change, Therefore We are the Same

(setq lexical-binding t);; important to include this line

(fset 'kons 'cons)
(fset 'kar 'car)
(fset 'kdr 'cdr)

(defun lots (m)
  (cond
   ((zerop m) (quote ()))
   (t (kons (quote egg)
	    (lots (1- m))))))

(lots 3)
(lots 5)

(defun lenkth (l)
  (cond
   ((null l) 0)
   (t (1+ (lenkth (kdr l))))))

(lenkth (lots 3))
(lenkth (lots 5))
(lenkth (lots 12))

;;Create a list of four egges from (lots 3)
(kons (quote egg) (lots 3))


(setq kounter ())
(setq set-kounter ())

(setq konsC
      (let ((N 0))
	(setq kounter
	      (lambda ()
		N))
	(setq set-kounter
	      (lambda (x)
		(setq N x)))
	(lambda (x y)
	  (setq N (1+ N))
	  (kons x y))))

(defun add-at-end (l)
  (cond
   ((null (kdr l))
    (funcall konsC (kar l)
	     (kons (quote eggo)
		   (quote ()))))
   (t (funcall konsC (kar l)
	    (add-at-end (kdr l))))))

(funcall set-kounter 0)
(add-at-end (lots 3))
(funcall kounter)


(defun add-at-end-too (l)
  (letrec
      ((A (lambda (ls)
	    (cond
	     ((null (kdr ls))
	      (rplacd ls
		      (kons (quote eggo)
			    (quote ()))))
	     (t (funcall A (kdr ls)))))))
    (funcall A l)
    l))

(funcall set-kounter 0)
(add-at-end-too (lots 3))
(funcall kounter)






















    
