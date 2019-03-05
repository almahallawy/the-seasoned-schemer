;;Chapter 18 - We Change, Therefore We are the Same

(setq lexical-binding t);; important to include this line

(fset 'kons 'cons)
(fset 'kar 'car)
(fset 'kdr 'cdr)

(defun lots (m)
  (cond
   ((zerop m) (quote ()))
   (t (kons (quote eggo)
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


(setq kounter ())
(setq set-counter ())

(setq konC
      (let ((N 0))
	(setq kounter
	      (lambda ()
		N))
	(setq set-counter
	      (lambda (x)
		(setq N x)))
	(lambda (x y)
	  (setq N (1+ N))
	  (kons x y))))








