;;Chapter 18 - We Change, Therefore We are the Same

(setq lexical-binding t);; important to include this line

;;set to nil to have no limit
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

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

lenkth

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

;; (defun add-at-end-too (l)
;;   (letrec
;;       ((A (lambda (ls)
;; 	    (cond
;; 	     ((null (kdr ls))
;; 	      (rplacd ls
;; 		      (kons (quote eggo)
;; 			    (quote ()))))
;; 	     (t (funcall A (kdr ls)))))))
;;     (funcall A l)
;;     l))

;;rplacd is an alias for ‘setcdr’ in ‘subr.el’.
(fset 'set-kdr 'setcdr)
(defun add-at-end-too (l)
  (letrec
      ((A (lambda (ls)
	    (cond
	     ((null (kdr ls))
	      (set-kdr ls
		      (kons (quote eggo)
			    (quote ()))))
	     (t (funcall A (kdr ls)))))))
    (funcall A l)
    l))

(funcall set-kounter 0)
(add-at-end-too (lots 3))
(funcall kounter)


;From Little Schemer -  Chapter 6
;Another representations for numbers
;Zero (), One (()), Two (() ()), Three (() () ())

;; (defun sero? (n)
;;   (null n))


;; (defun edd1 (n)
;;   (cons (quote()) n))

;; (defun zub1 (n)
;;   (cdr n))


;;Unbound previous definitions of kons, kar, and kdr.
(fmakunbound 'kons)
(fmakunbound 'kar)
(fmakunbound 'kdr)

(defun kons (kar kdr)
  (lambda (selector)
    (funcall selector kar kdr))) ;call selector with kar and kdr arguments

(defun kar (c)
  (funcall c (lambda (a d) a))) ;applies selector (lambda (a d) a) on (a d) and returns a (car)

(defun kdr (c)
  (funcall c (lambda (a d) d)))  ;applies selector (lambda (a d) d) on (a d) and returns d (cdr)

(kons 'a '())
(kar (kons 'a '()))
(kdr (kons 'a '()))
(kar (kons 'a (kons 'b '())))

(defun bons (kar)
  (let ((kdr (quote ())))
    (lambda (selector)
      (funcall selector
	       (lambda (x) (setq kdr x))
	       kar
	       kdr))))

(fmakunbound 'kar)
(fmakunbound 'kdr)

(defun kar (c)
  (funcall c (lambda (s a d) a)))

(defun kdr (c)
  (funcall c (lambda (s a d) d)))

(defun set-kdr (c x)
  (funcall (funcall c (lambda (s a d) s)) x))

(bons (quote egg))
(kar (bons (quote egg)))
(kdr (bons (quote egg)))
(set-kdr (bons (quote eggg)) '(egoo))


;;Use set-kdr and bons to define kons
(fmakunbound 'kons)

(defun kons (a d)
  (let ((c (bons a)))
    (set-kdr c d)
    c))

(kons 'a '(b c d))
(kar (kons 'a '(b c d)))
(kdr (kons 'a '(b c d)))

;;------

(fmakunbound 'kons)
(fmakunbound 'kar)
(fmakunbound 'kdr)

(fset 'kons 'cons)
(fset 'kar 'car)
(fset 'kdr 'cdr)


(setq dozen (lots 12))

(funcall set-kounter 0)
(setq bakers-dozen (add-at-end dozen))
(funcall kounter)

(setq bakers-dozen-too (add-at-end-too dozen))
dozen

(setq bakers-dozen-again (add-at-end dozen))
(funcall kounter)

(defun eklist? (ls1 ls2)
  (cond
   ((null ls1) (null ls2))
   ((null ls2) nil)
   (t (and (eq (kar ls1) (kar ls2))
	   (eklist? (kdr ls1) (kdr ls2))))))

(eklist? bakers-dozen bakers-dozen-too)

(defun same? (c1 c2)
  (let ((t1 (kdr c1))
	(t2 (kdr c2)))
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ((v (= (kdr c1) (kdr c2))))
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))

(same? dozen dozen)
(same? dozen bakers-dozen-too)
(same? bakers-dozen bakers-dozen-too)


(same? (kons (quote egg) (quote ()))
       (kons (quote egg) (quote ())))


(defun last-kons (ls)
  (cond
   ((null (kdr ls)) ls)
   (t (last-kons (kdr ls)))))

(setq long (lots 12))
(lenkth long)
(set-kdr (last-kons long) long)

(lenkth long)

(setq long (lots 12))
(set-kdr (last-kons long) (kdr (kdr long)))
(lenkth long)


(defun finite-lenkth (p)
  (catch 'infinite
    (letrec
	((C (lambda (p q)
	      (print p)
	      (print q)
	      (print '===)
	      (cond
	       ((null q) 0)
	       ((null (kdr q)) 1)
	       ((same? p q)
		(throw 'infinite nil))
	       (t (+ (funcall C (funcall sl p) (funcall qk q))
		     2)))))
	 (qk (lambda (x) (kdr (kdr x))))
	 (sl (lambda (x) (kdr x))))
      (cond
       ((null p) 0)
       (t (1+ (funcall C p (kdr p))))))))

(setq lk '(1 2 3 4 5))
lk
(set-kdr (last-kons lk) lk)
lk
(finite-lenkth lk)


(setq ll '(1 2 3))
(finite-lenkth ll)

;; (setq lk '(1 2 3))
;; (set-kdr (last-kons lk) lk)

;; Guy's Favorite Pie
(setq mongo
  (kons (quote pie)
        (kons (quote a)
              (kons (quote la)
                    (kons (quote mode)
                          (quote ()))))))

mongo

(set-kdr (kdr (kdr (kdr mongo))) (kdr mongo))

mongo
