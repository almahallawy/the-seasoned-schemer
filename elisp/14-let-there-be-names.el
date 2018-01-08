;;14. Let There Be Names

(setq lexical-binding t);; important to include this line

(defun atom? (x)
  (not (listp x)))

(defun leftmost-1 (l)
  (cond
   ((atom? (car l)) (car l))
   (t (leftmost-1 (car l)))))

(leftmost-1 '(((a) b) (c d)))
(leftmost-1 '(((a) ()) () (e)))
(leftmost-1 '(((() a) ()))) ;;No answer


(defun leftmost-2 (l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l)) (car l))
   (t (cond
       ((atom? (leftmost-2 (car l)))
	(leftmost-2 (car l)))
       (t (leftmost-2 (cdr l)))))))

(leftmost-2 '(((a) b) (c d)))
(leftmost-2 '(((a) ()) () (e)))
(leftmost-2 '(((() a) ()))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;using let
(defun leftmost-3 (l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l)) (car l))
   (t (let ((a (leftmost-3 (car l))))
	(cond
	 ((atom? a) a)
	 (t (leftmost-3 (cdr l))))))))

(leftmost-3 '(((a) b) (c d)))
(leftmost-3 '(((a) ()) () (e)))
(leftmost-3 '(((() a) ()))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rember* (a l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((eq (car l) a)
      (rember* a (cdr l)))
     (t (cons (car l)
	      (rember* a (cdr l))))))
   (t (cons (rember* a (car l))
	    (rember* a (cdr l))))))

(rember* 'cup '((coffee) cup ((tea) cup)
                         (and (hick)) cup))

(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))
              
;;use letrec
(defun rember*-1 (a l)
  (letrec
      ((R (lambda (l)
	    (cond
	     ((null l) (quote ()))
	     ((atom? (car l))
	      (cond
	       ((eq (car l) a)
		(funcall R (cdr l)))
	       (t (cons (car l)
			(funcall R (cdr l))))))
	     (t (cons
		 (funcall R (car l))
		 (funcall R (cdr l))))))))
    (funcall R l)))

(rember*-1 'cup '((coffee) cup ((tea) cup)
                         (and (hick)) cup))

(rember*-1 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))

;;;;;rember1*

;;eqlist
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

(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage) (and (soda)))) '(beef ((salami) (and (soda)))))
(eqlist? '(beef ((sausage) (and (soda)))) '(beef ((sausage) (and (soda)))))

(defun rember1* (a l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((eq (car l) a) (cdr l))
     (t (cons (car l)
	      (rember1* a (cdr l))))))
   (t (cond
       ((eqlist? (rember1* a (car l))
		 (car l))
	(cons (car l)
	      (rember1* a (cdr l))))
       (t (cons (rember1* a (car l))
		(cdr l)))))))

(rember1* 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1* 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))

;;Use l2 commandment (letrec)

(defun rember1*-1 (a l)
  (letrec
      ((R (lambda (l)
	    (cond
	     ((null l) (quote ()))
	     ((atom? (car l))
	      (cond
	       ((eq (car l) a) (cdr l))
	       (t (cons (car l)
			(funcall R (cdr l))))))
	     (t (cond
		 ((eqlist? (funcall R (car l))
			   (car l))
		  (cons (car l)
			(funcall R (cdr l))))
		 (t (cons (funcall R (car l))
			  (cdr l)))))))))
    (funcall R l)))

(rember1*-1 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-1 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 


;;using let
(defun rember1*-2 (a l)
  (letrec
      ((R (lambda (l)
	    (cond
	     ((null l) (quote ()))
	     ((atom? (car l))
	      (cond
	       ((eq (car l) a) (cdr l))
	       (t (cons (car l)
			(funcall R (cdr l))))))
	     (t (let ((av (funcall R (car l))))
		  (cond
		   ((eqlist? (car l) av)
		    (cons (car l) (funcall R (cdr l))))
		   (t (cons av (cdr l))))))))))
    (funcall R l)))

(rember1*-2 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-2 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;depth

(defun depth* (l)
  (cond
   ((null l) 1)
   ((atom? (car l))
    (depth* (cdr l)))
   (t (cond
       ((> (depth* (cdr l))
	   (1+ (depth* (car l))))
	(depth* (cdr l)))
       (t (1+ (depth* (car l))))))))

(depth* '((pickled) peppers (peppers pickled)))


(depth* '(margarine
            ((bitter butter)
             (makes)
             (batter (bitter)))
            better))

(depth* '(c (b (a b) a) a))


;;using let - incorrect version
(defun depth*-1 (l)
  (let ((a (1+ (depth*-1 (car l))))
	(d (depth*-1 (cdr l))))
    (cond
     ((null l) 1)
     ((atom? (car l)) d)
     (t (cond
	 ((> d a) d)
	 (t a))))))

;;will not work
(depth*-1 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))

;;using let for the last cond-line
;;our favorite version
(defun depth*-2 (l)
  (cond
   ((null l) 1)
   ((atom? (car l))
    (depth*-2 (cdr l)))
   (t
    (let ((a (1+ (depth*-2 (car l))))
	  (d (depth*-2 (cdr l))))
      (cond
       ((> d a) d)
       (t a))))))

(depth*-2 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))

(depth*-2 '((pickled) peppers (peppers pickled)))
(depth*-2 '(c (b (a b) a) a))


;;use let right after finding out whether or not l is empyt
(defun depth*-3 (l)
  (cond
   ((null l) 1)
   (t (let ((d (depth*-3 (cdr l))))
	(cond
	 ((atom? (car l)) d)
	 (t (cond
	     ((> d (1+ (depth*-3 (car l)))) d)
	     (t (1+ (depth*-3 (car l)))))))))))

(depth*-3 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))
(depth*-3 '((pickled) peppers (peppers pickled)))
(depth*-3 '(c (b (a b) a) a))

;;use let for repated expression (1+ (depth*-3 (car l)))
(defun depth*-4 (l)
  (cond
   ((null l) 1)
   (t (let ((d (depth*-4 (cdr l))))
	(cond
	 ((atom? (car l)) d)
	 (t (let ((a (1+ (depth*-4 (car l)))))
	      (cond
	       ((> d a) d)
	       (t a)))))))))

(depth*-4 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))
(depth*-4 '((pickled) peppers (peppers pickled)))
(depth*-4 '(c (b (a b) a) a))

;;Make depth* more enjoyable
;;Use if
(defun depth*-5 (l)
  (cond
   ((null l) 1)
   ((atom? (car l))
    (depth*-5 (cdr l)))
   (t (let ((a (1+ (depth*-5 (car l))))
	    (d (depth*-5 (cdr l))))
	(if (> d a) d a)))))

(depth*-5 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))
(depth*-5 '((pickled) peppers (peppers pickled)))
(depth*-5 '(c (b (a b) a) a))

;;define max
(defun max (n m)
  (if (> n m) n m))

(max 4 5)
(max 6 3)

;;use Max in depth*
(defun depth*-6 (l)
  (cond
   ((null l) 1)
   ((atom? (car l))
    (depth*-6 (cdr l)))
   (t
    (let ((a (1+ (depth*-6 (car l))))
	  (d (depth*-6 (cdr l))))
      (max a d)))))

(depth*-6 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))
(depth*-6 '((pickled) peppers (peppers pickled)))
(depth*-6 '(c (b (a b) a) a))

;;use max without let
(defun depth*-7 (l)
  (cond
   ((null l) 1)
   ((atom? (car l))
    (depth*-7 (cdr l)))
   (t (max
       (1+ (depth*-7 (car l)))
       (depth*-7 (cdr l))))))

(depth*-7 '(()
	    ((bitter butter)
	     (makes)
	     (batter (bitter)))
	    butter))
(depth*-7 '((pickled) peppers (peppers pickled)))
(depth*-7 '(c (b (a b) a) a))


;;Scramble the protected version

(defun one? (n)
  (zerop (1- n)))

(defun pick (n lat)
  (cond
   ((one? n) (car lat))
   (t (pick (1- n) (cdr lat)))))


(defun scramble-1 (tup)
  (letrec
      ((P (lambda (tup rp)
	    (cond
	     ((null tup) (quote ()))
	     (t (cons (pick (car tup)
			    (cons (car tup) rp))
		      (funcall P (cdr tup)
			       (cons (car tup) rp))))))))
    (funcall P tup (quote ()))))

(scramble-1 '(1 2 3 4 5 6 7 8 9))
(scramble-1 '(1 1 1 3 4 2 1 1 9 2))
(scramble-1 '(1 2 3 1 2 3 4 1 8 2 10))

;;use let with the repeated express (cons (car tup) rp)
(defun scramble (tup)
  (letrec
      ((P (lambda (tup rp)
	    (cond
	     ((null tup) (quote ()))
	     (t
	      (let ((rp (cons (car tup) rp)))
		  (cons (pick (car tup) rp)
			(funcall P (cdr tup) rp))))))))
    (funcall P tup (quote ()))))

(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble-1 '(1 2 3 1 2 3 4 1 8 2 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leftmost-4 (l)
  (catch 'skip
    (lm l 'skip)))

(defun lm (l out)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (throw out (car l)))
   (t (let ()
	(lm (car l) out)
	(lm (cdr l) out)))))

(leftmost-4 '(((a) b) (c d)))
(leftmost-4 '(((a) ()) () (e)))
(leftmost-4 '(((() a) ()))) 


;;;;;;;;;;;;;;;;
;;; Hide lm

;;minor function out of (lambda..)'s value part
(setq leftmost-5
      (letrec
	  ((lm (lambda (l out)
		 (cond
		  ((null l) (quote ()))
		  ((atom? (car l))
		   (throw out (car l)))
		  (t (let ()
		       (funcall lm (car l) out)
		       (funcall lm (cdr l) out)))))))
	(lambda (l)
	  (catch 'skip
	    (funcall lm l 'skip)))))

(funcall leftmost-5 '(((a) b) (c d)))
(funcall leftmost-5 '(((a) ()) () (e)))
(funcall leftmost-5 '(((() a) ()))) 

;;minor function moved in the (lambda ..) value part
(defun leftmost-6 (l)
  (letrec
      ((lm (lambda (l out)
	     (cond
	      ((null l) (quote ()))
	      ((atom? (car l))
	       (throw out (car l)))
	      (t (let ()
		   (funcall lm (car l) out)
		   (funcall lm (cdr l) out)))))))
    (catch 'skip
      (funcall lm l 'skip))))

(leftmost-6 '(((a) b) (c d)))
(leftmost-6 '(((a) ()) () (e)))
(leftmost-6 '(((() a) ()))) 

;;Move (letrecc in the value part of letcc/catch
(defun leftmost-7 (l)
  (catch 'skip
    (letrec
	((lm (lambda (l out)
	       (cond
		((null l) (quote ()))
		((atom? (car l))
		 (throw out (car l)))
		(t (let ()
		     (funcall lm (car l) out)
		     (funcall lm (cdr l) out)))))))
      (funcall lm l 'skip))))
       
(leftmost-7 '(((a) b) (c d)))
(leftmost-7 '(((a) ()) () (e)))
(leftmost-7 '(((() a) ()))) 


;;Use 12 Commandment on lm. out/skip doesn't change, so drop it

;;first rename out to skip
(defun leftmost-8 (l)
  (catch 'skip
    (letrec
	((lm (lambda (l skip)
	       (cond
		((null l) (quote ()))
		((atom? (car l))
		 (throw skip (car l)))
		(t (let ()
		     (funcall lm (car l) skip)
		     (funcall lm (cdr l) skip)))))))
      (funcall lm l 'skip))))

(leftmost-8 '(((a) b) (c d)))
(leftmost-8 '(((a) ()) () (e)))
(leftmost-8 '(((() a) ()))) 

;;Use 12 Commandment
(defun leftmost-9 (l)
  (catch 'skip
    (letrec
	((lm (lambda (l)
	       (cond
		((null l) (quote ()))
		((atom? (car l))
		 (throw 'skip (car l)))
		(t (let ()
		     (funcall lm (car l))
		     (funcall lm (cdr l))))))))
      (funcall lm l))))

(leftmost-9 '(((a) b) (c d)))
(leftmost-9 '(((a) ()) () (e)))
(leftmost-9 '(((() a) ()))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;using letcc with rember1*

(defun rm (a l oh)
  (cond
   ((null l) (throw oh (quote no)))
   ((atom? (car l))
    (if (eq (car l) a)
	(cdr l)
      (cons (car l)
	    (rm a (cdr l) oh))))
   (t (if (atom? (catch 'oh
		   (rm a (car l) 'oh)))
	  (cons (car l);a is not in the (car l)
		(rm a (cdr l) oh))
	(cons (rm a (car l) 0);list returned, a removed
	      (cdr l))))))

(defun rember1*-3 (a l)
  (if (atom? (catch 'Say (rm a l 'Say)))
      l
      (rm a l (quote ()))))

(rember1*-3 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-3 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 

;;Name the value of expression using let

(defun rm-1 (a l oh)
  (cond
   ((null l) (throw oh (quote no)))
   ((atom? (car l))
    (if (eq (car l) a)
	(cdr l)
      (cons (car l)
	    (rm-1 a (cdr l) oh))))
   (t (let ((new-car (catch 'oh
		       (rm-1 a (car l) 'oh))))
	(if (atom? new-car)
	    (cons (car l)
		  (rm-1 a (cdr l) oh))
	  (cons new-car (cdr l)))))))

  
(defun rember1*-4 (a l)
  (let ((new-l (catch 'Say (rm-1 a l 'Say))))
    (if (atom? new-l)
	l
        new-l)))

(rember1*-4 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-4 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 

;;protect rm and remove a. 12 & 13 Commandment
(defun rember1*-5 (a l)
  (letrec
      ((rm (lambda (l oh)
	     (cond
	      ((null l) (throw oh (quote no)))
	      ((atom? (car l))
	       (if (eq (car l) a)
		   (cdr l)
		 (cons (car l)
		       (funcall rm (cdr l) oh))))
	      (t (let ((new-car (catch 'oh
				  (funcall rm (car l) 'oh))))
		   (if (atom? new-car)
		       (cons (car l)
			     (funcall rm (cdr l) oh))
		     (cons new-car (cdr l)))))))))
    (let ((new-l (catch 'Say
		   (funcall rm l 'Say))))
      (if (atom? new-l)
	  l
	new-l))))

(rember1*-5 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-5 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		 

(defun rember1*-6 (a l)
  (condition-case nil
      (rm-1 a l)
   (error nil l)))

(rember1*-6 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))


(rember1*-6 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))		


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Understanding letrec, letcc, and let with multiple expressions in the value part

(let ()
  (quote little)
  (quote schemer))

(progn
  (quote little)
  (quote schemer))


(letrec ()
  (quote little)
  (quote schemer))


(letrec ((a (lambda (x)
              (print x))))
  (quote 'season)
  (funcall a 'schemer))

(catch 'hop
  (quote daniel))


(catch 'hop
  (quote daniel)
  (quote friedman))

(catch 'success
  (catch 'oh
    (throw 'success (throw 'oh 'no))))

(catch 'success
  (catch 'oh
    (throw 'success 'yes)))

(catch 'success
  (catch 'oh
    (throw 'success (throw 'oh 'no)));Throw 'oh return value= No
  (quote original-list));This line executed; No skipped, and return the original list

(catch 'success
  (catch 'oh
    (throw 'success 'new-list));new-list is returned
  (quote original-list));this line is not executed. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;How to implement try??!!















