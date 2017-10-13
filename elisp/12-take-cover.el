;;Code for Chatper 12. Take Cover

(setq lexical-binding t);; important to include this line

(defun multirember_old (a lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) a)
    (multirember_old a (cdr lat)))
   (t (cons (car lat)
	       (multirember_old a (cdr lat))))))

(multirember_old 'tuna
                 '(shrimp salad tuna salad and tuna))

(defun lenght (l)
  (cond
   ((null l) 0)
   (t (1+ (length (cdr l))))))

(length '(1 2 3 4))


;;Applicative-Order Y
(defun Y (le)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall le (lambda (x)
		   (funcall (funcall f f) x))))))

;;Use funcall when invoking a function argument or a function that has not been defuned.LS-pg126
(funcall
 ((lambda (le)
    ((lambda (f)
       (funcall f f))
     (lambda (f)
       (funcall le (lambda (x)
		     (funcall (funcall f f) x))))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))
 '(1 2 3 4 5 6))

(funcall
 (Y  (lambda (length)
       (lambda (l)
	 (cond
	  ((null l) 0)
	  (t (1+ (funcall length (cdr l))))))))
 '(1 2 3 4 5 6))


(defun length_1 ()
    (Y  (lambda (length)
       (lambda (l)
	 (cond
	  ((null l) 0)
	  (t (1+ (funcall length (cdr l)))))))))

(funcall (length_1) '(1 2 3 4))

;;Use setq to define a function that can be funcalled. LS-pg.128
(setq length_2
      (Y  (lambda (length)
       (lambda (l)
	 (cond
	  ((null l) 0)
	  (t (1+ (funcall length (cdr l)))))))))

(funcall length_2 '(1 2 3 4))

(defun multirember_1 (a lat)
  (funcall
   (Y (lambda (mr)
	(lambda (lat)
	  (cond
	   ((null lat) (quote ()))
	   ((eq (car lat) a)
	    (funcall mr (cdr lat)))
	   (t (cons (car lat)
		    (funcall mr (cdr lat))))))))
   lat))

(multirember_1 'tuna
                 '(shrimp salad tuna salad and tuna))


(defun multirember_2 (a lat)
  (funcall
   (letrec
       ((mr (lambda (lat)
	      (cond
	       ((null lat) (quote ()))
	       ((eq (car lat) a)
		(funcall mr (cdr lat)))
	       (t (cons (car lat)
			(funcall mr (cdr lat))))))))
     mr)
   lat))

(multirember_2 'tuna
                 '(shrimp salad tuna salad and tuna))

(multirember_2 'pie
               '(apple custard pie linzer pie torte))


(defun multirember_3 (a lat)
  (letrec
      ((mr (lambda (lat)
	     (cond
	      ((null lat) (quote ()))
	      ((eq (car lat) a)
	       (funcall mr (cdr lat)))
	      (t (cons (car lat)
		       (funcall mr (cdr lat))))))))
    (funcall mr lat)))

(multirember_3 'pie
               '(apple custard pie linzer pie torte))

(defun rember-f (test?)
  (lambda (a l)
    (cond
     ((null l) (quote ()))
     ((funcall test? (car l) a)
      (cdr l))
     (t (cons (car l)
	      (funcall (rember-f test?) a (cdr l)))))))

(setq rember-eq? (rember-f (function eq)))

(funcall (rember-f (function eq)) 'tuna '(tuna salad is good))

(funcall rember-eq? 'tuna '(tuna salad is good))

(defun multirember-f (test?)
  (lambda (a lat)
    (cond
     ((null lat) (quote ()))
     ((funcall test? (car lat) a)
      (funcall (multirember-f test?) a (cdr lat)))
     (t (cons (car lat)
	      (funcall (multirember-f test?) a (cdr lat)))))))

(funcall (multirember-f (function eq)) 'pie
               '(apple custard pie linzer pie torte))

(defun multirember-f-1 (test?)
  (letrec
      ((m-f (lambda (a lat)
	      (cond
	       ((null lat) (quote ()))
	       ((funcall test? (car lat) a)
		(funcall m-f a (cdr lat)))
	       (t (cons (car lat)
			(funcall m-f a (cdr lat))))))))
    m-f))


(funcall (multirember-f-1 (function eq)) 'pie
	 '(apple custard pie linzer pie torte))

(setq multirember_4
      (letrec
	  ((mr (lambda (a lat)
		 (cond
		  ((null lat) (quote ()))
		  ((eq (car lat) a)
		   (funcall mr a (cdr lat)))
		  (t (cons (car lat)
			   (funcall mr a (cdr lat))))))))
	mr))

(funcall multirember_4 'pie
	 '(apple custard pie linzer pie torte))

(defun member_1? (a lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) a) t)
   (t (memeber? a (cdr lat)))))

(member_1? 'ice '(salad greens with pears brie cheese frozen yogurt))

(defun member_2? (a lat)
  (funcall
   (letrec
       ((yes? (lambda (l)
		(cond
		 ((null l) nil)
		 ((eq (car l) a) t)
		 (t (funcall yes? (cdr l)))))))
     yes?)
   lat))

(member_2? 'ice '(salad greens with pears brie cheese frozen yogurt))

(defun member? (a lat)
  (letrec
      ((yes? (lambda (l)
	       (cond
		((null l) nil)
		((eq (car l) a) t)
		(t (funcall yes? (cdr l)))))))
    (funcall yes? lat)))

(member? 'ice '(salad greens with pears brie cheese frozen yogurt))

(defun union_1 (set1 set2)
  (cond
   ((null set1) set2)
   ((member? (car set1) set2)
    (union_1 (cdr set1) set2))
   (t (cons (car set1)
	    (union_1 (cdr set1) set2)))))


(union_1 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(defun union_2 (set1 set2)
  (letrec
      ((U (lambda (set)
	    (cond
	     ((null set) set2)
	     ((member? (car set) set2)
	      (funcall U (cdr set)))
	     (t (cons (car set)
		      (funcall U (cdr set))))))))
    (funcall U set1)))

(union_2 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(defun union_3 (set1 set2)
  (letrec
      ((U (lambda (set)
	    (cond
	     ((null set) set2)
	     ((funcall M? (car set) set2)
	      (funcall U (cdr set)))
	     (t (cons (car set)
		      (funcall U (cdr set)))))))
       (M? (lambda (a lat)
	     (cond
	      ((null lat) nil)
	      ((eq (car lat) a) t)
	      (t (funcall M? a (cdr lat)))))))
    (funcall U set1)))


(union_3 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(defun union_4 (set1 set2)
  (letrec
      ((U (lambda (set)
	    (cond
	     ((null set) set2)
	     ((funcall M? (car set) set2)
	      (funcall U (cdr set)))
	     (t (cons (car set)
		      (funcall U (cdr set)))))))
       (M? (lambda (a lat)
	     (letrec
		 ((N? (lambda (lat)
			(cond
			 ((null lat) nil)
			 ((eq (car lat) a) t)
			 (t (funcall N? (cdr lat)))))))
	       (funcall N? lat)))))
    (funcall U set1)))

(union_4 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(defun two-in-a-row-1? (lat)
  (letrec
      ((W (lambda (a lat)
	    (cond
	     ((null lat) nil)
	     (t (or (eq (car lat) a)
		    (funcall W (car lat) (cdr lat))))))))
    (cond
     ((null lat) nil)
     (t (funcall W (car lat) (cdr lat))))))

(two-in-a-row-1? '(Italian sardines spaghetti parsely))
(two-in-a-row-1? '(Italian sardines sardines spaghetti parsely))
(two-in-a-row-1? '(Italian sardines more sardines spaghetti))


(setq two-in-a-row-2?
    (letrec
	((W (lambda (a lat)
	      (cond
	       ((null lat) nil)
	       (t (or (eq (car lat) a)
		      (funcall W (car lat) (cdr lat))))))))
      (lambda (lat)
	(cond
	 ((null lat) nil)
	 (t (funcall W (car lat) (cdr lat)))))))

(funcall two-in-a-row-2? '(Italian sardines spaghetti parsely))
(funcall two-in-a-row-2? '(Italian sardines sardines spaghetti parsely))
(funcall two-in-a-row-2? '(Italian sardines more sardines spaghetti))

(defun two-in-a-row-3? ()
  (letrec
      ((W (lambda (a lat)
	    (cond
	     ((null lat)nil)
	     (t (or (eq (car lat) a)
		    (funcall W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
       ((null lat) nil)
       (t (funcall W (car lat) (cdr lat)))))))

(funcall (two-in-a-row-3?) '(Italian sardines spaghetti parsely))
(funcall (two-in-a-row-3?) '(Italian sardines sardines spaghetti parsely))
(funcall (two-in-a-row-3?) '(Italian sardines more sardines spaghetti))

(defun sum-of-prefixes-1 (tup)
  (letrec
      ((S (lambda (sss tup)
	    (cond
	     ((null tup) (quote()))
	     (t (cons (+ sss (car tup))
		      (funcall S (+ sss (car tup))
			       (cdr tup))))))))
    (funcall S 0 tup)))

(sum-of-prefixes-1 '(2 1 9 17 0))
(sum-of-prefixes-1 '( 1 1 1 1))

(setq sum-of-prefixes-2
      (letrec
	  ((S (lambda (sss tup)
		(cond
		 ((null tup) (quote ()))
		 (t (cons (+ sss (car tup))
			  (funcall S (+ sss (car tup))
				   (cdr tup))))))))
	(lambda (tup)
	  (funcall S 0 tup))))

(funcall sum-of-prefixes-2 '(2 1 9 17 0))
(funcall sum-of-prefixes-2 '( 1 1 1 1))

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

(setq scramble-2
      (letrec
	  ((P (lambda (tup rp)
		(cond
		 ((null tup) (quote ()))
		 (t (cons (pick (car tup)
				(cons (car tup) rp))
			  (funcall P (cdr tup)
				   (cons (car tup) rp))))))))
	(lambda (tup)
	  (funcall P tup (quote ())))))

(funcall scramble-2 '(1 2 3 4 5 6 7 8 9))
(funcall scramble-2 '(1 1 1 3 4 2 1 1 9 2))
(funcall scramble-2 '(1 2 3 1 2 3 4 1 8 2 10))

