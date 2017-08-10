
(defun member? (a lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) a)
	  (member? a (cdr lat))))))

(member? 'sardines '(Italian sardines spaghetti parsley))


(defun is-first? (a lat)
  (cond
   ((null lat) nil)
   (t (eq (car lat) a))))

(defun two-in-a-row? (lat)
  (cond
   ((null lat) nil)
   (t (or
       (is-first? (car lat) (cdr lat))
       (two-in-a-row? (cdr lat))))))

(two-in-a-row? '(Italian sardines spaghetti parsely))
(two-in-a-row? '(Italian sardines sardines spaghetti parsely))
(two-in-a-row? '(Italian sardines more sardines spaghetti))


(defun two-in-a-row-2? (lat)
  (cond
   ((null lat) nil)
   (t (is-first-b? (car lat) (cdr lat)))))

(defun is-first-b? (a lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) a)
	  (two-in-a-row-2? lat)))))

(two-in-a-row-2? '(Italian sardines spaghetti parsely))
(two-in-a-row-2? '(Italian sardines sardines spaghetti parsely))
(two-in-a-row-2? '(Italian sardines more sardines spaghetti))

(defun two-in-a-row-b? (preceeding lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) preceeding)
	  (two-in-a-row-b? (car lat) (cdr lat))))))

(defun two-in-a-row? (lat)
  (cond
   ((null lat) nil)
   (t (two-in-a-row-b? (car lat) (cdr lat)))))


(two-in-a-row? '(Italian sardines spaghetti parsely))
(two-in-a-row? '(Italian sardines sardines spaghetti parsely))
(two-in-a-row? '(Italian sardines more sardines spaghetti))


(defun sum-of-prefixes-b (sonssf tup)
  (cond
   ((null tup) (quote ()))
   (t (cons (+ sonssf (car tup))
	    (sum-of-prefixes-b
	     (+ sonssf (car tup))
	     (cdr tup))))))

(defun sum-of-prefixes (tup)
  (cond
   ((null tup) (quote ()))
   (t (sum-of-prefixes-b 0 tup))))


(defun one? (n)
  (zerop (1- n)))

(one? 1)
(one? 5)


(defun pick (n lat)
  (cond
   ((one? n) (car lat))
   (t (pick (1- n) (cdr lat)))))

(pick 4 '(4 3 1 1 1))
(pick 2 '(2 4 3 1 1 1))


(defun scramble-b (tup rev-pre)
  (cond
   ((null tup) (quote ()))
   (t (cons (pick (car tup)
		  (cons (car tup) rev-pre))
	    (scramble-b (cdr tup)
			(cons (car tup) rev-pre))))))

(scramble-b '(1 2 3 4 5 6 7 8 9) '())
(scramble-b '(1 1 1 3 4 2 1 1 9 2) '())

(defun scramble (tup)
  (scramble-b tup '()))

(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble '(1 2 3 1 2 3 4 1 8 2 10))
