;;Chapter 13 Hope, Skip, and Jump
(setq lexical-binding t);; important to include this line

(defun member? (a lat)
  (letrec
      ((M (lambda (lat)
	    (cond
	     ((null lat) nil)
	     ((eq (car lat) a) t)
	     (t (funcall M (cdr lat)))))))
    (funcall M lat)))

(member? 3 '(1 2 3 4 5 6 7))


(defun intersect-1 (set1 set2)
  (cond
   ((null set1) (quote ()))
   ((member? (car set1) set2)
    (cons (car set1)
	  (intersect-1 (cdr set1) set2)))
   (t (intersect-1 (cdr set1) set2))))

(intersect-1 '(tomatoes and macaroni)
             '(macaroni and chees))

(defun intersect-2 (set1 set2)
  (letrec
      ((I (lambda (set1)
	    (cond
	     ((null set1) (quote ()))
	     ((member? (car set1) set2)
	      (cons (car set1)
		    (funcall I (cdr set1))))
	     (t (funcall I (cdr set1)))))))
    (funcall I set1)))

(intersect-2 '(tomatoes and macaroni)
             '(macaroni and chees))

;;intersectall: L.S. Chapter 7 - pg. 117
;;Assume the list of sets is non-empty
(defun intersectall-1 (lset)
  (cond
   ((null (cdr lset)) (car lset))
   (t (intersect-2 (car lset)
		   (intersectall-1 (cdr lset))))))

(intersectall-1 '((a b c) (c a de) (e f g h a b)))

(intersectall-1 '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with some apples)))


(defun intersectall-2 (lset)
  (cond
   ((null lset) (quote ()))
   ((null (cdr lset)) (car lset))
   (t (intersect-2 (car lset)
		   (intersectall-2 (cdr lset))))))

(intersectall-2 '((a b c) (c a de) (e f g h a b)))

(intersectall-2 '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with some apples)))


(defun intersectall-3 (lset)
  (letrec
      ((A (lambda (lset)
	    (cond
	     ((null (cdr lset)) (car lset))
	     (t (intersect-2 (car lset)
			     (funcall A (cdr lset))))))))
    (cond
     ((null lset) (quote ()))
     (t (funcall A lset)))))

(intersectall-3 '((a b c) (c a de) (e f g h a b)))

(intersectall-3 '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with some apples)))


(intersectall-3 (quote ()))

(intersectall-3 '((3 mangos and)
                  (3 kiwis and)
                  (3 hamburgres)))

(intersectall-3 '((3 steaks and)
                  (no food and)
                  (three baked potatoes)
                  (3 diet hamburges)))

(intersectall-3 '((3 mangoes and)
                  ()
                  (3 diet hamburgers)))


(defun intersectall-4 (lset)
  (catch 'hop
    (letrec
	((A (lambda (lset)
	      (cond
	       ((null (car lset))
		(throw 'hop (quote ())))
	       ((null (cdr lset))
		(car lset))
	       (t (intersect-2 (car lset)
			       (funcall A (cdr lset))))))))
      (cond
       ((null lset) (quote ()))
       (t (funcall A lset))))))

(intersectall-4 '((3 mangos and)
                  (3 kiwis and)
                  (3 hamburgres)))

(intersectall-4 '((3 steaks and)
                  (no food and)
                  (three baked potatoes)
                  (3 diet hamburges)))

(intersectall-4 '((3 mangoes and)
                  ()
                  (3 diet hamburgers)))

(defun intersect-3 (set1 set2)
  (letrec
      ((I (lambda (set1)
	    (cond
	     ((null set1) (quote ()))
	     ((member? (car set1) set2)
	      (cons (car set1)
		    (funcall I (cdr set1))))
	     (t (funcall I (cdr set1)))))))
    (cond
     ((null set2) (quote ()))
     (t (funcall I set1)))))

(intersect-3 '(tomatoes and macaroni)
             '(macaroni and chees))

(intersect-3 '(tomatoes and macaroni)
             '())

(defun intersectall-5 (lset)
  (catch 'hop
    (letrec
	((A (lambda (lset)
	      (cond
	       ((null (car lset))
		(throw 'hop (quote ())))
	       ((null (cdr lset))
		(car lset))
	       (t (funcall I (car lset)
			   (funcall A (cdr lset)))))))
	 (I (lambda (s1 s2)
	      (letrec
		  ((J (lambda (s1)
			(cond
			 ((null s1) (quote ()))
			 ((member? (car s1) s2)
			  (cons (car s1)
				(funcall J (cdr s1))))
			 (t (funcall J (cdr s1)))))))
		(cond
		 ((null s2)
		  (throw 'hop (quote ())))
		 (t (funcall J s1)))))))
      (cond
       ((null lset) (quote ()))
       (t (funcall A lset))))))


(intersectall-5 '((3 mangos and)
                  (3 kiwis and)
                  (3 hamburgres)))

(intersectall-5 '((3 steaks and)
                  (no food and)
                  (three baked potatoes)
                  (3 diet hamburges)))

(intersectall-5 '((3 mangoes and)
                  ()
                  (3 diet hamburgers)))

(defun rember-1 (a lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) a) (cdr lat))
   (t (cons (car lat)
	    (rember-1 a (cdr lat))))))

(rember-1 3 '(1 2 3 4 5 67 ))

(defun rember-2 (a lat)
  (letrec
      ((R (lambda (lat)
	    (cond
	     ((null lat) (quote ()))
	     ((eq (car lat) a) (cdr lat))
	     (t (cons (car lat)
		      (funcall R (cdr lat))))))))
    (funcall R lat)))

(rember-2 3 '(1 2 3 4 5 67 ))

(defun rember-beyond-first-1 (a lat)
  (letrec
      ((R (lambda (lat)
	    (cond
	     ((null lat) (quote ()))
	     ((eq (car lat) a)
	      (quote ()))
	     (t (cons (car lat)
		      (funcall R (cdr lat))))))))
    (funcall R lat)))

(rember-beyond-first-1 'roots
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))

(rember-beyond-first-1 'others
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))

(rember-beyond-first-1 'sweetthing
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))

(defun rember-upto-last (a lat)
  (catch 'skip
    (letrec
	((R (lambda (lat)
	      (cond
	       ((null lat) (quote ()))
	       ((eq (car lat) a)
		(throw 'skip (funcall R (cdr lat))))
	       (t (cons (car lat)
			(funcall R (cdr lat))))))))
      (funcall R lat))))

(rember-upto-last 'roots
                  '(noodles spaghetti spatzel
                            bean-thread roots
                            potatoes yam others
                            rice))

(rember-upto-last 'sweetthing
                  '(noodles spaghetti spatzel
                            bean-thread roots
                            potatoes yam others
                            rice))

(rember-upto-last 'cookies
                  '(cookies
                    chocolate mints
                    caramel delight ginger snaps
                    desserts
                    chocolate mousse
                    vanilla ice cream
                    German chocolate cake
                    more cookies
                    gingerbreadman chocolate
                    chip brownies))
