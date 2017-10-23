#lang racket
;;Chapter 13 Hope, Skip, and Jump

(define member?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (lat)
                 (cond
                   ((null? lat) #f)
                   ((eq? (car lat) a) #t)
                   (else (yes? (cdr lat)))))))
      (yes? lat))))

(member? 'frozen '(salad greens with pears brie cheese frozen yogurt))

(define intersect-1
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect-1 (cdr set1) set2)))
      (else (intersect-1 (cdr set1) set2)))))

(intersect-1 '(tomatoes and macaroni)
             '(macaroni and chees))


(define intersect-2
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) (quote ()))
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))

(intersect-2 '(tomatoes and macaroni)
             '(macaroni and chees))

;;intersectall: L.S. Chapter 7 - pg. 117
;;Assume the list of sets is non-empty
(define intersectall-1
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))
      (else (intersect-2 (car lset)
                         (intersectall-1 (cdr lset)))))))

(intersectall-1 '((a b c) (c a de) (e f g h a b)))
(intersectall-1 '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with some apples)))

(define intersectall-2
  (lambda (lset)
    (cond
      ((null? lset) (quote ()))
      ((null? (cdr lset)) (car lset))
      (else (intersect-2 (car lset)
                         (intersectall-2 (cdr lset)))))))

(intersectall-2 '((a b c) (c a de) (e f g h a b)))
(intersectall-2 '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with some apples)))
(intersectall-2 (quote ()))


(define intersectall-3
  (lambda (lset)
    (letrec
        ((A
          (lambda (lset)
            (cond
              ((null? (cdr lset)) (car lset))
              (else (intersect-2 (car lset)
                                 (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))


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

(define intersectall-4
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))
                    (hop (quote ())))
                   ((null? (cdr lset))
                    (car lset))
                   (else
                    (intersect-2 (car lset)
                                 (A (cdr lset))))))))
         (cond
           ((null? lset) (quote ()))
           (else (A lset))))))))

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

(define intersect-3
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set1)
              (cond
                ((null? set1) (quote ()))
                ((member? (car set1) set2)
                 (cons (car set1)
                       (I (cdr set1))))
                (else (I (cdr set1)))))))
      (cond
        ((null? set2) (quote ()))
        (else (I set1))))))

(define intersectall-5
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))
                    (hop (quote ())))
                   ((null? (cdr lset))
                    (car lset))
                   (else (I (car lset)
                            (A (cdr lset)))))))
            (I (lambda (s1 s2)
                 (letrec
                     ((J (lambda (s1)
                           (cond
                             ((null? s1) (quote ()))
                             ((member? (car s1) s2)
                              (cons (car s1)
                                    (J (cdr s1))))
                             (else (J (cdr s1)))))))
                   (cond
                     ((null? s2) (hop (quote ())))
                     (else (J s1)))))))
         (cond
           ((null? lset) (quote ()))
           (else (A lset))))))))

(intersectall-5 '((3 steaks and)
                  (no food and)
                  (three baked potatoes)
                  (3 diet hamburges)))

(intersectall-5 '((3 mangoes and)
                  ()
                  (3 diet hamburgers)))

(define rember-1
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (cdr lat))
      (else (cons (car lat)
                  (rember-1 a (cdr lat)))))))


(define rember-2
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a)
                 (cdr lat))
                (else (cons (car lat)
                            (R (cdr lat))))))))
      (R lat))))

(define rember-beyond-first-1
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (quote ()))
      (else (cons (car lat)
                  (rember-beyond-first-1 a (cdr lat)))))))

(rember-beyond-first-1 'roots
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))


(define rember-beyond-first-2
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a)
                 (quote ()))
                (else (cons (car lat)
                            (R (cdr lat))))))))
      (R lat))))

(rember-beyond-first-2 'roots
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))

(rember-beyond-first-2 'others
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))

(rember-beyond-first-2 'sweetthing
                       '(noodles spaghetti spatzel
                                 bean-thread roots
                                 potatoes yam others
                                 rice))

(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
           ((R (lambda (lat)
                 (cond
                   ((null? lat) (quote ()))
                   ((eq? (car lat) a)
                    (skip (R (cdr lat))))
                   (else (cons (car lat)
                               (R (cdr lat))))))))
         (R lat))))))

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