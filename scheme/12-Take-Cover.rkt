;;Chapter 12. Take Cover
#lang racket

(define multirember_old
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember_old a (cdr lat)))
      (else (cons (car lat)
                  (multirember_old a (cdr lat)))))))

(multirember_old 'tuna
                 '(shrimp salad tuna salad and tuna))

;Y combinator - Applicative-Order Y
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;;length
((Y (lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l))))))))
 '(1 2 3 4 5 6 7 8 9 0 11))

(define length_1
  ((lambda (le)
     ((lambda (f) (f f))
      (lambda (f)
        (le (lambda (x) ((f f) x))))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))))

(length_1 '(1 2 3 4 5))

(define length_2
  (Y (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1 (length (cdr l)))))))))

(length_2 '(1 2 3 4 5 6 7))

;;define legnth using letrec
(define length_3
  (lambda (l)
    ((letrec
         ((length (lambda (l)
                    (cond
                      ((null? l) 0)
                      (else
                       (add1 (length (cdr l))))))))
       length)
     l)))

(length_3 '(1 2 3 4 5 6 7 8))

(define length_4
  (lambda (l)
    (letrec
        ((length (lambda (l)
                   (cond
                     ((null? l) 0)
                     (else
                      (add1 (length (cdr l))))))))
      (length l))))

(length_4 '(1 2 3 4 5 6 7 8 9))

(define multirember_2
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? a (car lat))
               (mr (cdr lat)))
              (else (cons (car lat)
                          (mr (cdr lat))))))))
     lat)))

(multirember_2 'tuna
               '(shrimp salad tuna salad and tuna))


(define multirember_3
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? (car lat) a)
                   (mr (cdr lat)))
                  (else (cons (car lat)
                              (mr (cdr lat))))))))
       mr)
     lat)))

(multirember_3 'tuna
               '(shrimp salad tuna salad and tuna))

(multirember_3 'pie
               '(apple custard pie linzer pie torte))

(define multirember_4
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                 ((null? lat) (quote ()))
                 ((eq? (car lat) a)
                  (mr (cdr lat)))
                 (else (cons (car lat)
                             (mr (cdr lat))))))))
      (mr lat))))

(multirember_4 'pie
               '(apple custard pie linzer pie torte))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define multirember-f_1
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f_1 test?) a
                                             (cdr lat))))))))

(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) (quote ()))
              ((test? (car lat) a)
               (m-f a (cdr lat)))
              (else (cons (car lat)
                          (m-f a (cdr lat))))))))
      m-f)))

((multirember-f eq?) 'a '(1 2 a 3 4 a 5 6 a 7))


(define multirember_5
  (letrec
      ((mr (lambda (a lat)
             (cond
               ((null? lat) (quote ()))
               ((eq? (car lat) a)
                (mr a (cdr lat)))
               (else (cons (car lat)
                           (mr a (cdr lat))))))))
    mr))

(multirember_5 'a '(1 2 a 3 4 a 5 6 a))

;;Implementing multirember-f using Y
(define multirember-f-Y-1
  (lambda (test?)
    (Y (lambda (mr)
         (lambda (a lat)
           (cond
             ((null? lat) (quote ()))
             ((test? (car lat) a)
              (mr a (cdr lat)))
             (else (cons (car lat)
                         (mr a (cdr lat))))))))))

;;The following application of multirember-f-Y-1 will fail, because our definition of Y combinator defined above
;;accepts only funciton with one argument. To fix it, define Y combinator that accepts two arguments
;;((multirember-f-Y-1 eq?) 'pie '(apple custard pie linzer pie torte))


;;Y combinator that accepts function with two arguments
(define Y_2
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (a b) ((f f) a b)))))))

(define multirember-f-Y_2
  (lambda (test?)
    (Y_2 (lambda (mr)
           (lambda (a lat)
             (cond
               ((null? lat) (quote ()))
               ((test? (car lat) a)
                (mr a (cdr lat)))
               (else (cons (car lat)
                           (mr a (cdr lat))))))))))


((multirember-f-Y_2 eq?) 'pie '(apple custard pie linzer pie torte))

(define member_1?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member_1? a (cdr lat))))))

(member_1? 'ice '(salad greens with pears brie cheese frozen yogurt))

(define member_2?
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                  (cond
                    ((null? l) #f)
                    ((eq? (car l) a) #t)
                    (else (yes? (cdr l)))))))
       yes?)
     lat)))

(member_2? 'ice '(salad greens with pears brie cheese frozen yogurt))

(define member?
  (lambda (a lat)
    (letrec
     ((yes? (lambda (l)
              (cond
                ((null? l) #f)
                ((eq? (car l) a) #t)
                (else (yes? (cdr l)))))))
     (yes? lat))))

(member? 'ice '(salad greens with pears brie cheese frozen yogurt))

(define union_1
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union_1 (cdr set1) set2))
      (else (cons (car set1)
                  (union_1 (cdr set1) set2))))))

(union_1 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define union_2
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((member? (car set) set2)
                 (U (cdr set)))
                (else (cons (car set)
                            (U (cdr set))))))))
      (U set1))))

(union_2 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define union_3
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((M? (car set) set2)
                 (U  (cdr set)))
                (else (cons (car set)
                            (U (cdr set)))))))
         (M? (lambda (a lat)
                   (cond
                     ((null? lat) #f)
                     ((eq? (car lat) a) #t)
                     (else (M? a (cdr lat)))))))
      (U set1))))

(union_3 '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((M? (car set) set2)
                 (U (cdr set)))
                (else (cons (car set)
                            (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            ((eq? (car lat) a) #t)
                            (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))

(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese))


(define two-in-a-row_1?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond
                ((null? lat) #f)
                (else (or (eq? a (car lat))
                          (W (car lat) (cdr lat))))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))


(two-in-a-row_1? '(b d e i i a g))

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? a (car lat))
                        (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(two-in-a-row? '(b d e i i a g))

(define sum-of-prefixes_1
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
                ((null? tup) (quote ()))
                (else (cons
                       (+ sss (car tup))
                       (S (+ sss (car tup))
                          (cdr tup))))))))
      (S 0 tup))))


(sum-of-prefixes_1 '(2 1 9 17 0))
(sum-of-prefixes_1 '(1 1 1 1 1))

(define sum-of-prefixes
  (letrec
      ((S (lambda (sss tup)
            (cond
              ((null? tup) (quote ()))
              (else (cons
                     (+ sss (car tup))
                     (S (+ sss (car tup))
                        (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))

(sum-of-prefixes '(2 1 9 17 0))
(sum-of-prefixes '(1 1 1 1 1))


(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))


(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) (quote ()))
      (else
       (cons (pick (car tup)
                   (cons (car tup) rev-pre))
                   (scramble-b
                    (cdr tup)
                    (cons (car tup) rev-pre)))))))

(define scramble_1
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) (quote ()))
                (else
                 (cons (pick (car tup)
                             (cons (car tup) rp))
                       (P (cdr tup)
                          (cons (car tup) rp))))))))
      (P tup (quote ())))))

(scramble_1 '(1 1 1 3 4 2 1 1 9 2))
(scramble_1 '(1 2 3 4 5 6 7 8 9))
(scramble_1 '(1 2 3 1 2 3 4 1 8 2 10))


(define scramble
  (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) (quote ()))
              (else
               (cons (pick (car tup)
                           (cons (car tup) rp))
                     (P (cdr tup)
                        (cons (car tup) rp))))))))
    (lambda (tup)
      (P tup (quote ())))))

(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 2 3 1 2 3 4 1 8 2 10))