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

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1 (length (cdr l)))))))))

(length '(1 2 3 4 5 6 7))

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
                    ((rember-f test?) a
                                      (cdr l))))))))

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

(multirember-f eq?)

(define member_1?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member_1? a (cdr lat))))))

(define member?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (lat)
                 (cond
                   ((null? lat) #f)
                   ((eq? (car lat) a) #t)
                   (else (cons (car lat)
                               (yes? a (cdr lat))))))))
      (yes? lat))))v