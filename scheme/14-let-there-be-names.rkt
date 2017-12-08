#lang racket
;;Chapter 14 - Let There Be Names

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define leftmost-1
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost-1 (car l))))))

(leftmost-1 '(((a) b)(c d)))
(leftmost-1 '(((a) ()) () (e)))
;(leftmost-1 '(((() a) ())))

(define leftmost-2
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else (cond
              ((atom? (leftmost-2 (car l)))
               (leftmost-2 (car l)))
              (else (leftmost-2 (cdr l))))))))

(leftmost-2 '(((() aa) ())))

(leftmost-2 '(((a) b) (c d)))

(leftmost-2 '(((a) ()) () (e)))

;;==========================================
(define leftmost-3
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost-3 (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost-3 (cdr l)))))))))

(leftmost-3 '(((() aac) ())))

(leftmost-3 '(((aac) b) (c d)))

(leftmost-3 '(((aac) ()) () (e)))

;;============================================
(define  eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage) (and (soda)))) '(beef ((salami) (and (soda)))))
(eqlist? '(beef ((sausage) (and (soda)))) '(beef ((sausage) (and (soda)))))

;;============================================
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup)
                         (and (hick)) cup))

(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))

(define rember*-letrec
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a)
                    (R (cdr l)))
                   (else (cons (car l)
                               (R (cdr l))))))
                (else (cons (R (car l))
                            (R (cdr l))))))))
      (R l))))

(quote '(============================================))
(rember*-letrec 'cup '((coffee) cup ((tea) cup)
                                (and (hick)) cup))

(rember*-letrec 'sauce '(((tomato sauce))
                         ((bean) sauce)
                         (and ((flying)) sauce)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;rember1*;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember1*-1
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (cdr l))
         (else (cons (car l)
                     (rember1*-1 a (cdr l))))))
      (else
       (cond
         ((eqlist? (rember1*-1 a (car l))
                   (car l))
          (cons (car l)
                (rember1*-1 a (cdr l))))
         (else (cons (rember1*-1 a (car l))
                     (cdr l))))))))

(quote '(============================================))
(rember1*-1 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-1 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))

;; using letrec
(define rember1*-2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a)
                    (cdr l))
                   (else (cons (car l)
                               (R (cdr l))))))
                (else
                 (cond
                   ((eqlist? (R (car l))
                             (car l))
                    (cons (car l)
                          (R (cdr l))))
                   (else (cons (R (car l))
                               (cdr l)))))))))
      (R l))))

(quote '(============================================))
(rember1*-2 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-2 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))

;;========================================================
(define rember1*-3
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l)  a)
                    (cdr l))
                   (else (cons (car l)
                               (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? av (car l))
                      (cons (car l)
                            (R (cdr l))))
                     (else (cons av (cdr l))))))))))
      (R l))))

(quote '(============================================))
(rember1*-3 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-3 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          depth*

(define depth*-1
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-1 (cdr l)))
      (else
       (cond
         ((> (depth*-1 (cdr l))
             (add1 (depth*-1 (car l))))
          (depth*-1 (cdr l)))
         (else
          (add1 (depth*-1 (car l)))))))))

(quote '(============================================))


(depth*-1 '((pickled) peppers (peppers pickled)))

(depth*-1 '(margarine
            ((bitter butter)
             (makes)
             (batter (bitter)))
            better))

(depth*-1 '(c (b (a b) a) a))

;;; my first attempt :)
(define depth*-2
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-2 (cdr l)))
      (else
       (let ((dcdr (depth*-2 (cdr l)))
             (dcar (depth*-2 (car l))))
         (cond
           ((> dcdr (add1 dcar))
            dcdr)
           (else (add1 dcar))))))))

(quote '(============================================))
(depth*-2 '((pickled) peppers (peppers pickled)))

(depth*-2 '(margarine
            ((bitter butter)
             (makes)
             (batter (bitter)))
            better))

(depth*-2 '(c (b (a b) a) a))


(define depth*-3
  (lambda (l)
    (let
        ((a (add1 (depth*-3 (car l))))
         (d (depth*-3 (cdr l))))
      (cond
        ((null? l) 1)
        ((atom? (car l)) d)
        (else
         (cond
           ((> d a) d)
           (else a)))))))

;(quote '(============================================))

;(depth*-3 '(()
;            ((bitter butter)
;             (makes)
;             (batter (bitter)))
;            butter))
;
;(depth*-3 '((pickled) peppers (peppers pickled)))
;
;(depth*-3 '(margarine
;            ((bitter butter)
;             (makes)
;             (batter (bitter)))
;            better))
;
;(depth*-3 '(c (b (a b) a) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define depth*-4
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-4 (cdr l)))
      (else
       (let ((a (add1 (depth*-4 (car l))))
             (d (depth*-4 (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))

(quote '(============================================))
(depth*-4 '(()
            ((bitter butter)
             (makes)
             (batter (bitter)))
            butter))

(depth*-4 '((pickled) peppers (peppers pickled)))


(depth*-4 '(c (b (a b) a) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define depth*-5
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth*-5 (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (cond
              ((> d (add1 (depth*-5 (car l)))) d)
              (else (add1 (depth*-5 (car l))))))))))))

(quote '(============================================))
(depth*-5 '(()
            ((bitter butter)
             (makes)
             (batter (bitter)))
            butter))

(depth*-5 '((pickled) peppers (peppers pickled)))


(depth*-5 '(c (b (a b) a) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define depth*-6
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth*-6 (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (let ((a (add1 (depth*-6 (car l)))))
              (cond
                ((> d a) d)
                (else a))))))))))

(quote '(============================================))
(depth*-6 '(()
            ((bitter butter)
             (makes)
             (batter (bitter)))
            butter))

(depth*-6 '((pickled) peppers (peppers pickled)))


(depth*-6 '(c (b (a b) a) a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define depth*-7
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-7 (cdr l)))
      (else
       (let ((a (add1 (depth*-7 (car l))))
             (d (depth*-7 (cdr l))))
         (if (> d a) d a))))))

(quote '(============================================))
(depth*-7 '(()
            ((bitter butter)
             (makes)
             (batter (bitter)))
            butter))

(depth*-7 '((pickled) peppers (peppers pickled)))


(depth*-7 '(c (b (a b) a) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*-8
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-8 (cdr l)))
      (else
       (let ((a (add1 (depth*-8 (car l))))
             (d (depth*-8 (cdr l))))
         (max d a))))))

(quote '(============================================))
(depth*-8 '(()
            ((bitter butter)
             (makes)
             (batter (bitter)))
            butter))

(depth*-8 '((pickled) peppers (peppers pickled)))


(depth*-8 '(c (b (a b) a) a))


;;;;;;;;;;;;;;;without let;;;;;;;;;;;;;

(define depth*-9
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-9 (cdr l)))
      (else
       (max (depth*-9 (cdr l))
            (add1 (depth*-9 (car l))))))))


(quote '(============================================))
(depth*-9 '(()
            ((bitter butter)
             (makes)
             (batter (bitter)))
            butter))

(depth*-9 '((pickled) peppers (peppers pickled)))


(depth*-9 '(c (b (a b) a) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         Letting scramble


(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define scramble-1
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

(quote '(============================================))
(scramble-1 '(1 1 1 3 4 2 1 1 9 2))
(scramble-1 '(1 2 3 4 5 6 7 8 9))
(scramble-1 '(1 2 3 1 2 3 4 1 8 2 10))

;;;;

(define scramble-2
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) (quote()))
                (else
                 (let ((rp (cons (car tup) rp)))
                   (cons (pick (car tup) rp)
                         (P (cdr tup) rp))))))))
      (P tup (quote ())))))

(quote '(============================================))
(scramble-2 '(1 1 1 3 4 2 1 1 9 2))
(scramble-2 '(1 2 3 4 5 6 7 8 9))
(scramble-2 '(1 2 3 1 2 3 4 1 8 2 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define leftmost-4
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost-4 (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost-4 (cdr l)))))))))

(quote '(============================================))
(leftmost-4 '(((a)) b (c)))


;;;;;;;;;;;;;;;Leftmost with letcc - my first solution

(define leftmost-5
  (lambda (l)
    (call-with-current-continuation
     (lambda (hop)
       (cond
         ((null? l) (quote()))
         ((atom? (car l))
          (hop (car l)))
         (else
          (let ((a (leftmost-5 (car l))))
            (cond
              ((atom? a) (hop a))
              (else (leftmost-5 (cdr l)))))))))))

(quote '(============================================))
(leftmost-5 '(((a)) b (c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define leftmost-6
  (lambda (l)
    (call-with-current-continuation
     (lambda (skip)
       (lm l skip)))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (out (car l)))
      (else (let ()
              (lm (car l) out)
              (lm (cdr l) out))))))
       
(quote '(============================================))
(leftmost-6 '(((a)) b (c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move lm inside leftmost using letrec
;;this the solution that i figured out before checking the book

(define leftmost-7
  (lambda (l)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
           ((lm (lambda (l out)
                  (cond
                    ((null? l) (quote ()))
                    ((atom? (car l)) (out (car l)))
                    (else (let ()
                            (lm (car l) out)
                            (lm (cdr l) out)))))))
         (lm l skip))))))


(quote '(============================================))

(leftmost-7 '(((a)) b (c)))

;;;;; no need for out in lm 
(define leftmost-8
  (lambda (l)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
           ((lm (lambda (l)
                  (cond
                    ((null? l) (quote ()))
                    ((atom? (car l)) (skip (car l)))
                    (else (let ()
                            (lm (car l))
                            (lm (cdr l))))))))
         (lm l))))))


(quote '(============================================))

(leftmost-8 '(((a)) b (c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define leftmost-9
  (letrec
      ((lm (lambda (l out)
             (cond
               ((null? l) (quote ()))
               ((atom? (car l)) (out (car l)))
               (else (let ()
                       (lm (car l) out)
                       (lm (cdr l) out)))))))
    (lambda (l)
      (call-with-current-continuation
       (lambda (skip)
         (lm l skip))))))


(quote '(============================================))
(leftmost-9 '(((a)) b (c)))

;;;;;
(define leftmost-10
  (lambda (l)
    (letrec
        ((lm (lambda (l out)
               (cond
                 ((null? l) (quote ()))
                 ((atom? (car l)) (out (car l)))
                 (else (let ()
                         (lm (car l) out)
                         (lm (cdr l) out)))))))
      (call-with-current-continuation
       (lambda (skip)
         (lm l skip))))))

(quote '(============================================))
(leftmost-10 '(((a)) b (c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (if (atom?
            (call-with-current-continuation
             (lambda (oh)
               (rm a (car l) oh))))
           (cons (car l)
                 (rm a (cdr l) oh))
           (cons (rm a (car l) 0)
                 (cdr l)))))))

(define rember1*-4
  (lambda (a l)
    (if (atom?
         (call-with-current-continuation
          (lambda (oh)
            (rm a l oh))))
        l
        (rm a l (quote ())))))


(quote '(============================================))

(rember1*-4 'noodles '((food) more (food)))
(rember1*-4 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-4 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use let in rember1*-4

(define rember1*-5
  (lambda (a l)
    (let
        ((new-l
          (call-with-current-continuation
           (lambda (oh)
             (rm a l oh)))))
      (if (atom? new-l)
          l
          new-l))))

(quote '(============================================))

(rember1*-5 'noodles '((food) more (food)))
(rember1*-5 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-5 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;use let in rm

(define rm-1
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm-1 a (cdr l) oh))))
      (else
       (let ((new-car
              (call-with-current-continuation
               (lambda (oh)
                 (rm-1 a (car l) oh)))))
         (if (atom? new-car)
             (cons (car l)
                   (rm-1 a (cdr l) oh))
             (cons new-car (cdr l))))))))


(define rember1*-6
  (lambda (a l)
    (let
        ((new-l
          (call-with-current-continuation
           (lambda (oh)
             (rm-1 a l oh)))))
      (if (atom? new-l)
          l
          new-l))))
(quote '(============================================))

(rember1*-6 'noodles '((food) more (food)))
(rember1*-6 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-6 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protect rm

(define rember1*-7
  (lambda (a l)
    (letrec
        ((rm (lambda (a l oh)
               (cond
                 ((null? l) (oh (quote no)))
                 ((atom? (car l))
                  (if (eq? (car l) a)
                      (cdr l)
                      (cons (car l)
                            (rm a (cdr l) oh))))
                 (else
                  (let ((new-car
                         (call-with-current-continuation
                          (lambda (oh)
                            (rm a (car l) oh)))))
                    (if (atom? new-car)
                        (cons (car l)
                              (rm a (cdr l) oh))
                        (cons new-car (cdr l)))))))))
      (let ((new-l
             (call-with-current-continuation
              (lambda (oh)
                (rm a l oh)))))
        (if (atom? new-l)
            l
            new-l)))))

(quote '(============================================))
(rember1*-7 'noodles '((food) more (food)))
(rember1*-7 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-7 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Understand letrec, letecc ,and let with multiple vlaues
(quote '(============================================))


(let ((a (quote little)))
  (quote 'schemer)
  a)

;;(begin f1 f2 ... fn) evaluates f1 ... fn in turn and then returns the value of fn.
(begin
  (quote 'schemer)
  'a)


(letrec ((a (lambda (x)
              (print x))))
  (quote 'season)
  (a 'schemer))

;(try x y z)
;=
;(letcc sucess
;  (letcc x
;    (sucess y))
;  z)
(quote '(============================================))

(call-with-current-continuation
 (lambda (hop)
   (quote daniel)))

(call-with-current-continuation
 (lambda (hop)
   (quote daniel)
   (quote friedman)))

(call-with-current-continuation
 (lambda (success)
   (call-with-current-continuation
    (lambda (oh)
      (success (oh 'no))))))

(call-with-current-continuation
 (lambda (success)
   (call-with-current-continuation
    (lambda (oh)
      (success 'yes)))))

(call-with-current-continuation
 (lambda (success)
   (call-with-current-continuation ;; This letcc has a value = 'no
    (lambda (oh)
      (success (oh 'no)))) ;;success will be skipped by call to oh
   (quote L)));;This is the value executed and returned

(call-with-current-continuation
 (lambda (success)
   (call-with-current-continuation ;; this letcc will be skipped by the call for success
    (lambda (oh)
      (success (quote (1 2 3)))));;this is the value returned
   (quote L))) ;;this will not be executed 

(quote '(============================================))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Define try using two letcc
;;http://community.schemewiki.org/?seasoned-schemer

;(define-syntax letcc 
;  (syntax-rules () 
;    ((letcc var body ...) 
;     (call-with-current-continuation 
;       (lambda (var)  body ... ))))) 
;
; (define-syntax try 
;  (syntax-rules () 
;    ((try var a . b) 
;     (letcc success 
;       (letcc var (success a)) . b)))) 

;define try using call-with-current-continuation
(define-syntax try 
  (syntax-rules () 
    ((try var a b)
     (call-with-current-continuation
      (lambda (success)
        (call-with-current-continuation
         (lambda (var)
           (success a)))
        b)))))


(define rm-2
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm-2 a (cdr l) oh))))
      (else
       (try oh2
            (cons (rm-2 a (car l) oh2)
                  (cdr l))
            (cons (car l)
                  (rm-2 a (cdr l) oh)))))))


(define rember1*-8
  (lambda (a l)
    (try oh (rm-2 a l oh) l)))

(rember1*-8 'noodles '((food) more (food)))
(rember1*-8 'salad '((Swedish rye)
                     (French (mustard salad tureky))
                     salad))

(rember1*-8 'meat '((pasta meat)
                    pasta (noodles meat sauce)
                    meat tomatoes))

