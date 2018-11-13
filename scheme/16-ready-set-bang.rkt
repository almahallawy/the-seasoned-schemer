#lang racket

;;Chapter 16. Ready, Set, Bang!


(define sweet-tooth
  (lambda (food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(define last (quote angelfood))

(sweet-tooth (quote chocolate))

(sweet-tooth (quote fruit))

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothL (quote chocolate))

last

(sweet-toothL (quote fruit))

last

(sweet-toothL (quote chees))

(sweet-toothL (quote carrot))

(define ingredients (quote ()))

(define sweet-toothR
  (lambda (food)
    (set! ingredients
          (cons food ingredients))
    (cons food
          (cons (quote cake)
                (quote ())))))

(quote '(===========Sweet-toothR=============))

(sweet-toothR (quote chocolate))

ingredients 

(sweet-toothR (quote fruit))

ingredients

(sweet-toothR (quote chees))

ingredients

(sweet-toothR (quote carrot))

ingredients

;;Using 16 Commandment
(quote '(===========Sweet-toothR Using 16 Commandment=============))
(define sweet-toothR1
  (let ((ingredients (quote ())))
    (lambda (food)
      (set! ingredients
            (cons food ingredients))
      (print ingredients)
      (cons food
            (cons (quote cake)
                  (quote ()))))))


(sweet-toothR1 'chocolate)

(sweet-toothR1 'fruit)

(sweet-toothR1 'carrot)

(sweet-toothR1 'cheese)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define deep-1
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deep-1 (sub1 m))
                  (quote ()))))))

(quote '(===========deep=============))

(deep-1 3)
(deep-1 7)
(deep-1 0)


(define Ns (quote ()))

(define deepR
  (lambda (n)
    (set! Ns (cons n Ns))
    (deep-1 n)))

(deepR 3)

Ns

(deepR 7)

Ns

;;clearn Ns
(set! Ns (quote ()))

;;Remember All the resutls
(quote '(==============Remember All the resutl============))

(define Rs (quote ()))

(define deepR-1
  (lambda (n)
    (set! Rs (cons (deep-1 n) Rs))
    (set! Ns (cons n Ns))
    (deep-1 n)))


(deepR-1 3)

Ns
Rs

(deepR-1 7)

Ns
Rs

 ;;rest Rs & Ns
(set! Ns (quote ()))
(set! Rs (quote ()))


(quote '(===========deep-R2=================))

;;Using the fifteenth Commadments
(define deepR-2
  (lambda (n)
    (let ((result (deep-1 n)))
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(deepR-2 3)

Ns
Rs

(deepR-2 5)

Ns
Rs

(deepR-2 3)

Ns
Rs

;; The Nineteenth Commandment

;;Find: n guranteed to be in Ns
(define find-1
  (lambda (n Ns Rs)
    (letrec ((A (lambda (ns rs)
                  (cond
                    ((= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define member?
  (lambda (a lat)
    (letrec ((M (lambda (lat)
                  (cond
                    ((null? lat) #f)
                    (else (or (eq? (car lat) a)
                              (M (cdr lat))))))))
      (M lat))))

(member? 4 '(1 2  5 6 ))


(define deepM-1
  (lambda (n)
    (if (member? n Ns)
        (find-1 n Ns Rs)
        (deepR-2 n))))

;;remove duplicates from Ns & Rs

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

Ns
Rs


;;No need for deepR-2

(define deepM-2
  (lambda (n)
    (if (member? n Ns)
        (find-1 n Ns Rs)
        (let ((result (deep-1 n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(quote '(===========deep-M=================))

(deepM-2 3)
Ns
Rs

(deepM-2 6)
Ns
Rs

;;change the recursion in deep


(define deepM-3
  (lambda (n)
    (if (member? n Ns)
        (find-1 n Ns Rs)
        (let ((result (deep-2 n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

  
(define deep-2
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deepM-3 (sub1 m))
                  (quote ()))))))

(quote '(::::::::::::::::::Change recursion in deep))
(deepM-3 9)

Ns
Rs


;;introduce Ns Rs by let
(define deepM-4
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (if (member? n Ns)
          (find-1 n Ns Rs)
          (let ((result (deep-3 n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(define deep-3
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deepM-4 (sub1 m))
                  (quote ()))))))

(deepM-4 16)

;; Modify find to handle in n is not in Ns
(define find
  (lambda (n Ns Rs)
    (letrec ((A (lambda (ns rs)
                  (cond
                    ((null? ns) #f)
                    ((= (car ns) n) (car rs))
                    (else
                     (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

;;No need for member? now. Use find

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define deepM-5
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (if (atom? (find-1 n Ns Rs))
          (let ((result (deep-3 n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          (find-1 n Ns Rs)))))

;;find is duplicate. use let

(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons result Ns))
              result)
            (find n Ns Rs))))))

(define deep
  (lambda (n)
    (cond
      ((zero? n) (quote pizza))
      (else (cons (deepM (sub1 n))
                  (quote ()))))))

(quote (=================final deepM=============))

(deepM 17)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define length-1
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length-1 (cdr l)))))))

(define length-2
  (lambda (l)
    0))

(set! length-2
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length-2 (cdr l)))))))


(length-2 '(1 2 3 4 5))


(define length-3
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (h (cdr l)))))))
    h))

(length-3 '(1 2 3 4 5 6 7 ))


(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))


(define length-4
  (let ((h (lambda (l) 0)))
    (set! h
          (L (lambda (arg) (h arg))))
    h))

(length-4 '(1 2 3 4))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) (quote ()))))
      (set! h
            (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

(define length-5 (Y! L))

(length-5 '(1 2 3 4 5))

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*-old
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-old (cdr l)))
      (else
       (let ((a (add1 (depth*-old (car l))))
             (d (depth*-old (cdr l))))
         (max d a))))))


(define D
  (lambda (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s))
         (depth* (cdr s)))
        (else (max
               (add1 (depth* (car s)))
               (depth* (cdr s))))))))


         
(define depth* (Y! D))

(depth* '((pickled) peppers (peppers pickled)))


(depth* '(c (b (a b) a) a))


;Y combinator - Applicative-Order Y
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define length_6 (Y L))

(length_6 '(1 2 3 4 5 6))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

(println '((Y biz) 5))
((Y biz) 5)
;;((Y biz) 5) ; NO answer, will yield infinite recursion call
;;because x = 5 before calling ((Y biz) 5) one more time, so X will keep increasing and will never hav a = x

;;((Y! biz) 5) ;;NO Answer

;;why Y! brings now answer???
(quote '(===========================))

(define x 0)

(define biz1
  (lambda (f)
    (set! x (add1 x))
    (lambda (a)
      (if (= a x)
          0
          (f a)))))

(println '((Y biz1) 5))
((Y biz1) 5)
;((Y! biz1) 5) ;;NO answer

(set! x 0)

(define length-7
  (let ((h (lambda (l) 0)))
    (set! h
         (biz1 (lambda (arg) (h arg))))
    h))


;;(length-7 5) ;;Still no answer!!

(define h1 (lambda (l) (quote ())))

(define length-8
  (let ()
    (set! h1
          (biz1 (lambda (arg) (h1 arg))))
    h1))

;;(length-8 5) ;; No answer

(define h2 (lambda (l) (quote ())))
(set! h2
      (biz1 (lambda (arg) (h2 arg))))


;(h2 5) ;;no answer

(set! x 0)
(define Nx (quote ()))
(define y 0)

(define biz2
  (lambda (f)
    (set! y (add1 y))
    ;(print 'biz2)
    (lambda (a)
      (set! x (add1 x))
      ;(print 'biz2)
      (set! Nx (cons x Nx))
      (if (= a x)
          0
          (f a)))))

(define h3 (lambda (l) (quote ())))

(set! h3
      (biz2 (lambda (arg) (h3 arg))))

(quote '(-----------------------))
(h3 5) ;;works
;Nx

; So not just the shap of the function provided to Y and Y! cause the infinite loop.
;;it seems if the name in the set used in the recursive funciton body, it will cause infintie loop

(quote '(_____________________________))

(define biz3
  (lambda (f)
    (set! x (add1 x))
    (println 'biz)
    (lambda (a)
      (println 'biz3)
      (println x)
      (if (= a x)
          (println 'answer)
          (f a)))))

(define h4 (lambda (l) (quote ())))

(set! x 4)  ;with (h4 5) will work, note that (add1 x) when evaluating (set! h4..)
x

(set! h4
      (biz3 (lambda (arg) (h4 arg))))
x

(println '(h4 5))
(h4 5) ;answer

(println '(h4 6))
(set! x 6)
(h4 6)  ;answer

(println '((Y biz3) 5))
(set! x 0)
((Y biz3) 5)
x

(set! x 0)
;;(h4 6)  ;inifinte loop. biz3 ll be printed infinitly. And x is not increased, which means (set! x (add1 x)) is not executed
;((Y! biz3) 5)

;;Comparing ((Y biz3) 5) vs ((Y! biz3) 5) or (h4 5)) shows that (set! x (add1 x)) is executed in Y with every recursion call but it is  not executed in Y1 or h4 with every recursion call.

;; When evaluting (set! h4 ..)  or (Y! biz3) , (set! x (add1 x)) evaluted only once, but it is not evaluated when we recur using h4 or Y! because (set! x (add1 x))  doesn't exist anymore. Why?

;;Because the VALUE of  (biz3 (lambda (arg) (h4 arg))) or the VALUE (biz3 (lambda (arg) (h arg))) in (Y! biz3)  is the (lambda (a) ...) only, (set! x (add1 x)) is evaluated once and its value is discarded after evaluating (biz3 (lambda (arg) (h4 arg)));
;;check page 93 for lambda with two expressions

;;that is why x is not increased and we kept recurring  (f a) inifnite times because x is not changed closer to termination which violated Fourth Commadment

;;However in Y , (set! x (add1 x)) exists and we dont' have this problem.


(Y biz3)

;==>

((lambda (le)
  ((lambda (f) (f f))
   (lambda (f)
     (le (lambda (x) ((f f) x))))))
 biz3)

;==>

((lambda (f) (f f))
 (lambda (f)
   (biz3 (lambda (x) ((f f) x)))))


;======
(println '((Y biz3) 2)) 
(set! x 0)

((Y biz3) 2)

;==>
(println '(lambda (f) (f f)))
(set! x 0)

(((lambda (f) (f f))
 (lambda (f)
   (biz3 (lambda (x) ((f f) x)))))
 2)

;==>
(set! x 0)
(println '((f f) 2))

(((lambda (f)
   (biz3 (lambda (x) ((f f) x))))
 (lambda (f)
   (biz3 (lambda (x) ((f f) x)))))
 2)

;==>
(set! x 0)
(println '(biz3 (lambda x)))

((biz3 (lambda (x)
         (((lambda (f)
             (biz3 (lambda (x) ((f f) x))))
           (lambda (f)
             (biz3 (lambda (x) ((f f) x))))) x)))
 2)


