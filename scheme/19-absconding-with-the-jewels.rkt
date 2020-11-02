#lang scheme
;;Chapter 19. Absconding with the Jewels
;;Note: use c-x c-e to evaluate one line at time instead of using C-c C-c. Check racket mode help (C-x C-e racket-send-last-sexp)

(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deep (sub1 m))
                  (quote ()))))))

;(deep 6)

(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons p (quote ()))
         (quote ()))
        (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

;(six-layers 'Neapolitan)

(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

;(four-layers 'pizza)


(define toppings '())

(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (call-with-current-continuation
        (lambda (jump)
          (set! toppings jump)
          (quote pizza))))
      (else (cons (deepB (sub1 m))
                  (quote ()))))))


;;(deepB 6)
;; (toppings 'mozzarella)
;; (toppings 'cake)
;; (toppings (quote pizza))

;Add another layer to the cake
;;(cons (toppings 'cake) (quote ()))

;; (cons
;;  (cons
;;   (cons (toppings (quote mozzarella))
;;         (quote ()))
;;   (quote ()))
;;  (quote ()))

;; (deepB 4)
;; (cons
;;  (cons
;;   (cons (toppings (quote mozzarella))
;;         (quote ()))
;;   (quote ()))
;;  (quote ()))

;;Just example to understand jump. jump remember the call stack ?!
(define deepK
  (lambda (m)
    (cond
      ((zero? m)
       (call-with-current-continuation
        (lambda (jump)
          (set! toppings jump)
          (quote 'pizza))))
      (else (cons (deepK (sub1 m))
                  (cons 6 (quote ())))))))
;; (deepK 6)
;; (toppings 'mozza)

;; (cons (toppings (quote cake))
;;       (toppings (quote cake)))


;; (cons (toppings (quote cake))
;;       (cons (toppings (quote mozzarella))
;;             (cons (toppings (quote pizz))
;;                   (quote ()))))

(define deep&co
  (lambda (m k)
          (cond
            ((zero? m) (k (quote pizza)))
            (else
             (deep&co (sub1 m)
                      (lambda (x)
                        (k (cons x (quote ())))))))))

;; (deep&co 0 (lambda (x) x))
;; (deep&co 6 (lambda (x) x))
;; (deep&co 2 (lambda (x) x))

;; expanding (deep&co 2 ..) collector
(define p1
  (lambda (x) x))

(define p2
 (lambda (x)
   (p1 (cons x (quote ())))))

;; (p2 'pizza)

(define p3
  (lambda (x)
    (p2 (cons x (quote ())))))

;; (p3 'pizza)

;; p3 can be written as two-layers
(define two-layers
  (lambda (p)
    (cons
     (cons p (quote ()))
     (quote ()))))

;; (two-layers 'pizza)

;;p2 can be replaced by
(define p2-new
  (lambda (x)
    (cons x (quote ()))))

;; (p2-new 'pizza)

;;replace p2 in p3 by p2-new
;; (define p3-new
;;   (lambda (x)
;;     (p2-new (cons x (quote ())))))

;; apply p2-ew on (cons x (quote ())), will result on two layers
(define p3-new
  (lambda (x)
    (cons
     (cons x (quote ()))
     (quote ()))))

(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! toppings k)
         (k (quote pizza))))
      (else
       (deep&coB (sub1 m)
                 (lambda (x)
                   (k (cons x (quote ())))))))))

;; (deep&coB 2 (lambda (x) x))
;; (deep&coB 6 (lambda (x) x))
;; (deep&coB 4 (lambda (x) x))

;; (cons (toppings (quote cake))
;;       (toppings (quote cake)))


;; (define two-in-a-row?
;;   (lambda (lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else (two-in-a-row-b? (car lat)
;;                             (cdr lat))))))

;; (define two-in-a-row-b?
;;   (lambda (a lat)
;;     (cond
;;       ((null? lat) #f)
;;       (else (or (eq? (car lat) a)
;;                 (two-in-a-row-b? (car lat) (cdr lat)))))))


(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else
               (let ((nxt (car lat)))
                 (or (eq? nxt a)
                     (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

;; (two-in-a-row? '(1 2 2 3))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define leave '())
(define walk
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (leave (car l)))
      (else
       (let ()
         (walk (car l))
         (walk (cdr l)))))))

;; walk is the minor function lm in leftmost
(define leftmost
  (lambda (l)
    (call-with-current-continuation
     (lambda (skip)
       (letrec ((lm (lambda (l)
                      (cond
                        ((null? l) (quote ()))
                        ((atom? (car l))
                         (skip (car l)))
                        (else (let ()
                                (lm (car l))
                                (lm (cdr l))))))))
         (lm l))))))


(define start-it
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (walk l)))))

;; (start-it  '((potato) (chips (chips (with))) fish) )
;; (leave 'pizza)

(define fill '())

;;uncomment println to understand
(define waddle
  (lambda (l)
    (cond
      ((null? l)
       (let ()
         (println 'null)
         (quote ())))
      ((atom? (car l))
       (let ()
         (println 'atom)
         (println l)
         (println "car l")
         (println (car l))
         (call-with-current-continuation
          (lambda (rest)
            (set! fill rest)
            (leave (car l))))
         (println 'rest)
         (println l)
         (println "cdr l")
         (println (cdr l))
         (waddle (cdr l))))
      (else (let ()
              (println 'list)
              (println l)
              (println "car l")
              (println (car l))
              (println "cdr l")
              (println (cdr l))
              (println "waddle (car l)")
              (waddle (car l))
              (println "waddle (cdr l)")
              (println l)
              (println "cdr l")
              (println (cdr l))
              (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (waddle l)
       (println  "start-it2") ;;comment to see the final value when l is exhausted = ()
       ))))

;; (start-it2 '((donuts)
;;              (cheerios (cheeriosss (spaghettios)))
;;              donut))

;; rest of the list
(define l1 '(()
         (cheerios (cheeriosss (spaghettios)))
         donut))

;; function corresponds to rest
(define rest1
  (lambda (x)
    (println rest1)
    (waddle l1)))

(define get-next
  (lambda (x)
    (call-with-current-continuation
     (lambda (here-again)
       (set! leave here-again)
       (fill (quote go))))))

(start-it2 '((donuts)
             (cheerios (cheeriosss (spaghettios)))
             donut))
(get-next (quote go))
(rest1 (quote go))

(define rest2
  (lambda (x)
    (waddle l2)))
(define l2 '(((cheeriosss (spaghettios)))
         donut))
;; (rest2 (quote go))


;;Does get-next deserves its name
(start-it2 '((donuts)
             (cheerios (cheeriosss (spaghettios)))
             donut))
(get-next (quote go))
(get-next (quote go))
(get-next (quote go))
(get-next (quote go))
(get-next (quote go)) ;;Wow! -> get back to start-it2


(define get-first
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (waddle l)
       (println "get-first")
       (leave (quote ()))))))

;; For illustratio only. Not to use with two-in-a-row. Will not return a value. Will print instead
(define get-next-2
  (lambda (x)
    (let ()
      (call-with-current-continuation
       (lambda (here-again)
         (set! leave here-again)
         (fill (quote go))))
      (println "get-next-2")
      )))

;;Compare start-it2 vs get-first

;;using start-it2, when the list is exhausted, we will eventually return from start-it2
(start-it2 '((donuts)
             (cheerios (cheeriosss (spaghettios)))
             donut))

;;using get-first, when the list is exhausted, we will eventually return from get-next-2
(get-first '((donuts)
             (cheerios (cheeriosss (spaghettios)))
             donut))
(get-next-2 (quote go))
(get-next-2 (quote go))
(get-next-2 (quote go))
(get-next-2 (quote go))
(get-next-2 (quote go))


(get-first '())
(get-first '(donut))
(get-next (quote go))

(get-first '(fish (chips)))
(get-next (quote go))
(get-next (quote go))


(get-first '(fish (chips) chips))
(get-next (quote go))
(get-next (quote go))


(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next (quote go))))
      (if (atom? n)
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))

;;The Thirteenth Commandment
(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 0)))
               (if (atom? n)
                   (or (eq? n a)
                       (T? n))
                   #f))))
       (get-next
        (lambda (x)
          (call-with-current-continuation
           (lambda (here-again)
             (set! leave here-again)
             (fill (quote go))))))
       (fill (lambda (x) x))
       (waddle
        (lambda (l)
          (cond
            ((null? l) (quote ()))
            ((atom? (car l))
             (let ()
               (call-with-current-continuation
                (lambda (rest)
                  (set! fill rest)
                  (leave (car l))))
               (waddle (cdr l))))
            (else (let ()
                    (waddle (car l))
                    (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (call-with-current-continuation
                  (lambda (here)
                    (set! leave here)
                    (waddle l)
                    (println "get-first")
                    (leave (quote ()))))))
        (if (atom? fst) (T? fst) #f)))))

(two-in-a-row*? '(((food) ()) (((food)))))

;;Helfpul reference: https://stackoverflow.com/questions/42044943/get-first-the-seasoned-schemer
