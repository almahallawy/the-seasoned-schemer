#lang scheme
;;Chapter 19. Absconding with the Jewels

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


;; (deepB 6)
;; (toppings 'mozzarella)
;; (toppings (quote pizza))
;; (cons (toppings 'cake) (quote ()))
;; (cons
;;  (cons
;;   (cons (toppings (quote mozzarella))
;;         (quote ()))
;;   (quote ()))
;;  (quote ()))

(deepB 4)
(cons (toppings (quote cake))
      (toppings (quote cake)))

