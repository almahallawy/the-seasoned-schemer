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

(quote '(===========Sweet-toohR=============))

(sweet-toothR (quote chocolate))

ingredients 

(sweet-toothR (quote fruit))

ingredients

(sweet-toothR (quote chees))

ingredients

(sweet-toothR (quote carrot))

ingredients


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deep (sub1 m))
                  (quote ()))))))

(quote '(===========deep=============))

(deep 3)

(deep 7)

(deep 0)


(define Ns (quote ()))

(define deepR
  (lambda (n)
    (set! Ns (cons n Ns))
    (deep n)))

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
    (set! Rs (cons (deep n) Rs))
    (set! Ns (cons n Ns))
    (deep n)))


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

;;Using the 15th Commadments
(define deepR-2
  (lambda (n)
    (let ((result (deep n)))
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



