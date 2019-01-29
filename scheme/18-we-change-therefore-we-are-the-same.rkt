#lang scheme

;Chapter 18. We Change, Therefore We Are the Same!

; Use until we define kons later.
(define kons
  (lambda (x y)
    (cons x y)))

(define kdr
  (lambda (l)
    (cdr l)))

(define kar
  (lambda (l)
    (car l)))


(define lots
  (lambda (m)
    (cond
     ((zero? m) (quote ()))
     (else (kons (quote egg)
                 (lots (sub1 m)))))))

;(lots 3)
;(lots 5)

(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (kdr l)))))))

;; (lenkth (lots 3))
;; (lenkth (lots 5))

;create a list of four eggs from (lots 3)
;(kons (quote eggs) (lots 3))

(define add-at-end
  (lambda (l)
    (cond
      ((null? (kdr l))
       (kons (kar l)
             (kons (quote eeggs)
                   (quote ()))))
      (else
       (kons (kar l)
             (add-at-end (kdr l)))))))

;;(add-at-end (lots 5))


;konsC is to consC what kons is to cons
;; (define counter '())
;; (define set-counter '())

;; (define consC
;;   (let ((N 0))
;;     (set! counter
;;           (lambda ()
;;             N))
;;     (set! set-counter
;;           (lambda (x)
;;             (set! N x)))
;;     (lambda (x y)
;;       (set! N (add1 N))
;;       (cons x y))))

;; (set-counter 0)
;; (consC 'egg
;;        (consC 'egg
;;               (consC 'egg
;;                      (consC 'egg '()))))
;; (counter)

(define kounter '())
(define set-kounter '())

(define konsC
  (let ((N 0))
    (set! kounter
          (lambda ()
            N))
    (set! set-kounter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))


(define add-at-end-1
  (lambda (l)
    (cond
      ((null? (kdr l))
       (konsC (kar l)
             (konsC (quote eeggs)
                   (quote ()))))
      (else
       (konsC (kar l)
             (add-at-end-1 (kdr l)))))))

;; (set-kounter 0)
;; (add-at-end-1 (lots 3))
;; (kounter)


;Add an egg at the end without making any new kons except for the last one
(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
                ((null? (kdr ls))
                 (set-cdr! ls
                          (kons (quote egg)
                                (quote ()))))
                (else (A (kdr ls)))))))
      (A l)
      l)))
    

