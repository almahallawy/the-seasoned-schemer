#lang scheme
;;Chapter 20. What's in Store?

;;Note: use c-x c-e to evaluate one line at time instead of using C-c C-c. Check racket mode help (C-x C-e racket-send-last-sexp)


(define initial-table
  (lambda (name)
    (car (quote ())))) ;;breaks The Law of Car

;; (define the-empty-table
;;   (lambda (name)
;;     ...))

;;table is a function
(define lookup
  (lambda (table name)
    (table name))) ;;apply table to the name

;;returns table: (lambda (name2) ....)
(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        ((eq? name2 name1) value)
        (else (table name2))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define x 3)
x
(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (eq? (car e) (quote define)))
      (else #f))))


(define global-table '()))

(define define*
  (lambda (e)
    (set! global-table
          (extend
           (name-of e)
           (box
            (the-meaning
             (right-side-of e)))
           global-table))))

(define box
  (lambda (it)
    (lambda (sel)
      (set it (lambda (new)
                (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))
