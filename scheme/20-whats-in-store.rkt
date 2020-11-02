#lang scheme
;;Chapter 20. What's in Store?

;;Note: use c-x c-e to evaluate one line at time instead of using C-c C-c. Check racket mode help (C-x C-e racket-send-last-sexp)


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


(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (eq? (car e) (quote define)))
      (else #f))))

(define *define
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
;-----------------------------------;
    (lambda (sel)                   ;
      (sel it (lambda (new)         ; ===> box
                (set! it new))))))  ;
;-----------------------------------;



;; sel = (lambda (it set) (set new))
;; set = (lambda (new) (set! it new))
(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

;; Unfolding (box (lambda (it set) (set new)))))
;;  ((lambda (it set) (set new))
;;   it (lambda (new) (set! it new)))

;; ((lambda (new) (set! it new))
;;  new)

;; (set! it new)

;; sel = (lambda (it set) it)
(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

;; unfolding (box (lambda (it set) it))
;; ((lambda (it set) it)
;;  it (lambda (new)
;;       (set! it new)))

;; it


(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define meaning
  (lambda (e table)
    ((expression-to-action e)
     e table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox
     ((lookup table (name-of e)) ;;box paired with name whose value is to be changed
      (meaning (right-side-of e) table))))) ;; value of the right-hand side of (set! ...)

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend
               (formals-of e)
               (box-all args)
               table)))       ))

(define beglis
  (lambda (es table)
    (cond
      ((null? (cdr es))
       (meaning (car es) table))
      (else ((lambda (val)
               (beglis (cdr es) table))
             (meaning (car es) table))))))

(define box-all
  (lambda (vals)
    (cond
      ((null? vals) (quote ()))
      (else (cons (box (car vals))
                  (box-all (cdr vals)))))))

;;Remember Pg 66.
;; (let ...) is an abbreviation:
;; (let((x1 y1)  ...(xn yn)) B ...)
;; = ((lambda (x1 ... xn) B ...) y1 ... yn))

;; ((lambda (val) ...)
;;  (meaning (car es) table))
;; ;; =
;; (let ((vale (meaning (car es) table))) ...)

(let ((x 1)) (+ x 10))
;=
((lambda (x)
   (+ x 10)) 1)



(let ((x 1) (y 10)) (+ x y))
;=
((lambda (x y) (+ x y)) 1 10)


;; beglis using let instead of lambda
(define beglis1
  (lambda (es table)
    (cond
      ((null? (cdr es))
       (meaning (car es) table))
      (else (let ((val (meaning (car es) table))) ;; val is ignored
              (beglis1 (cdr es) table))))))

(define multi-extend
  (lambda (names values table)
    (cond
      ((null? names) table)
      (else
       (extend (car names) (car values)
               (multi-extend
                (cdr names)
                (cdr values)
                table))))))

(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       ((lambda (val)
          (cons val
                (evlis (cdr args) table)))
        (meaning (car args) table))))))


(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))

(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))


(define *const1
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      ((eq? e (quote cons))
       (b-prim cons))
      ((eq? e (quote car))
       (a-prim car))
      ((eq? e (quote cdr))
       (a-prim cdr))
      ((eq? e (quote eq?))
       (b-prim eq?))
      ((eq? e (quote atom?))
       (a-prim atom?))
      ((eq? e (quote null?))
       (a-prim null?))
      ((eq? e (quote zero?))
       (a-prim zero?))
      ((eq? e (quote add1))
       (a-prim add1))
      ((eq? e (quote sub1))
       (a-prim sub1))
      ((eq? e (quote number?))
       (a-prim number?)))))

(define *const2
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:null? (a-prim null?))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))
        (:number? (a-prim number?)))
    (lambda (e table)
      (cond
        ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        ((eq? e (quote cons)) :cons)
        ((eq? e (quote car)) :car)
        ((eq? e (quote cdr)) :cdr)
        ((eq? e (quote null?)) :null?)
        ((eq? e (quote eq?)) :eq?)
        ((eq? e (quote atom?)) :atom?)
        ((eq? e (quote zero?)) :zero?)
        ((eq? e (quote add1)) :add1)
        ((eq? e (quote sub1)) :sub1)
        ((eq? e (quote number?) :number?))))))


(define *const
  ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
     (lambda (e table)
       (cond
         ((number? e) e)
         ((eq? e #t) #t)
         ((eq? e #f) #f)
         ((eq? e (quote cons)) :cons)
         ((eq? e (quote car)) :car)
         ((eq? e (quote cdr)) :cdr)
         ((eq? e (quote null?)) :null?)
         ((eq? e (quote eq?)) :eq?)
         ((eq? e (quote atom?)) :atom?)
         ((eq? e (quote zero?)) :zero?)
         ((eq? e (quote add1)) :add1)
         ((eq? e (quote sub1)) :sub1)
         ((eq? e (quote number?)) :number?))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (a-prim null?)
   (b-prim eq?)
   (a-prim atom?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))


(define *letcc
  (lambda (e table)
    (call-with-current-continuation
     (lambda (skip)
       (beglis (ccbody-of e)
               (extend
                (name-of e)
                (box (a-prim skip))
                table))))))


(define value
  (lambda (e)
    (call-with-current-continuation
     (lambda (the-end)
       (set! abort the-end)
       (cond
         ((define? e) (*define e))
         (else (the-meaning e)))))))

(define abort '())

(define the-empty-table
  (lambda (name)
    (abort
     (cons (quote no-answer)
           (cons name (quote ()))))))

(define global-table
  (lambda (name)
    (the-empty-table name)))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))


(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote letcc))
          *letcc)
         ((eq? (car e) (quote set!))
          *set)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))

(define text-of
  (lambda (x)
    (car (cdr x))))

(define formals-of
  (lambda (x)
    (car (cdr x))))

(define body-of
  (lambda (x)
    (cdr (cdr x))))

(define ccbody-of
  (lambda (x)
    (cdr (cdr x))))

(define name-of
  (lambda (x)
    (car (cdr x))))

(define right-side-of
  (lambda (x)
    (cond
      ((null? (cdr (cdr x))) 0)
      (else (car (cdr (cdr x)))))))

(define cond-lines-of
  (lambda (x)
    (cdr x)))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (car (cdr x))))

(define function-of
  (lambda (x)
    (car x)))

(define arguments-of
  (lambda (x)
    (cdr x)))


;; Try the interpreter

;; (value 1)

;; (value 'z)

;; (value
;;  '(cond (else 0)))

;; (value
;;  '(cond
;;     ((null? (cons 0 (quote ()))) 0)
;;     (else 1)))

;; (value
;;  '(value 1))

;; (value
;;  '(car (cons 0 (quote ()))))


;; (value
;;  '(define ls
;;     (cons
;;      (cons
;;       (cons 1 (quote ()))
;;       (quote ()))
;;      (quote ()))))

;; (value 'ls)

;; (value
;;  '(car (car (car ls))))


;; (value
;;  '(car '(1 3)))

;; (value
;;  '(cdr '(1 3)))

;; (value '(null? '()))

;; (value '(eq? 3 4))
;; (value '(eq? 3 3))

;; (value '(add1 1))
;; (value '(sub1 1))

;; (value '(zero? 0))
;; (value '(zero? 1))

;; (value '(number? 3))


;; (value '(define xx 3))
;; (value 'xx)

;; (value '(lambda (x) 5))
;; (value '((lambda (x) 5)

;;(value '(define pp (lambda (x) 5)))
;; (value '(pp 2))

(value
 '(define odd?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (even? (sub1 n)))))))

(value
 '(define even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (odd? (sub1 n)))))))

(value '(odd? 3))
(value '(even? 2))

(value '(odd? 2))
(value '(even? 3))
