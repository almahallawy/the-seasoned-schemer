#lang scheme
;Chapter 18. We Change, Therefore We Are the Same!

;;Install SICP Support for DrRacket in order to get support for set-cdr!
;;https://docs.racket-lang.org/sicp-manual/index.html
(require sicp)


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

;Add an egg at the other end of the list
(define add-at-end
  (lambda (l)
    (cond
      ((null? (kdr l))
       (konsC (kar l)
             (kons (quote eggee)
                   (quote ()))))
      (else
       (konsC (kar l)
             (add-at-end (kdr l)))))))

;; (set-kounter 0)
;; (add-at-end (lots 3))
;; (kounter)

;Add an egg at the end without making any new kons except for the last one
;This will fail if we use set-cdr! in Racket. Check http://blog.racket-lang.org/2007/11/getting-rid-of-set-car-and-set-cdr.html
;;That is why we include (require sicp)
(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
                ((null? (kdr ls))
                 (set-cdr! ls
                          (kons (quote eggee)
                                (quote ()))))
                (else (A (kdr ls)))))))
      (A l)
      l)))
    

;;(add-at-end-too (lots 3))

;From Little Schemer -  Chapter 6
;Another representations fo numbers
;Zero (), One (()), Two (() ()), Three (() () ())

;; (define sero?
;;   (lambda (n)
;;     (null? n)))

;; (define edd1
;;   (lambda (n)
;;     (cons (quote ()) n)))

;; (define zub1
;;   (lambda (n)
;;     (cdr n)))

;Return (lambda (selector)..)
(define kons1
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr)))) ;call selector with kar and kdr arguments

(define kar1
  (lambda (c)
    (c (lambda (a d) a)))) ;applies selector (lambda (a d) a) on (a d) and returns a (car)


(define kdr1
  (lambda (c)
    (c (lambda (a d) d)))) ;applies selector (lambda (a d) d) on (a d) and returns d (cdr)

;;Thanks to https://github.com/pkrumins/the-seasoned-schemer/blob/master/18-we-change-therefore-we-are-the-same.ss for the examples

;; (kons1 'a '())
;; (kar1 (kons1 'a '()))
;; (kdr1 (kons1 'a '()))
;; (kar1 (kons1 'a (kons 'b '())))


(define bons
  (lambda (kar)
    (let ((kdr (quote ())))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar2
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr2
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

;; (bons (quote egg))
;; (kar2 (bons (quote egg)))
;; (kdr2 (bons (quote egg)))
;; (set-kdr (bons (quote egg)) '())

;;Use set-kdr and bons to define kons
(define kons2
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))


;; (kons2 'a '(b c d))
;; (kar2 (kons2 'a '(b c d)))
;; (kdr2 (kons2 'a '(b c d)))

(define dozen (lots 12))

;dozen

(define bakers-dozen (add-at-end dozen))

;; bakers-dozen
;; dozen

(define bakers-dozen-too 
  (add-at-end-too dozen))

;; bakers-dozen-too
;; bakers-dozen
;; dozen

(define bakers-dozen-again
  (add-at-end dozen))

;; bakers-dozen-again
;; dozen

(define eklist?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) (null? ls2))
      ((null? ls2) #f)
      (else
       (and (eq? (kar ls1) (kar ls2))
            (eklist? (kdr ls1) (kdr ls2)))))))

;;(eklist? bakers-dozen bakers-dozen-too)


(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-cdr! c1 1)
      (set-cdr! c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-cdr! c1 t1)
        (set-cdr! c2 t2)
        v))))

;; dozen
;; (same? dozen dozen)

;; bakers-dozen-too
;; (same? dozen bakers-dozen-too)

;; bakers-dozen
;; (same? bakers-dozen bakers-dozen-too) ;;Not the same


;; (same?
;;  (kons (quote egg) (quote ()))
;;  (kons (quote egg) (quote ())))

;;returns the last kons in a non-empty kons-list
(define last-kons
  (lambda (ls)
    (cond
      ((null? (kdr ls)) ls)
      (else (last-kons (kdr ls))))))


;; Read the following to understand the result: https://people.csail.mit.edu/jaffer/r5rs/Pairs-and-lists.html
;;List: (a b c d e) = (a . (b . (c . (d . (e . ())))))
;;improper list (a b c . d) = (a . (b . (c . d)))
;;Note: The above definitions imply that all lists have finite length and are terminated by the empty list.

;;https://stackoverflow.com/questions/8629315/mysterious-scheme-procedure-for-cycling-through-lists
;; In the expression #0={1 2 3 . #0#}, think of #0= as an anchor and #0# as a link to that anchor - that is, in the list the first three elements are 1 2 3 but the fourth element is a pointer to the start of the list, therefore forming a three-element circular list.

;;https://www.scheme.com/csug7/intro.html#./intro:s28
;; Shared and cyclic structure may be printed using the graph mark and reference prefixes #n= and #n#. #n= is used to mark an item in the input, and #n# is used to refer to the item marked n. For example, '(#1=(a) . #1#) is a pair whose car and cdr contain the same list, and #0=(a . #0#) is a cyclic list, i.e., its cdr is itself.


(define long (lots 12))

;;long

;;(last-kons long)

(set-cdr! (last-kons long) long) ;=> #0={egg egg egg egg egg egg egg egg egg egg egg egg . #0#}

;;long 

;(kdr long)

;(lenkth long) ;;No answer

;; (set-cdr! (last-kons long) (kdr long)) ;=> {egg . #0={egg egg egg egg egg egg egg egg egg egg egg . #0#}}

;;long

;; (set-cdr! (last-kons long) (kdr (kdr long))) ;=> {egg egg . #0={egg egg egg egg egg egg egg egg egg egg . #0#}}

;; long

;;(lenkth long) ;;No Answer

(define finite-lenkth
  (lambda (p)
    (call-with-current-continuation
     (lambda (infinite)
       (letrec
           ((C (lambda (p q)
                 (print 'p=)
                 (println p)
                 (print 'q=)
                 (println q)
                 (println '===)
                 (cond
                   ((null? q) 0)
                   ((null? (kdr q)) 1)
                   ((same? p q)
                    (infinite #f))
                   (else
                    (+ (C (sl p) (qk q))
                       2)))))
            (qk (lambda (x) (kdr (kdr x))))
            (sl (lambda (x) (kdr x))))
         (cond
           ((null? p) 0)
           (else
            (add1 (C p (kdr p))))))))))

;(finite-lenkth long)

(define lk '(1 2 3 4 5))
lk
(set-cdr! (last-kons lk) lk)
lk
(finite-lenkth lk)

(define ll '(1 2 3))
(finite-lenkth ll)



;; Guy's Favorite Pie
(define mongo
  (kons (quote pie)
        (kons (quote a)
              (kons (quote la)
                    (kons (quote mode)
                          (quote ()))))))

mongo

(set-cdr! (kdr (kdr (kdr mongo))) (kdr mongo))

mongo
