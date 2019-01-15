#lang scheme

;Chapter 17, We Change, Therefore We are!

;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (cons (deep (sub1 m))
              (quote ())))))

;(deep 3)

(define find
  (lambda (n Ns Rs)
    (letrec ((A (lambda (ns rs)
                  (cond
                    ((null? ns) #f)
                    ((= (car ns) n) (car rs))
                    (else
                     (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM-1
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  (quote pizza)
                  (cons (D (sub1 m))
                        (quote ()))))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

;(deepM-1 5)

;;Help D with its work, D should refer to deepM instead of itself
(define deepM-2
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  (quote pizza)
                  (cons (deepM-2 (sub1 m))
                        (quote ()))))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))


;(deepM-2 7)

;(letrec ..) is not longer needed because D is no longer mentioned in the definition of D
(define deepM-3
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (let
        ((D (lambda (m)
              (if (zero? m)
                  (quote pizza)
                  (cons (deepM-3 (sub1 m))
                        (quote ()))))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))


;(deepM-3 10)

;;Only one let is needed because Ns and Rs do not appear in the definition of D
(define deepM-4
  (let ((Rs (quote ()))
        (Ns (quote ()))
        (D (lambda (m)
              (if (zero? m)
                  (quote pizza)
                  (cons (deepM-4 (sub1 m))
                        (quote ()))))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists)))))

;(deepM-4 3)

;;replace the one use of D byt the experssion it names
(define deepM-5
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   ((lambda (m)
                      (if (zero? m)
                          (quote pizza)
                          (cons (deepM-5 (sub1 m))
                                (quote ()))))
                    n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;(deepM-5 '2)

;;introduce a name to name another name
(define deepM-6
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (let ((m n))
                     (if (zero? m)
                         (quote pizza)
                         (cons (deepM-6 (sub1 m))
                               (quote ()))))))
                     (set! Rs (cons result Rs))
                     (set! Ns (cons n Ns))
                     result)
            exists)))))

;(deepM-6 3)

;;uname again because a name is replaced by a name
(define deepM-7
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (cons (deepM-7 (sub1 n))
                             (quote ())))))
                   
                     (set! Rs (cons result Rs))
                     (set! Ns (cons n Ns))
                     result)
            exists)))))

;(deepM-7 4)


(define consC-1
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep-1
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC-1 (deep-1 (sub1 m))
               (quote ())))))

;(deep-1 5)

(define counter '()) ;;(define counter) didn't work with DrRacket!!. 

(define consC-2
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep-2
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC-2 (deep-2 (sub1 m))
                 (quote ())))))

;(deep-2 5)

;(deep-2 7)

;(counter)


(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

;(supercounter deep-2)

(define set-counter '())

(define consC
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep-3
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC (deep-3 (sub1 m))
                (quote ())))))

;; (deep-3 5)
;; (deep-3 7)
;; (supercounter deep-3)
;; (set-counter 0)
;; (supercounter deep-3)

(define deepM-8
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (consC (deepM-8 (sub1 n))
                                (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;; (set-counter 0)
;; (deepM-8 5)
;; (counter)

;; (deepM-8 7)
;; (counter) ;;counter should be 7, not 12, because that is point of DeepM

;; (supercounter deepM-8)

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
                ((null? l) (oh (quote no)))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (cons (car l)
                           (R (cdr l) oh))))
                (else
                 (let ((new-car
                        (call-with-current-continuation
                         (lambda (oh)
                           (R (car l) oh)))))
                   (if (atom? new-car)
                       (cons (car l)
                             (R (cdr l) oh))
                       (cons new-car (cdr l)))))))))
      (let ((new-l
             (call-with-current-continuation
              (lambda (oh)
                (R l oh)))))
        (if (atom? new-l)
            l
            new-l)))))

;; (rember1* 'noodles '((food) more (food)))
;; (rember1* 'salad '((Swedish rye)
;;                      (French (mustard salad tureky))
;;                      salad))

;; (rember1* 'meat '((pasta meat)
;;                     pasta (noodles meat sauce)
;;                     meat tomatoes))


;;Using consC
(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
                ((null? l) (oh (quote no)))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (consC (car l)
                           (R (cdr l) oh))))
                (else
                 (let ((new-car
                        (call-with-current-continuation
                         (lambda (oh)
                           (R (car l) oh)))))
                   (if (atom? new-car)
                       (consC (car l)
                             (R (cdr l) oh))
                       (consC new-car (cdr l)))))))))
      (let ((new-l
             (call-with-current-continuation
              (lambda (oh)
                (R l oh)))))
        (if (atom? new-l)
            l
            new-l)))))

;; (set-counter 0)

;; (rember1*C 'noodles '((food) more (food)))

;; (counter)

;; (rember1*C 'salad '((Swedish rye)
;;                      (French (mustard salad tureky))
;;                      salad))
;; (counter)


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

;;Version that failed by repeatedly checking whether anything had changed
;;for the car of a list that was a list:
(define rember1*-1
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (cons (car l)
                           (R (cdr l)))))
                (else
                 (let ((av (R (car l))))
                   (if (eqlist? (car l) av)
                       (cons (car l)
                             (R (cdr l)))
                       (cons av (cdr l)))))))))
      (R l))))

;; (rember1*-1 'noodles '((food) more (food)))
;; (rember1*-1 'salad '((Swedish rye)
;;                      (French (mustard salad tureky))
;;                      salad))

;; (rember1*-1 'meat '((pasta meat)
;;                     pasta (noodles meat sauce)
;;                     meat tomatoes))

;;use consC
(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (consC (car l)
                            (R (cdr l)))))
                (else
                 (let ((av (R (car l))))
                   (if (eqlist? (car l) av)
                       (consC (car l)
                              (R (cdr l)))
                       (consC av (cdr l)))))))))
      (R l))))

(set-counter 0)

(consC (consC 'food (quote ()))
       (consC 'more
              (consC (consC 'food (quote ()))
                     (quote ()))))
(counter)

(set-counter 0)

(rember1*C2 'noodles '((food) more (food)))

(counter)

(set-counter 0)

(rember1*C 'noodles '((food) more (food)))

(counter)
