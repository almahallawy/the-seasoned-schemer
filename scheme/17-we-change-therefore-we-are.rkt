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

;;Help D with its work
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

