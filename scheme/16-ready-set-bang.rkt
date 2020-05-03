#lang scheme

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
            exists)))))

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
      ;(print l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

(define length-4
  (let ((h (lambda (l) 0)))
    (set! h
          (L (lambda (arg) (h arg))))
    h))

(length-4 '(1 2 3 4))

(quote (=========(set! h (L h))====))

;;why we can't use (L h) directly instead of (L (lambda (arg) (h arg)))
(define length_4_1
  (let ((h (lambda (l) 100)))
    (set! h
          (L h))
    h))


;;because doing this will substitute h in (L h) with the  (lambda (l) 0) instead of the function that looks like length that we need to get from L. So the value of (L h) will be the function

;(lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1
;           ((lambda (l) 100)
;            (cdr l))))))

;;However using (lambda (arg) (h arg)), we can get h that refers to itself as

;(lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1
;           ((lambda (arg) (h arg))
;            (cdr l))))))
;

;;So the following gives a wrong answer
(length_4_1 '(a b c));;101


;;You need to understand how the binding works in Scheme. Here is couple of useful references:
;;https://docs.racket-lang.org/guide/let.html
;;https://www.scheme.com/tspl4/binding.html
;;https://docs.racket-lang.org/guide/set_.html
;;https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lambda-Expressions.html
;;https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lexical-Binding.html
;;https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Assignments.html
;;https://www.scheme.com/tspl4/start.html#./start:h5
;;https://www.scheme.com/tspl4/start.html#./start:h9

;;-------------------------------------------------------------------------------------------------
;;Source: https://www.scheme.com/tspl4/start.html#./start:h5 
;; As with let expressions, lambda expressions become somewhat more interesting when they are nested within other lambda or let expressions.

;; (let ([x 'a])
;;   (let ([f (lambda (y) (list x y))])
;;     (f 'b))) ==> (a b)

;; The occurrence of x within the lambda expression refers to the x outside the lambda that is bound by the outer let expression. The variable x is said to occur free in the lambda expression or to be a free variable of the lambda expression. The variable y does not occur free in the lambda expression since it is bound by the lambda expression. A variable that occurs free in a lambda expression should be bound, e.g., by an enclosing lambda or let expression, unless the variable is (like the names of primitive procedures) bound outside of the expression, as we discuss in the following section.

;; What happens when the procedure is applied somewhere outside the scope of the bindings for variables that occur free within the procedure, as in the following expression?

;; (let ([f (let ([x 'sam])
;;            (lambda (y z) (list x y z)))])
;;   (f 'i 'am)) ==> (sam i am)

;; The answer is that the same bindings that were in effect when the procedure was created are in effect again when the procedure is applied. This is true even if another binding for x is visible where the procedure is applied.

;; (let ([f (let ([x 'sam])
;;            (lambda (y z) (list x y z)))])
;;   (let ([x 'not-sam])
;;     (f 'i 'am))) ==> (sam i am)

;; In both cases, the value of x within the procedure named f is sam.

;;------------------------------------------------------------------------------------
;;Source: https://www.scheme.com/tspl4/binding.html#./binding:h7
;;syntax: (set! var expr) 
;;set! does not establish a new binding for var but rather alters the value of an existing binding. It first evaluates expr, then assigns var to the value of expr. Any subsequent reference to var within the scope of the altered binding evaluates to the new value.


;;------------------------------------------------------------------------
;;Source: https://www.scheme.com/tspl4/binding.html#./binding:h4
;; syntax: (letrec ((var expr) ...) body1 body2 ...) 
;; returns: the values of the final body expression 
;; libraries: (rnrs base), (rnrs)

;; letrec is similar to let and let*, except that all of the expressions expr ... are within the scope of all of the variables var .... letrec allows the definition of mutually recursive procedures.

;; (letrec ([sum (lambda (x)
;;                 (if (zero? x)
;;                     0
;;                     (+ x (sum (- x 1)))))])
;;   (sum 5)) ==> 15

;; The order of evaluation of the expressions expr ... is unspecified, so a program must not evaluate a reference to any of the variables bound by the letrec expression before all of the values have been computed. (Occurrence of a variable within a lambda expression does not count as a reference, unless the resulting procedure is applied before all of the values have been computed.) If this restriction is violated, an exception with condition type &assertion is raised.

;;  ...
;; A letrec expression of the form

;; (letrec ((var expr) ...) body1 body2 ...)

;; may be expressed in terms of let and set! as

;; (let ((var #f) ...)
;;   (let ((temp expr) ...)
;;     (set! var temp) ...
;;     (let () body1 body2 ...)))

;; where temp ... are fresh variables, i.e., ones that do not already appear in the letrec expression, one for each (var expr) pair. The outer let expression establishes the variable bindings. The initial value given each variable is unimportant, so any value suffices in place of #f. The bindings are established first so that expr ... may contain occurrences of the variables, i.e., so that the expressions are computed within the scope of the variables. The middle let evaluates the values and binds them to the temporary variables, and the set! expressions assign each variable to the corresponding value. The inner let is present in case the body contains internal definitions.


;;--------------------------------------------------------------------------
;;Source: https://docs.racket-lang.org/guide/let.html#%28part._.Recursive_.Binding__letrec%29

;; The syntax of letrec is also the same as let:

;; (letrec ([id expr] ...) body ...+)
;; While let makes its bindings available only in the bodys, and let* makes its bindings available to any later binding expr, letrec makes its bindings available to all other exprs—even earlier ones. In other words, letrec bindings are recursive.

;; The exprs in a letrec form are most often lambda forms for recursive and mutually recursive functions

;; While the exprs of a letrec form are typically lambda expressions, they can be any expression. The expressions are evaluated in order, and after each value is obtained, it is immediately associated with its corresponding id. If an id is referenced before its value is ready, an error is raised, just as for internal definitions.

;; > (letrec ([quicksand quicksand])
;;     quicksand)
;; quicksand: undefined;

;;  cannot use before initialization

;;-------------------------------------------------------------------------------------
;;Souurce: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lexical-Binding.html#Lexical-Binding

;; special form: letrec ((variable init) …) expression expression …
;; The variables are bound to fresh locations holding unassigned values, the inits are evaluated in the extended environment (in some unspecified order), each variable is assigned to the result of the corresponding init, the expressions are evaluated sequentially in the extended environment, and the value of the last expression is returned. Each binding of a variable has the entire letrec expression as its region, making it possible to define mutually recursive procedures.

;; MIT/GNU Scheme allows any of the inits to be omitted, in which case the corresponding variables are unassigned.

;; One restriction on letrec is very important: it shall be possible to evaluated each init without assigning or referring to the value of any variable. If this restriction is violated, then it is an error. The restriction is necessary because Scheme passes arguments by value rather than by name. In the most common uses of letrec, all the inits are lambda or delay expressions and the restriction is satisfied automatically.

;;----------------------------------------------------------------------------------------------

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
       (max (add1 (depth*-old (car l)))
            (depth*-old (cdr l)))))))

(quote ====depth====)
(depth*-old '(((1)) 1))

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

(quote ===========Y-Combinator============)

;Y combinator - Applicative-Order Y
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define length_6 (Y L))

(length_6 '(1 2 3 4 5 6))

;;-----------------------------------------------------------------

(println '-----------------biz--------------)

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

(print '****)
(print '((Y biz) 5))
(println '=)
((Y biz) 5)
;;calling ((Y biz) 5) again
;;((Y biz) 5) ; NO answer, will yield infinite recursion call
;;because x = 5 before calling ((Y biz) 5) one more time, so x will keep increasing and will never have a = x

;;((Y! biz) 5) ;;NO Answer

;;why Y! brings no answer???

(define x 0)

(define biz1
  (lambda (f)
    (set! x (add1 x))
    (lambda (a)
      (if (= a x)
          0
          (f a)))))

(print '****)
(print '((Y biz1) 5))
(println '=)
((Y biz1) 5)
;((Y! biz1) 5) ;;NO answer

(set! x 0)

(define length-7
  (let ((h (lambda (l) 0)))
    (set! h
         (biz1 (lambda (arg) (h arg))))
    h))

;;(println '(length-7 5))
;;(length-7 5) ;;Still no answer!!

(define h1 (lambda (l) (quote ())))

(define length-8
  (let ()
    (set! h1
          (biz1 (lambda (arg) (h1 arg))))
    h1))

;;(println '(length-8 5))
;;(length-8 5) ;; No answer

(define h2 (lambda (l) (quote ())))
(set! h2
      (biz1 (lambda (arg) (h2 arg))))

;;(println '(h2 5))
;;(h2 5) ;;no answer

;;; Different Biz where (set! x (add1 x)) is moved inside the internal lambda(a)

(set! x 0)
(define Nx (quote ()))

(define biz2
  (lambda (f)
    (println 'biz2-1)
    (lambda (a)
      (set! x (add1 x)) ;;<---- Note that x is increased inside internal lambda, not like  biz and biz1 where it is increased outsie this internal lambda
      (println 'biz2-2)
      (set! Nx (cons x Nx))
      (if (= a x)
          0
          (f a)))))

(define h3 (lambda (l) (quote ())))

(print '****)
(print '(set! h3(biz2 (lambda (arg) (h3 arg)))))
(println '=)
(set! h3
      (biz2 (lambda (arg) (h3 arg))))

(print '****)
(print '(h3 5))
(println ':)
(h3 5) ;;it works
(println '****Nx=)
Nx

(set! x 0)
(set! Nx (quote ()))

(print '***)
(print '((Y! biz2) 5))
(println '=)
((Y! biz2) 5) ;;Has an answer


(define biz-correct
  (let ((x 0))
    (lambda (f)
      (lambda (a)
        (set! x (add1 x)) ;;<- Now x will be increased with every recursion
        (print 'x=)
        (println x)
        (if (= a x)
            0
            (f a))))))

(print '****)
(print '((Y! biz-correct) 5))
(println '=)
((Y! biz-correct) 5)

;;biz3 is similar to the original biz and biz1 but with more println for clarification
(define biz3 
  (lambda (f)
    (set! x (add1 x))
    (println 'biz3_1)
    (print  'x=)
    (println x)
    (lambda (a)
      (println 'biz3_2)
      (print  'x=)
      (println x)
      (if (= a x)
          (println 'RecursionEnded)
          (f a)))))

(define h4 (lambda (l) (quote ())))

(println '****x=4)
(set! x 4)  ;setting x=4 in order for (h4 5) to work, note that (add1 x) is executed once when evaluating (set! h4..), biz3_1 is printed

x

(print '***)
(print '(set! h4 (biz3 (lambda (arg) (h4 arg)))))
(println '=)
(set! h4
      (biz3 (lambda (arg) (h4 arg))))

(println '****x=)
x

(print '****)
(print '(h4 5))
(println '=)
(h4 5) ;recursion ends

(print '****)
(println '(set! x 6))
(set! x 6)

(print '****)
(print '(h4 6))
(println '=)
(h4 6) ;recursion ends

;;Now notice the output when using Y combinator instead

;(Y biz3)
(print '****)
(print '(Y biz3))
(println '=)
(Y biz3)

(set! x 0)
(println '****x=)
x

(print '****)
(print '((Y biz3) 5))
(println '=)
((Y biz3) 5)

(println '****x=)
x

(set! x 0)
;(h4 6)  ;inifinte loop. biz3_2 ll be printed infinitly. And x is not increased, which means (set! x (add1 x)) is not executed
;;((Y! biz3) 5) ;;Also will go to infinite loop.

;;Comparing ((Y biz3) 5) vs ((Y! biz3) 5) & (h4 5)) shows that (set! x (add1 x)) is executed in Y with every recursion call but it is  not executed in Y! or h4 with every recursion call.

;; When evaluting (set! h4 ..)  or (Y! biz3) , (set! x (add1 x)) evaluted only once, but it is not evaluated when we recur using h4 or Y! because (set! x (add1 x))  doesn't exist anymore. Why?

;;Because the VALUE of  (biz3 (lambda (arg) (h4 arg))) or the VALUE (biz3 (lambda (arg) (h arg))) in (Y! biz3)  is the (lambda (a) ...) only, (set! x (add1 x)) is evaluated once and its value is discarded after evaluating (biz3 (lambda (arg) (h4 arg)));
;;check page 93 for lambda with two expressions

;;that is why x is not increased and we kept recurring  (f a) inifnite times because x is not changed closer to termination which violated Fourth Commadment

;;However in Y , (set! x (add1 x)) exists and is executed with every recursion call and we dont' have the problem we faces with Y!


;To understand more why Y works check the following substituion of biz3 in Y defintion

(println '--------------------------------------------------------)

(print '****)
(print '(Y biz3))
(println '=)
;;(Y biz3) =

((lambda (le)
  ((lambda (f) (f f))
   (lambda (f)
     (le (lambda (x) ((f f) x))))))
 biz3)

;==>

((lambda (f) (f f))
 (lambda (f)
   (biz3 (lambda (x) ((f f) x)))))


;----------
;;Now apply biz3 on number

(print '****)
(print '((Y biz3) 2))
(println '=)
(set! x 0)

((Y biz3) 2)

;Expand Y ==>
(print '****)
(print '(((lambda (f) (f f))) ... 2))
(println '=)
(set! x 0)

(((lambda (f) (f f))
 (lambda (f)
   (biz3 (lambda (x) ((f f) x)))))
 2)

;==>
(set! x 0)
(print '****)
(print '((f f) 2))
(print '=)

;;[Repeat]******--==>>
(((lambda (f)
   (biz3 (lambda (x) ((f f) x))))
 (lambda (f)
   (biz3 (lambda (x) ((f f) x)))))
 2)

;==>
(set! x 0)
(print '****)
(print '(biz3 (lambda x)))
(println '=)

((biz3 (lambda (x)
         (((lambda (f)
             (biz3 (lambda (x) ((f f) x))))
           (lambda (f)
             (biz3 (lambda (x) ((f f) x))))) x)))
 2)

;===>

(set! x 0)
(print '****)
(print '((lambda(f)... ) 2))
(println '=)

(((lambda (f)
    (set! x (add1 x))
    (println 'biz3_1)
    (print  'x=)
    (println x)
    (lambda (a)
      (println 'biz3_2)
      (print  'x=)
      (println x)
      (if (= a x)
          (println 'RecursionEnded)
          (f a))))
  (lambda (x)
    (((lambda (f)
        (biz3 (lambda (x) ((f f) x))))
      (lambda (f)
        (biz3 (lambda (x) ((f f) x))))) x)))
 2)
;==>
(set! x 1) ;;set x=1 because it will be already increased by (set! x (add1 x)) in lambda(f)

(print '****)
(print '((lambda (a) ...(lambda (x)....)) 2))
(println '=)

((lambda (a)
   (println 'biz3_2)
   (print  'x=)
   (println x)
   (if (= a x)
       (println 'RecursionEnded)
       ((lambda (x)
          (((lambda (f)
              (biz3 (lambda (x) ((f f) x))))
            (lambda (f)
              (biz3 (lambda (x) ((f f) x))))) x))
        a)))
 2)

;==>
(println '------------)
(set! x 1)

((lambda (x)
   (((lambda (f)
       (biz3 (lambda (x) ((f f) x))))
     (lambda (f)
       (biz3 (lambda (x) ((f f) x))))) x))
 2)

(println '--------------)
(set! x 1)

(((lambda (f)
       (biz3 (lambda (x) ((f f) x))))
     (lambda (f)
       (biz3 (lambda (x) ((f f) x)))))
 2)
;;which is exactly the same at the substution at link marked with: [Repeat]******--==>>
;;and the substituion will be the same as before till x = a = 2
