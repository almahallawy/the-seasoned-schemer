
#lang racket

(define x
  (cons (quote chicago)
        (cons (quote pizza)
              (quote ()))))


(set! x (quote gone))

(set! x (quote skins))

(define gourmet
  (lambda (food)
    (cons food
          (cons x (quote ())))))

(gourmet 'onion)

(cons x (quote ()))

(set! x (quote rings))

(gourmet (quote onion))

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x
                (quote ())))))

(gourmand (quote potato))

(gourmand (quote rice))

(define diner
  (lambda (food)
    (cons (quote milkshake)
          (cons food (quote ())))))

(diner (quote onion))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
          (cons food (quote ())))))

(dinerR (quote onion))

(dinerR (quote pecanpie))

(gourmand (quote onion))

(define omnivore
  (let ((x (quote minestrone))) ;;x exists after lambda is done
    (lambda (food)
      (set! x food)
      (cons food
             (cons x (quote ()))))))

(omnivore (quote bouillabaisse))

(define gobbler
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x (quote ()))))))

(gobbler (quote gumbo))

(define nibbler
  (lambda (food)
    (let ((x (quote donut))) ;;x is gone after lambda is done
      (set! x food)
      (cons food
            (cons x (quote ()))))))

(nibbler (quote cheerio))


(define food (quote none))

(define glutton
  (lambda (x)
    (set! food x)
    (cons (quote more)
          (cons x
                (cons (quote more)
                      (cons x
                            (quote ())))))))

(glutton (quote onion))
(glutton (quote garlic))


(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(chez-nous)

(glutton (quote garlic))

(gourmand (quote potato))

(chez-nous)