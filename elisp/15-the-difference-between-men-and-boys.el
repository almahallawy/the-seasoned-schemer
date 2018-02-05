;;Chapter 15. The Difference Between Men and Boys

(defun x
    (cons (quote chicago)
          (cons (quote pizz)
                (quote ()))))

(setq x
    (cons (quote chicago)
          (cons (quote pizz)
                (quote ()))))

x

(setq x (quote gone))

x

(setq x (quote skins))

x

(defun gourmet (food)
  (cons food
        (cons x (quote ()))))

(gourmet (quote onion))

(cons x (quote ()))

(setq x (quote rings))

(gourmet (quote onion))

(defun gourmand (food)
  (setq x food)
  (cons food
        (cons x
              (quote ()))))

(gourmand (quote potatos))

x

(gourmand (quote rice))

x

(defun diner (food)
  (cons (quote milkshake)
        (cons food
              (quote ()))))

(diner (quote onion))

(defun dinerR (food)
  (setq x food)
  (cons (quote milkshake)
        (cons food
              (quote ()))))

(dinerR (quote onion))

x

(dinerR (quote pecanpie))

(gourmand (quote onion))

x

(defun omnivore ()
    (let ((x (quote minestrone)))
      (lambda (food)
        (setq x food)
        (cons food
              (cons x
                    (quote ()))))))

;;if (setq omnivore)
;(funcall omnivore (quote bouillabaisee))
(funcall omnivore (quote bouillabaisse))
















