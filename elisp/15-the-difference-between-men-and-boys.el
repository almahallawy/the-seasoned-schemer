;;Chapter 15. The Difference Between Men and Boys

(setq lexical-binding t);; important to include this line

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

x

(gourmand (quote onion))

x

(defun omnivore ()
    (let ((x (quote minestrone)))
      (lambda (food)
        (setq x food)
        (cons food
              (cons x
                    (quote ()))))))

(funcall (omnivore) (quote bouillabaisse)) ;;note that parenthesis surronding omnivore

x;;not same x in omnivore!!

;;Different Implementation 
(setq omnivore
    (let ((x (quote minestrone)))
      (lambda (food)
        (setq x food)
        (cons food
              (cons x
                    (quote ()))))))

(funcall omnivore (quote bouillabaisse))

x

(setq gobbler 
    (let ((x (quote minestrone)))
      (lambda (food)
	(print x);gobbler remember last food 
	(setq x food)
	(cons food
	      (cons x
		    (quote ()))))))

;;execute the following line twice after runnign (setq gobbler)
(funcall gobbler (quote gumbo))

(funcall gobbler (quote shrimp))

x

(defun nibbler (food)
  (let ((x (quote donut)))
    (print x) ;;nibller doesn't remember last food
    (setq x food)
    (cons food
	  (cons x
		(quote ())))))

(nibbler (quote cheerio))

x

(setq food (quote none))

food

(defun glutton (x)
  (setq food x)
  (cons (quote more)
	(cons x
	      (cons (quote more)
		    (cons x
			  (quote()))))))

(glutton (quote garlic))

food

x

(defun chez-nous ()
  (let ((a food))
    (setq food x)
    (setq x a)))

(chez-nous)

food

x

(glutton (quote garlic))

food

(gourmand (quote potato))

x

(chez-nous)

food

x

`
