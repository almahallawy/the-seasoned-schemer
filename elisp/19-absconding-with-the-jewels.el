;;Chapter 19. Absconding with the Jewels

(setq lexical-binding t);; important to include this line

;;set to nil to have no limit
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (deep (1- m))
	    (quote ())))))

(deep 6)

(zerop 5)

(deep 0)

(defun six-layers (p)
  (cons
   (cons
    (cons
     (cons
      (cons
       (cons p (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))
    (quote ()))
   (quote ())))

(six-layers 'mozzarella)

(defun four-layers (p)
  (cons
   (cons
    (cons
     (cons p (quote ()))
     (quote ()))
    (quote ()))
   (quote ())))


;;Page 158: This is impossible in Lisp. But Scheme can do it. 
;;This end the chapter in elisp code. Please check the Scheme Code instead
