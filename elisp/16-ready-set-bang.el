;;Chapter 16. Ready, Set, Bang!

(setq lexical-binding t);; important to include this line

;;set to nil to have no limit
(setq eval-expression-print-level nil)

(defun sweet-tooth (food)
  (cons food
	(cons (quote cake)
	      (quote ()))))

(defun last (quote anglefood))

(sweet-tooth 'chocolate)

(sweet-tooth 'fruit)

(defun sweet-toothL (food)
  (setq last food)
  (cons food
	(cons (quote cake)
	      (quote ()))))

(sweet-toothL (quote chocolate))

last

(sweet-toothL (quote fruit))

last

(sweet-toothL (quote cheese))

last

(sweet-toothL (quote carrot))

last

;;(defun ingredients (quote ())) ;doesn't work 
(setq ingredients (quote ()))

(defun sweet-toothR (food)
  (setq ingredients (cons food ingredients))
  (cons food
	(cons (quote cake)
	      (quote ()))))

(sweet-toothR (quote chocolate))

ingredients

(sweet-toothR (quote fruit))

ingredients

(sweet-toothR (quote cheese))

ingredients

(sweet-toothR (quote carrot))

ingredients

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (deep (1- m))
	    (quote ())))))

(deep 3)

(deep 7)

;;if the output looks like the following. 
;;((((...))))

;;That is because  there is a limit on the max depth of nesting of parenthese and brackets when printed.
;; https://emacs.stackexchange.com/questions/34413/what-is-the-meaning-of-the-ellipsis-at-the-end-of-some-output
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Output-Variables.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Eval.html
;; https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-20-2.5/html_chapter/elisp_19.html
;; https://stackoverflow.com/questions/10728859/how-do-i-insert-a-lenghthier-list-in-my-buffer-with-elisp

;To solve this, set the variable here or in .emacs file
(print eval-expression-print-level)

;;4 is my value

;;set to nil to have no limit
(setq eval-expression-print-level nil)


(deep 0)

;;(defun Ns (quote ())) doesn't work 
(setq Ns (quote ()))

(defun deepR (n)
  (setq Ns (cons n Ns))
  (deep n))

(setq Rs (quote ()))

(defun deepR (n)
  (setq Rs (cons (deep n) Rs))
  (setq Ns (cons n Ns))
  (deep n))

(defun deepR (n)
  (let ((result (deep n)))
    (setq Rs (cons result Rs))
    (setq Ns (cons n Ns))
    result))

(deepR 3)
Rs
Ns

(deepR 5)
Rs
Ns

(deepR 3)
Rs
Ns


(defun find (n Ns Rs)
  (letrec ((A (lambda (ns rs)
		(cond
		 ((= (car ns) n) (car rs))
		 (t (funcall A (cdr ns) (cdr rs)))))))
    (funcall A Ns Rs)))

(find 3 Ns Rs)
(find 5 Ns Rs)

(defun member? (n lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) n)
	  (member? n (cdr lat))))))

(defun deepM (n)
  (if (member? n Ns)
      (find n Ns Rs)
    (deepR n)))

(setq Ns (cdr Ns))
(setq Rs (cdr Rs))

(defun deepM (n)
  (if (member? n Ns)
      (find n Ns Rs)
    (let ((result (deep n)))
      (setq Rs (cons result Rs))
      (setq Ns (cons n Ns))
      result)))

(deepM 3)
(deepM 6)

Rs
Ns

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (deepM (1- m))
	    (quote ())))))

(deepM 9)
Ns
Rs

;;Check omnivore implementatoin in chapter 15
;;to understand why we used setq.
(setq deepM 
  (let ((Rs (quote ()))
	(Ns (quote ())))
    (lambda (n)
      (if (member? n Ns)
	  (find n Ns Rs)
	(let ((result (deep n)))
	  (setq Rs (cons result Rs))
	  (setq Ns (cons n Ns))
	  result)))))

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (funcall deepM (1- m))
	    (quote ())))))

(funcall deepM 16)

(defun deepM ()
  (let ((Rs (quote ()))
	(Ns (quote ())))
    (lambda (n)
      (if (member? n Ns)
	  (find n Ns Rs)
	(let ((result (deep n)))
	  (setq Rs (cons result Rs))
	  (setq Ns (cons n Ns))
	  result)))))

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (funcall (deepM) (1- m))
	    (quote ())))))

(funcall (deepM) 16)

(defun find (n Ns Rs)
  (letrec ((A (lambda (ns rs)
		(cond
		 ((null ns) nil)
		 ((= (car ns) n) (car rs))
		 (t (funcall A (cdr ns) (cdr rs)))))))
    (funcall A Ns Rs)))

(setq A '(1 2 3 4 5 6 7))
(setq B '(1 2 3 4 5 6 7))

(find 5 A B)


(defun atom? (x)
  (not (listp x)))

(defun deepM ()
  (let ((Rs (quote ()))
	(Ns (quote ())))
    (lambda (n)
       (if (null (find n Ns Rs))
	  (let ((result (deep n)))
	    (setq Rs (cons result Rs))
	    (setq Ns (cons n Ns))
	    (print Ns)
	    result)
	(find n Ns Rs)))))

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (funcall (deepM) (1- m))
	    (quote ())))))

(funcall (deepM) 6)

;;Using defun doesn't keep the value of imaginary Rs and Ns between calls

(setq deepM
  (let ((Rs (quote ()))
	(Ns (quote ())))
    (lambda (n)
       (if (null (find n Ns Rs))
	  (let ((result (deep n)))
	    (setq Rs (cons result Rs))
	    (setq Ns (cons n Ns))
	    (print Ns)
	    result)
	(find n Ns Rs)))))

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (funcall deepM (1- m))
	    (quote ())))))

(funcall deepM 6)
(funcall deepM 4)

;;Using setq keep the value of the imaginary Rs Ns between calls

(setq deepM
      (let ((Rs (quote ()))
	    (Ns (quote ())))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (null exists)
		(let ((result (deep n)))
		  (setq Rs (cons result Rs))
		  (setq Ns (cons n Ns))
		  (print Ns)
		  result)
	      exists)))))

(funcall deepM 6)
(funcall deepM 6)
(funcall deepM 4)


		  
