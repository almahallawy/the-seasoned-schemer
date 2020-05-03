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

;; 16-commandment
(setq sweet-toothR
      (let ((ingredients (quote ())))
	(lambda (food)
	  (setq ingredients
		(cons food ingredients))
	  (print ingredients)
	  (cons food
		(cons (quote cake) (quote ()))))))

(funcall sweet-toothR (quote chocolate))
(funcall sweet-toothR (quote fruit))
(funcall sweet-toothR (quote carrot))
(funcall sweet-toothR (quote cheese))

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

;To solve this, set the variable eval-expression-print-level here or in .emacs file
(print eval-expression-print-level)

;;4 is my value

;;set to nil to have no limit
(setq eval-expression-print-level nil)

(deep 0)

;;(defun Ns (quote ())) ;;doesn't work , we need to use setq
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

;;Here we use defun instead of setq
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

(funcall (deepM) 16);;note that deepM is surrounded by "()"

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

(atom? nil);; nil not atom

(defun deepM ()
  (let ((Rs (quote ()))
	(Ns (quote ())))
    (lambda (n)
       (if (null (find n Ns Rs)) ;;use null instead of atom? because nil is not atom
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

;; Using defun doesn't keep the value of imaginary Rs and Ns between calls
;; This form is umimaginable, Check pg 100 & 7th commandment
;; Use setq instead.
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
(funcall deepM 10)

;;Reset deepM
(makunbound 'deepM)

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

(defun deep (m)
  (cond
   ((zerop m) (quote pizza))
   (t (cons (funcall deepM (1- m))
	    (quote ())))))

(funcall deepM 6)
(funcall deepM 6)
(funcall deepM 4)
(funcall deepM 10)

;;--------------------------------------------
;;Check the following links.
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html#Lexical-Binding
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Lexical-Binding.html#Using-Lexical-Binding
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html#Closures

;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbols.html#Symbols
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Components.html#Symbol-Components
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Names.html#Function-Names

;;The following URL has functions that is really useful in case you need to unbound a variable as shown below for the length function 
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html#Function-Cells
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Void-Variables.html#Void-Variables


;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Forms.html#Symbol-Forms
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html#Closures
;https://www.gnu.org/software/emacs/manual/html_node/elisp/Intro-Eval.html#Intro-Eval

;;------------------------------------------------------------
;;Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbols.html#Symbols
;; A symbol is an object with a unique name. 
;; You can test whether an arbitrary Lisp object is a symbol with symbolp:

;; — Function: symbolp object
;; This function returns t if object is a symbol, nil otherwise.

;;----------------------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Components.html#Symbol-Components

;; Each symbol has four components (or “cells”), each of which references another object:

;; Print name
;;    The symbol's name. 
;; Value
;;    The symbol's current value as a variable. 
;; Function
;;    The symbol's function definition. It can also hold a symbol, a keymap, or a keyboard macro. 
;; Property list
;;    The symbol's property list.

;; ...

;; The value cell holds a symbol's value as a variable, which is what you get if the symbol itself is evaluated as a Lisp expression.
;; Most symbols can have any Lisp object as a value, but certain special symbols have values that cannot be changed; these include nil and t, and any symbol whose name starts with ‘:’ (those are called keywords).

;; The function cell holds a symbol's function definition. Often, we refer to “the function foo” when we really mean the function stored in the function cell of foo; we make the distinction explicit only when necessary. Typically, the function cell is used to hold a function (see Functions) or a macro (see Macros). However, it can also be used to hold a symbol (see Function Indirection), keyboard macro (see Keyboard Macros), keymap (see Keymaps), or autoload object (see Autoloading). To get the contents of a symbol's function cell, use the function symbol-function (see Function Cells).

;; The function cell or the value cell may be void, which means that the cell does not reference any object. (This is not the same thing as holding the symbol void, nor the same as holding the symbol nil.) Examining a function or value cell that is void results in an error, such as ‘Symbol's value as variable is void’.

;; Because each symbol has separate value and function cells, variables names and function names do not conflict. For example, the symbol buffer-file-name has a value (the name of the file being visited in the current buffer) as well as a function definition (a primitive function that returns the name of the file):

     ;; buffer-file-name
     ;;      => "/gnu/elisp/symbols.texi"
     ;; (symbol-function 'buffer-file-name)
     ;;      => #<subr buffer-file-name>
;;--------------------------------------------------------------------------------------------

;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Definitions.html#Definitions

;; A definition is a special kind of Lisp expression that announces your intention to use a symbol in a particular way. It typically specifies a value or meaning for the symbol for one kind of use, plus documentation for its meaning when used in this way. Thus, when you define a symbol as a variable, you can supply an initial value for the variable, plus documentation for the variable.

;; defvar and defconst are special forms that define a symbol as a global variable—a variable that can be accessed at any point in a Lisp program. See Variables, for details about variables. To define a customizable variable, use the defcustom macro, which also calls defvar as a subroutine (see Customization).

;; In principle, you can assign a variable value to any symbol with setq, whether not it has first been defined as a variable. However, you ought to write a variable definition for each global variable that you want to use; otherwise, your Lisp program may not act correctly if it is evaluated with lexical scoping enabled (see Variable Scoping).

;; defun defines a symbol as a function, creating a lambda expression and storing it in the function cell of the symbol. This lambda expression thus becomes the function definition of the symbol. (The term “function definition”, meaning the contents of the function cell, is derived from the idea that defun gives the symbol its definition as a function.) defsubst and defalias are two other ways of defining a function. See Functions.

;; defmacro defines a symbol as a macro. It creates a macro object and stores it in the function cell of the symbol. Note that a given symbol can be a macro or a function, but not both at once, because both macro and function definitions are kept in the function cell, and that cell can hold only one Lisp object at any given time. See Macros.

;; As previously noted, Emacs Lisp allows the same symbol to be defined both as a variable (e.g., with defvar) and as a function or macro (e.g., with defun). Such definitions do not conflict.

;; These definitions also act as guides for programming tools. For example, the C-h f and C-h v commands create help buffers containing links to the relevant variable, function, or macro definitions. See Name Help.
;;--------------------------------------------------------------------------------------------

;;Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Forms.html#Symbol-Forms
;; When a symbol is evaluated, it is treated as a variable. The result is the variable's value, if it has one. If the symbol has no value as a variable, the Lisp interpreter signals an error.
;; (setq a 123)
;;           => 123
;;      (eval 'a)
;;           => 123
;;      a
;;           =>123

;; The symbols nil and t are treated specially, so that the value of nil is always nil, and the value of t is always t; you cannot set or bind them to any other values. Thus, these two symbols act like self-evaluating forms, even though eval treats them like any other symbol. A symbol whose name starts with ‘:’ also self-evaluates in the same way; likewise, its value ordinarily cannot be changed
;;--------------------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Self_002dEvaluating-Forms.html#Self_002dEvaluating-Forms

;; A self-evaluating form is any form that is not a list or symbol. Self-evaluating forms evaluate to themselves: the result of evaluation is the same object that was evaluated. Thus, the number 25 evaluates to 25, and the string "foo" evaluates to the string "foo". Likewise, evaluating a vector does not cause evaluation of the elements of the vector—it returns the same vector with its contents unchanged.
;;---------------------------------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Indirection.html#Function-Indirection

;; If the first element of the list is a symbol then evaluation examines the symbol's function cell, and uses its contents instead of the original symbol. If the contents are another symbol, this process, called symbol function indirection, is repeated until it obtains a non-symbol. See Function Names, for more information about symbol function indirection

;; The following example illustrates the symbol indirection process. We use fset to set the function cell of a symbol and symbol-function to get the function cell contents (see Function Cells). Specifically, we store the symbol car into the function cell of first, and the symbol first into the function cell of erste.

     ;; Build this function cell linkage:
     ;;   -------------       -----        -------        -------
     ;;  | #<subr car> | <-- | car |  <-- | first |  <-- | erste |
     ;;   -------------       -----        -------        -------
     ;; (symbol-function 'car)
     ;;      => #<subr car>
     ;; (fset 'first 'car)
     ;;      => car
     ;; (fset 'erste 'first)
     ;;      => first
     ;; (erste '(1 2 3))   ; Call the function referenced by erste.
     ;;      => 1


;; By contrast, the following example calls a function without any symbol function indirection, because the first element is an anonymous Lisp function, not a symbol.

;;      ((lambda (arg) (erste arg))
;;       '(1 2 3))
;;           => 1


;;--------------------------------------------------------------------------
;;Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Forms.html#Function-Forms

;; If the first element of a list being evaluated is a Lisp function object, byte-code object or primitive function object, then that list is a function call. For example, here is a call to the function +:

;;      (+ 1 x)
;; The first step in evaluating a function call is to evaluate the remaining elements of the list from left to right. The results are the actual argument values, one value for each list element. The next step is to call the function with this list of arguments,

;;-----------------------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Quoting.html#Quoting

;; The special form quote returns its single argument, as written, without evaluating it. This provides a way to include constant symbols and lists, which are not self-evaluating objects, in a program. (It is not necessary to quote self-evaluating objects such as numbers, strings, and vectors.)

;; — Special Form: quote object
;; This special form returns object, without evaluating it.

;; Because quote is used so often in programs, Lisp provides a convenient read syntax for it. An apostrophe character (‘'’) followed by a Lisp object (in read syntax) expands to a list whose first element is quote, and whose second element is the object. Thus, the read syntax 'x is an abbreviation for (quote x).

;; Here are some examples of expressions that use quote:

;;      (quote (+ 1 2))
;;           => (+ 1 2)
;;      (quote foo)
;;           => foo
;;      'foo
;;           => foo
;;      ''foo
;;           => (quote foo)
;;      '(quote foo)
;;           => (quote foo)
;;      ['foo]
;;           => [(quote foo)]

;;-----------------------------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Variables.html#Variables

;; A variable is a name used in a program to stand for a value. In Lisp, each variable is represented by a Lisp symbol (see Symbols). The variable name is simply the symbol's name, and the variable's value is stored in the symbol's value cell1. See Symbol Components. In Emacs Lisp, the use of a symbol as a variable is independent of its use as a function name.

;;------------------------------------------------------------------------------------------
;; Source:https://www.gnu.org/software/emacs/manual/html_node/elisp/Void-Variables.html#Void-Variables


;; We say that a variable is void if its symbol has an unassigned value cell (see Symbol Components).

;; Under Emacs Lisp's default dynamic scoping rule (see Variable Scoping), the value cell stores the variable's current (local or global) value. Note that an unassigned value cell is not the same as having nil in the value cell. The symbol nil is a Lisp object and can be the value of a variable, just as any other object can be; but it is still a value. If a variable is void, trying to evaluate the variable signals a void-variable error, instead of returning a value.

;; Under the optional lexical scoping rule, the value cell only holds the variable's global value—the value outside of any lexical binding construct. When a variable is lexically bound, the local value is determined by the lexical environment; hence, variables can have local values even if their symbols' value cells are unassigned.

;;----------------------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html#Local-Variables

;; The special forms let and let* exist to create local bindings:

;; — Special Form: let (bindings...) forms...
;; This special form sets up local bindings for a certain set of variables, as specified by bindings, and then evaluates all of the forms in textual order. Its return value is the value of the last form in forms. The local bindings set up by let will be in effect only within the body of forms.

;; Each of the bindings is either (i) a symbol, in which case that symbol is locally bound to nil; or (ii) a list of the form (symbol value-form), in which case symbol is locally bound to the result of evaluating value-form. If value-form is omitted, nil is used.

;; All of the value-forms in bindings are evaluated in the order they appear and before binding any of the symbols to them. Here is an example of this: z is bound to the old value of y, which is 2, not the new value of y, which is 1.

;;           (setq y 2)
;;                => 2
          
;;           (let ((y 1)
;;                 (z y))
;;             (list y z))
;;                => (1 2)
;; On the other hand, the order of bindings is unspecified: in the following example, either 1 or 2 might be printed.

;;           (let ((x 1)
;;                 (x 2))
;;             (print x))
;; Therefore, avoid binding a variable more than once in a single let form.
;;------------------------------------------------------------------

;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#Setting-Variables

;; The usual way to change the value of a variable is with the special form setq. When you need to compute the choice of variable at run time, use the function set.

;; — Special Form: setq [symbol form]...
;; This special form is the most common method of changing a variable's value. Each symbol is given a new value, which is the result of evaluating the corresponding form. The current binding of the symbol is changed.

;; setq does not evaluate symbol; it sets the symbol that you write. We say that this argument is automatically quoted. The ‘q’ in setq stands for “quoted”.

;; The value of the setq form is the value of the last form.

;;           (setq x (1+ 2))
;;                => 3
;;           x                   ; x now has a global value.
;;                => 3
;;           (let ((x 5))
;;             (setq x 6)        ; The local binding of x is set.
;;             x)
;;                => 6
;;           x                   ; The global value is unchanged.
;;                => 3
;; Note that the first form is evaluated, then the first symbol is set, then the second form is evaluated, then the second symbol is set, and so on:

;;           (setq x 10          ; Notice that x is set before
;;                 y (1+ x))     ;   the value of y is computed.

;;                => 11

;; — Function: set symbol value
;; This function puts value in the value cell of symbol. Since it is a function rather than a special form, the expression written for symbol is evaluated to obtain the symbol to set. The return value is value.

;; When dynamic variable binding is in effect (the default), set has the same effect as setq, apart from the fact that set evaluates its symbol argument whereas setq does not. But when a variable is lexically bound, set affects its dynamic value, whereas setq affects its current (lexical) value. See Variable Scoping.

;;           (set one 1)
;;           error--> Symbol's value as variable is void: one
;;           (set 'one 1)
;;                => 1
;;           (set 'two 'one)
;;                ⇒ one
;;           (set two 2)         ; two evaluates to symbol one.
;;                => 2
;;           one                 ; So it is one that was set.
;;                => 2
;;           (let ((one 1))      ; This binding of one is set,
;;             (set 'one 3)      ;   not the global value.
;;             one)
;;                => 3
;;           one
;;                => 2
;; If symbol is not actually a symbol, a wrong-type-argument error is signaled.

;;           (set '(x y) 'z)
;;           error--> Wrong type argument: symbolp, (x y)
;;-----------------------------------------------------------------------
;;Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html#Lexical-Binding

;;-------------------------------------------------------------------------------
;;Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Names.html#Function-Names

;; A symbol can serve as the name of a function. This happens when the symbol's function cell (see Symbol Components) contains a function object (e.g., a lambda expression). Then the symbol itself becomes a valid, callable function, equivalent to the function object in its function cell.

;; The contents of the function cell are also called the symbol's function definition. The procedure of using a symbol's function definition in place of the symbol is called symbol function indirection; see Function Indirection. If you have not given a symbol a function definition, its function cell is said to be void, and it cannot be used as a function.

;; In practice, nearly all functions have names, and are referred to by their names. You can create a named Lisp function by defining a lambda expression and putting it in a function cell (see Function Cells). However, it is more common to use the defun special form, described in the next section. See Defining Functions.

;; We give functions names because it is convenient to refer to them by their names in Lisp expressions. Also, a named Lisp function can easily refer to itself—it can be recursive. Furthermore, primitives can only be referred to textually by their names, since primitive function objects (see Primitive Function Type) have no read syntax.

;; A function need not have a unique name. A given function object usually appears in the function cell of only one symbol, but this is just a convention. It is easy to store it in several symbols using fset; then each of the symbols is a valid name for the same function.

;; Note that a symbol used as a function name may also be used as a variable; these two uses of a symbol are independent and do not conflict. (This is not the case in some dialects of Lisp, like Scheme.)

;;-----------------------------------------------------------------------
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Calling-Functions.html#Calling-Functions

;; When you write a list as an expression in your program, you specify which function to call, and how many arguments to give it, in the text of the program. Usually that's just what you want. Occasionally you need to compute at run time which function to call. To do that, use the function funcall. When you also need to determine at run time how many arguments to pass, use apply.

;; — Function: funcall function &rest arguments
;; funcall calls function with arguments, and returns whatever function returns.

;; Since funcall is a function, all of its arguments, including function, are evaluated before funcall is called. This means that you can use any expression to obtain the function to be called. It also means that funcall does not see the expressions you write for the arguments, only their values. These values are not evaluated a second time in the act of calling function; the operation of funcall is like the normal procedure for calling a function, once its arguments have already been evaluated.

;; The argument function must be either a Lisp function or a primitive function. Special forms and macros are not allowed, because they make sense only when given the unevaluated argument expressions. funcall cannot provide these because, as we saw above, it never knows them in the first place.

;;           (setq f 'list)
;;                => list
;;           (funcall f 'x 'y 'z)
;;                => (x y z)
;;           (funcall f 'x 'y '(z))
;;                => (x y (z))
;;           (funcall 'and t nil)
;;           error--> Invalid function: #<subr and>


;;-----------------------------------------------------------------------------
;Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html#Function-Cells
;; https://stackoverflow.com/questions/9942675/in-elisp-how-do-i-put-a-function-in-a-variable
;; https://stackoverflow.com/questions/8670207/when-to-quote-symbol-in-emacs-lisp
;; https://emacs.stackexchange.com/questions/715/how-to-know-when-or-when-not-to-use-the-single-quote-before-variable-names
;;-----------------------------------------------------------------
;Source: https://emacs.stackexchange.com/questions/9605/setq-with-lambda-argument-sets-symbols-variable-cell-or-function-cell

(setq callback (lambda () (message "I am a lambda in a variable cell")))
;; => (lambda nil (message "I am a lambda in a variable cell"))
(defun callback () (message "I am a function in a function cell"))
;; => callback
(funcall callback) ;; Call the function in the variable (expand the symbol)
;; => "I am a lambda in a variable cell"
(funcall 'callback) ;; Call the function defined by the symbol
;; => "I am a function in a function cell"

;Here you can see the value of your symbols as variables and functions:

(setq callback (lambda () (message "hai i am a lambda")))
(setq const-val "hai, i am a const")

(symbol-value 'callback) ;; => (lambda nil (message "hai i am a lambda"))
(symbol-function 'callback) ;; => nil

(symbol-value 'const-val) ;; => "hai, i am a const"
(symbol-function 'const-val) ;; => nil

;;---------------------------------------------------------------------------------------------------------

(defun length (l)
  (cond
   ((null l) 0)
   (t (1+ (length (cdr l))))))

(length '(1 2 3 4))

(funcall 'length '(1 2 3 4))

(symbol-function 'length)

(fboundp 'length)

(fmakunbound 'length)

(fboundp 'length)

(length '(1 2 3)) ;;ERROR. Void function length

(defun length nil)

(symbol-function 'length)

(fboundp 'length)

(length)

(length '(1 2 3)) ;ERROR

(setq length
      (lambda (l)
	0))

(setq length
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (funcall length (cdr l)))))))

(funcall length '(1 2 3))

(makunbound 'length)

(setq length
      (let ((h (lambda (l) 0)))
	(setq h
	      (lambda (l)
		(cond
		 ((null l) 0)
		 (t (1+ (funcall h (cdr l)))))))
	h))

(funcall length '(1 2 3 4))


(defun L (length)
  (lambda (l)
    (cond
     ((null l) 0)
     (t (1+ (funcall length (cdr l)))))))

(makunbound 'length)

(setq length
      (let ((h (lambda (l) 0)))
	(setq h
	      (L (lambda (arg) (funcall h arg))))
	h))

(funcall length '(1 2 3 4 5 6 7))

(makunbound 'length)

;;-----------------------------------------------
;;what if we try (L h) instead

(setq length
      (let ((h (lambda (l) 0)))
	(setq h
	      (L h))
	h))

(funcall length '(1 2 3 4 5))
;; => 1 ;;because of lexical binding h will be evaluated to 0 instead of the functoin returned by L
;;Check Chapter 16 scheme code for more details
;;------------------------------------------------

(defun Y! (L)
  (let ((h (lambda (l) (quote ()))))
    (setq h
	  (funcall L (lambda (arg) (funcall h arg))))
    h))

(makunbound 'length)

(setq length (Y! L));; Error: Void variable L, why, because L is not variable, it is function
;; and we need to pass Y! a function

(boundp 'L)
(fboundp 'L)
(symbol-value 'L)
(symbol-function 'L)


;;You need to Pass L as function as shown below
(setq length (Y! (function L)))

(funcall length '(1 2 3 4))


(defun Y-bang (f)
  (letrec ((h (f (lambda (arg) (h arg)))))
    h))

(defun max (n m)
  (if (> n m) n m))

(defun atom? (x)
  (not (listp x)))

(defun depth* (l)
  (cond
   ((null l) 1)
   ((atom? (car l))
    (depth* (cdr l)))
   (t (max
       (1+ (depth* (car l)))
       (depth* (cdr l))))))

(depth* '((pickled) peppers (peppers pickled)))
(depth* '(c (b (a b) a) a))


(defun D (depth*)
  (lambda (s)
    (cond
     ((null s) 1)
     ((atom? (car s))
      (funcall depth* (cdr s)))
     (t (max
	 (1+ (funcall depth* (car s)))
	 (funcall depth* (cdr s)))))))

(setq depth* (Y! (function D)))

(funcall depth* '((pickled) peppers (peppers pickled)))
(funcall depth* '(c (b (a b) a) a))

;;-------------------------------------------------

;;Applicative-Order Y
(defun Y (le)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall le (lambda (x)
		   (funcall (funcall f f) x))))))

(makunbound 'length)

(setq length (Y (function L)))

(funcall length '(1 2 3 4 5 6 7 8))

(setq biz
  (let ((x 0))
    (lambda (f)
      (setq x (1+ x))
      (lambda (a)
	(prin1 x)
	(prin1 " ")
	(if (= a x)
	    0
	  (funcall f a))))))


(funcall (Y biz) 5) ;;=> 0

;;Call it again
;;(funcall (Y biz) 5) ;;No answer, Infinite recursion. Lisp nesting exceeds ‘max-lisp-eval-depth’"
;; Because by end of the first call, x =5,so calling it again, x will keep increasing and will never be x = 5

(funcall (Y! biz) 5) ;;=> Infinite recursion.Lisp nesting exceeds ‘max-lisp-eval-depth’"

;;Check chapter 16 Scheme code for more details for (Y! biz) doesn't have an answer

;;In nutshell, when (Y! biz) is evaluated before calling the resulting function , (setq x (1+ x)) is executed only once and will not be called again with every recursion, the test for end of the recursion never reached because x = 1 always. never increased

;;However in (Y biz), (setq x (1+ x)) is executed in every recursion call. Check scheme code for the unfolding of this recursion

;;One possible fix is as shown below

(makunbound 'biz)

(setq biz
      (let ((x 0))
	(lambda (f)
	  (lambda (a)
	    (setq x (1+ x)) ;;Moved her to be called with every recursion
	    (prin1 x)
	    (prin1 " ")
	    (if (= a x)
		0
	      (funcall f a))))))

(funcall (Y! biz) 5)
