;;Ansi common lisp  
;;'y 'x '(and (integerp x) (zerop (mod x 2))))
					;(trace function-name)
					;all lisp expressions are atoms or 
(defun l-f ()
  (load "./common.lisp")) ;'common.lisp))
(defun l-cf ()
  (load (compile-file 'common.lisp)))

					;EVALUATION RULE
(+ 3 2)
					;+ is a function call
					;args evaluated left to right and values are passed to the function! 

					;quote doesn't follow the evaluation rule
					;QUOTE RULE -- do nothing
(quote (+ 3 5))
'(+ 3 5) ; => (+ 3 5) ie it gives the same thing back
					;protects expressions from evaluation

					;data
					;string "blah"
					;integer 1
					;symbol 'blah  -- actually this is pointer to a hash table
					;list '(1 a 3) -- this is a fundemental data type


					;lists are zero or more elements (made up of atoms even?) in parantheses.
'(my 3 "Sons")
'(the list (a b c) has 3 elements) ;one quote protects the entire list
					;but using the list function turns things into lists
(list 'my (+ 2 1) "Sons" 'meow)
					;lisp programs are expresse as lists! means that because they're lists themselves, we can generate code that genereates code...check out the lisp parse tree
(list '(+ 2 1) (+ 2 1))

(cons 'a '(b c d))
					;(print (cons '(a b) '(b c d)))
					;this makes the same thing as cons
					;(print (list 'a 'b))

					;(print (list '(a b) '(c d))); => this gives the  '( (a b) (c d) )
					;car is the first element in the list
					;cdr returns the rest of the list
(third '(a b c d))

					;listp checks to see if it's a list
(listp 29) ; nil
(listp '(1 2 3)) ; T

(null nil) ; T
(not nil) ; T

					;if first statement then second statemnt else last statment
(if (listp '(a b c))
    (print (+ 1 2))
    (print (+ 5 6)))

(if (listp ())
    (print (+ 1 2))
    (print (+ 5 10)))
					;if all arguments are true then it will evaluate the next one otherwise is stops
(and t (+ 1 2)) ; => 3
(and () (+ 1 2)) ; => () ;stops evaluating the rest of the args
					;or does the opposite it returns the true argument
(or (+ 1 2) t); 3
(or t (+ 1 2)) ; t 
					;takes a name, a list of parameters, one or more expressions that make up the body of the function 
(defun our-third (x)
  (car (cdr (cdr x))))
					;symbols are variable names, that exist as objects;
					;lists have to be quoted or else they're treaded as CODE
					;symbols have to be quoted or else they'll be treated as variables!!

					;no distinction between program, procedure and a function
(defun sum-greater (x y z)
  (> (+ x y) z))

					;tests if an element in a function is a list
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun our-member (obj lat)
  (cond
    ((null lat) nil)
    ((eql obj (car lat))
     lat)
    (t (our-member obj
		   (cdr lat)))))
					; ~A is a place holder in order, and ~% is the new line character
(format t "~A plus ~A equals ~A. ~%" 2 3 (+ 2 3))
					;read is a cool function and it's a parser all in one
(defun askem (string)
  (format t "~A" string)
  (read))
					;(defun askem "How old are you? ")
					;notice how this returned the object 19 if we entered it with 19
					;NO SIDE EFFECTS!!!!!

					;let makes local variables!!
;;let makes local variables!!!!!! 
(let ((x 1) (y 2))
  (+ x y))

(defun ask-number()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))
(defun ask-number2()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (cond
      ((numberp val) val)
      (t
       (ask-number2)))))

					;make a global variable by givina a symbol and a value to a defparameter
(defparameter *glob* 99)
					;another way to make a global constant
					;(defconstant limit (+ *glob* 1)) ;assigns the value of (+ *glob* 1) to limit
(setf *glob* 98)
(format t "using setf, the value is: ~A" (let ((n 10))
					   (setf n 2)
					   n))

					;if x is not a local variable it'll be become a global variable
(setf x (list 'a 'b 'c))
(setf x (list 'a 'b 'c '(a b c)))

					;see how it replaces stuff
					;first arg can also be a var name 
(setf (car x) 'n) ; x = '(n a b c (a b c))
(setf (cadddr x) 'z) ; x = '(n b c z)

					;the following also works as three separate calls to setf 
(setf (car x) 'changed
      (cadr x) 'lols)
					;(setf a b
					;      c d
					;      e f)

(setf lst '(c a r a t))
					;functional
(remove 'a lst) ; returns '(c a t) but doesn't modify the lst varialbe
					;side effect!
(setf x (remove 'a lst))

					;use iteration to generate some sort of table
(defun show-squares (start end)
  (do ((i start (1+ i))) ;first arg to do - (variable initial update)
      ((> i end) 'done)  ;second arg to do - (stopping-cond return-value)
    (format t "~A ~A~%" i (* i i)))) ;third arg to the do macro -- the body and expressed in cron order
					;first arg
					;initial value ofa  varialbe and how it will increment
					;do is uesed for iteration ((symbol initial update)
					;                          (x 0 (1+ x))
					;creates variables (var initial update)
					;second arg:
					;list with one ore more expressions
					;which tell when it should stop (var > end) in our case, and the rest of the expressions will be evaluated once the first one stops

					;recursive version of show-squares
(defun show-squares (i end)
  (if (> i end)
      'done
      (progn ;progn takes a list of statments and executes them in order
	(format t "~A ~A~%" i (* i i))
	(show-squares (1+ i) end))))
;;examples of progn
					;      (progn
					;        (print "lol")
					;        (print "line 2")
					;        (print "third line")))))

(defun show-squares (i end)
  (cond
    ((> i end) 'done)
    (t
     (format t "~A ~A~%" i (* i i))
     (show-squares (1+ i) end))))

					;return lenght of a list using dolist
(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst) ;for each obj in lst do some shit
      (setf len (1+ len)))
    len))

(defun our-length (lst)
  (cond 
    ((null lst) 0)
    (t
     (+ 1 (our-length (cdr lst))))))

					;funtions are regular objects
					;if we give the name of a function to FUNCTION, it will return the associated ojbect
(function +)
					;sharp quote is the same thing
#'+ ;this is like the ' and quote() functions it's an abbreviation for (function) ; it also returns the same + object
(funcall #'+ 2 3) ; => 5
(funcall (function +) 2 3) ; => 5

					;we can pass a function as an argument like we can pass anything else
(apply #'+ '(1 2 3)) ;takes any arg as long as the last one is a list
(funcall #'+ 1 2 3) ;doesn't take a list takes separate args
					;(apply #'1+ '(1 2 3))
(apply #'eql '(1 2))

					;apply takes a function and list of arguments and then it returns the result
					;of applying the function to it's arguments
					;so bascially like funcall which takes just arguments not in a list. apply can can take the arguments that are in the list to the function

(funcall #'+ 1 2 3); see how the last arg doesn't need to be list
(apply #'+ 1 2 '(1 2 3)) ;see how the last arg must be a list
(funcall #'list 'a 'v)
(apply #'list '(a v))
(apply #'list 'a '(a v))


					;lambda is a symbol!!!!
					;functions use to be represented internally as lists in earlier dialects of lisp. and the only way to tell a function
					;from a list int that case was to check if the first element in the list was the symbol lambda. ie 'lambda
					;in common lisp functions can be expressed as lists but they are represtened internally as distinct functjion objects.
					;so lambda is technically no longer really necessary.
					;((x) (+ x 100))
					;is the same as
					;(lambda (x)
					; (+ x 100))
					;both should be returning a function
(lambda (x y)
  (+ x y))
					;try
((lambda (x y) (+ x y)) 2 3); => 5 will add 2 and 3 together
((lambda (x) (+ x 100)) 1) ;=> 101 will return 1
					;or the same thing using sharp quote
(funcall #'(lambda (x) (+ x 100)) ;use functions without naming them
	 3)
					;values have types, not variables
					;manifest typing - every object has a label identifiying its type
(typep 27 'integer) ;=> returns true 
(typep nil t); => t; t is the supertype to everything including nil!!!
					;chap 1 exercises
					;(+ (- 5 1) (+ 7 3)) ;=> 14
(list 1 (+ 2 3)) ;  '(1 5)
(if (listp 1) (+ 1 2) (+ 3 4)) ; 7
(list (and (listp 3) t) (+ 1 2)) ; (T 3) WRONG IT'S (nil 3)
(list (or (listp 3) t) (+ 1 2)) ;(T 3) right
					;three distinct cons expressons that return (a b c)
(cons 'a (cons 'b (cons 'c nil))) ; right
(cons 'a (cons 'b (list 'c)))
(cons 'a '(b c)) ;right 
(cons 'a (cons 'b '(c))) ;right
					;function that takes two parameters and reutns the greater of the tow
(defun ex-1-4 (x y)
  (if (> x y) x y))
					;now with style
(funcall #'(lambda (x y) (if (> x y) x y))
	 1
	 2) ;=> should return 2
					;what does this do
(defun enigma (x) ;say '()
  (and (not (null x)) ;then it returns nil. on empty list it will return nil
       (or (null (car x)) ;returns true if there are any nils in the list
	   (enigma (cdr x)))))
					;enigma tells us if there are any nils


					;x is the stopping condition
					;shows the number of elements in the list before the stopping element x shows up)
(defun mystery (x y)
  (if (null y) 
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1)))))) ;ZERO DOES NOT EQUAL NIL!!!!!!!!!!! REMEWMBER SO (and 0 1) ===> 1 NOT ZERO as i expected
					;also (or 0 1) returns 0 which is just strange
					;actually it makes perfect sense!
					;(and 0 1 2 3 4 29 3) will evaluate all the arguments that are true!!! so the last arguemnt it evaluates is 3 which is not nil so it works!
					;(or 0 1 2 3 3919 3 3 43) will return 0 because it's the first non nil argument in list it has to process

					;1-6
(car (car (cdr '(a (b c) d)))) ;=>b 
(or 13 (/ 1 0)) ;this won't mess up because the first expression evalutes to not nil so the second one never is called
(funcall #'list 1 nil)
					;1-7
(defun one-arg-a-list (lst)
  (cond
    ((null lst) 'nil)
    ((listp (car lst)) t)
    (t
     (one-arg-a-list (cdr lst)))))
(defun one-arg-a-list (l)
  (if (null l)
      nil
      (if (listp (car l))
	  t
	  (one-arg-a-list (cdr l)))))
(defun num-x-in-lst (a l)
  (cond 
    ((null l) 0)
    ((eq (car l) a)
     (1+ (num-x-in-lst a (cdr l))))
    (t
     (num-x-in-lst a (cdr l)))))

;;this version works because lisp is all about functional programming so that in the first one lst isn't
;;actaully effected by the first remove call so when you apply it to nothing it does nothing
(defun summit (lst)
  (apply #'+ (remove nil lst)))

					;my version of summit
(defun my-summit2 (lst)
  (cond
    ((null lst) 0)
    ((null (car lst))
     (my-summit2 (cdr lst)))
    (t
     (+ (car lst)
	(my-summit2 (cdr lst))))))

					;pg version
(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
	(if (null x)
	    (summit (cdr lst))
	    (+ x (summit (cdr lst)))))))

					;chapter 3
					;(setf x (cons 'a nil))
					;(setf y (list 'a 'b 'c))
					;(setf z (list 'a (list 'b 'c) 'd))
(defun our-listp (x)
  (or (null x) (consp x))) ;consp returns true if cons is a list!
					;everything in a list that's not an atom is a list
(defun our-atom (x)
  (not (consp x))) ;try this with nil it's both an atom and a list!!!

(eql (cons 'a nil) (cons 'a nil)); => nil becuse they're distinct objects in memory ;but equalp would work

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))
					;NOTE
					;variables have values in the same way as lists have elements
					;(setf x '(a b c))
					;(setf y x)
					;(eql x y) ;=> T becuse they point to the same thing
					;(setf x (remove 'a x))
					;(eql x y) ;=> false no longer point to the same thing
(setf x '(a b c)
      y (copy-list x))
(eql x y) ;-> nil (looks like a picture of a double bus pointing to the same values)
					;new bus with the same passengers

(defun our-copy-list (lst)
  (if (atom lst)
      lst ;will happen in the last element or will happen if there is only one element
      (cons (car lst)
	    (our-copy-list (cdr lst)))))

					;append is the conatenation of any number of lists
(append '(a b) '(c d) '(e))

					;Notice
(consp nil) ;NIL
(listp nil) ;T
					;do some run-length-encoding
(defun compress (x)
  (if (consp x) ;is x a member of a list
      (compr (car x) 1 (cdr x))
      x))
(defun compr (elem n lst)
  (if (null lst)
      (list (n-elts elem n))
      (let ((next (car lst))) ;else 
	(if (eql next elem)
	    (compr elem (+ n 1) (cdr lst))
	    (cons (n-elts elem n)
		  (compr next 1 (cdr lst)))))))
					;returns the number of times a an element appeared as a list
					;ed (3 SOUP) ;3 == n AND elem = SOUP
(defun n-elts (elem n)
  (if (> n 1)
      (list n elem)
      elem))
					;use
(print (compress '(soup soup milk tea tea tea salad chicken salad salad)))

					;practice
					;(defun compr (elem n lst)
					;  (cond
					;    ((null lst)
					;     (list (n-elts elem n))) ;the function will always return a list
					;    ((eql elem (car lst)) ;elem is current element we're looking at, (car lst) is the next eleemnt we're comparing it to.
					;     (compr elem (1+ n) (cdr lst)))
					;    (t
					;      (cons (n-elts elem n)
					;        (compr (car lst) 1 (cdr lst))))))

					;should be the same as above
					;(print (compress '(soup soup milk tea tea tea salad chicken salad salad)))

					;practice using let
					;(defun compr (elem n lst)
					;  (cond
					;    ((null lst)
					;      (list (n-elts elem n)))
					;    (t
					;      (let ((next (car lst)))
					;        (cond
					;          ((eql next elem)
					;           (compr elem (1+ n) (cdr lst)))
					;          (t
					;            (cons (n-elts elem n)
					;              (compr next 1 (cdr lst)))))))))

					;(print (compress '(soup soup milk tea tea tea salad chicken salad salad)))

					;eg (list-of 3 'soup)
					;(SOUP SOUP SOUP)
(defun list-of (n elem)
  (if (zerop n)
      nil
      (cons elem (list-of (1- n) elem))))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elem (car lst))
	    (the-rest (uncompress (cdr lst))))
	(if (consp elem)
	    (append (apply #'list-of elem) ;see we use apply becuase we know that (4 SOUP) is what elem is. apply takes a list of args and then passes them in one at a time to the function that needs them! 
		    the-rest)
	    (cons elem the-rest)))))

(uncompress '((3 soup) chicken (2 pie)))
					;get element at nth position
(nth 0 '(a b c)) ; => 'A
					;get nth cdr call this
(nthcdr 2 '(a b c)); => '(C)

(defun our-nthchdr (n lst)
  (if (zerop n)
      lst
      (our-nthchdr (1- n) (cdr lst))))

(last '(a b c)) ;=> (C) not 'C 

					;takes a functiona nd applies it every member of the list
					;returns the list of the results of doing such thing
(print (mapcar #'(lambda (x) (+ x 10))
	       '(1 2 3))) ;-> (11 12 13)

					;applies list to both lists until one is empty
(print (mapcar #'list
	       '(a b c d e f g)
	       '(1 2 3))) ;=> ((a 1) (b 2) (c 3))
(mapcar #'(lambda (x) x)
	'(a b c)) ;=> '(A B C) returns itself 
(mapcar (function (lambda (x) x)) '(a b c)) ;function and #' do the same thing 
					;trees
					;lets take (a (b c) d)
					;function copy-tree takes a tree and returns a copy of it like so
(defun our-copy-tree (tr)
  (if (atom tr)
      tr ;if it's an atom we want it's value
      (cons (our-copy-tree (car tr)) ;so it's not an atom so we want it's left side value
	    (our-copy-tree (cdr tr))))) ;as well as we want the right side value
					;(and (integerp x) (zerop (mod x 2)))
(substitute 'y 'x '(and (integerp x) (zerop (mod x 2)))) ; will return list as is! no change! because it sees 3 elements that are not x => 'and '(integerp x) '(zerop (mod x 2))
					;but subst goes along the car and cdr of the tree
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree) ;NIL IS AN ATOM! 
	  tree
	  (cons (our-subst new old (car tree))
		(our-subst new old (cdr tree))))))

(defun our-subst2 (new old tree)
  (cond
    ((null tree) nil) ;nil is an atom
    ((atom tree)
     (cond
       ((eql tree old)
	new)
       (t
	tree)))
    (t
     (cons (our-subst2 new old (car tree))
	   (our-subst2 new old (cdr tree))))))
					;use
(our-subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
(our-subst 'y 'x '(and (integerp x) (zerop (mod x a b x (x a d d x (((x)))) 3 2))))
(our-subst2 'y 'x '(and (integerp x) (zerop (mod x a b x (x a d d x (((x)))) 3 2))))

					;see how all base cases are covered
					;works for n = 0 and for n+1
(defun our-len(lst)
  (if (null lst)
      0
      (1+ (our-len (cdr lst)))))

					;compares by using eql
(member 'b '(a b c)) ; => (B C) not just t it returns a list otherise it returns nil
					;:test that function will be used to test 
(member '(a) '((b) (a b) (a) (z)) :test #'equal) ;this will return true since (equal '(a) '(a)) => true
(member '(a) '((b) (a b) (a) (z)) :test #'eql) ;the default test operator will return nil since (eql '(a) '(a)) => NIL
					;:key specifies the function to be applied to each element before comparison
(member 'a '((x y) (b a) (1 2)) :key #'car)  ; nil! since
(member 'a '((x y) (b a) (1 2)) :key #'cadr) ;((B A) (1 2))
(member 'b '((x y) (b a) (1 2)) :key #'car)
					;order doesn't matter
(member 2 '((1) (2)) :key #'car :test #'equal)
(member 2 '((1) (2)) :test #'equal :key #'car)

(member-if #'oddp '(2 3 4 5)) ;=> (3 4 5)
(member-if #'evenp '( 1 2 3 4 5)) ;-> (2 3 4 5)

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
	   lst
	   (our-member-if fn (cdr lst)))))

					;use
(our-member-if 'oddp '( 2 3 4 5))

					;it ajoins only if it's not already in the list
					;and takes ame keyword args as member
(adjoin 'b '(a b c)) ;=> (a b c)
(adjoin 'z '(a b c)) ; => (z a b c)

(union '(a b c) '(c b s)) ;(a b c s) ;not in that order either playa
(intersection '(a b c) '(b b c)) ;=> ( c b) ;no order preservation
(set-difference '(a b c d e) '(b e)) ;=> (c d a) 

					;sequences - lists and vectors
(length '(a b c)) ;3
(subseq '(a b c d) 1 2); (b c)
(subseq '(a b c d) 1) ; (b c d)

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (let ((mid (/ len 2)))
	   (equal (subseq s 0 mid) ;(starts from 0 and goes to mid)
		  (reverse (subseq s mid))))))) ;starts from mid and goes to the other side

(defun mirror2? (s)
  (and (evenp (length s))
       (equal (reverse s) s)))
					;can also take a key
(sort '( 0 2 1 3 8) #'>)
(sort '( 0 2 1 3 8) #'<)

					;if lst is a list of tuples it can sort the list and return 
(defun nthmost (n lst)
  (nth (1- n)
       (sort (copy-list lst) #'>)))
(nthmost 2 '(0 2 1 3 8)) ;(8 3 2 1 0) a and picks the second one

(every #'oddp '(1 3 5)) ; T
(some  #'evenp '(1 3 5 2)); T

(every #'> '(1 3 5)
       '(0 2 4 1)); like the mapcar pics the shortest list to work through with the corresponding other list 
					;push and pull are macros
					;use
(setf xpush '(b))
(push 'a xpush)
(pop xpush)

					;use push to determine an itertive version of reverse for lists
					;see dolist is like  an iterator for a list
(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elem lst) ;for each element in the list
      (push elem acc))
    acc))
					;use
(our-reverse '(1 2 3 a blah b))

					;pushnew uses adjoin
(let ((x '(a b)))
  (pushnew 'c x)
  (pushnew 'a x)
  x)

					;proper lists end in a null
					;wont work for a list of nil
					;messes up for circular lists
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
	   (proper-list? (cdr x)))))
					;use
(consp (cons 'a 'b)) ;T
(consp (cdr (cons 'a nil))); T
(consp (cdr (cons 'a 'b))) ;NIL see how in this case the proper list doesn't end with a null that's all there
					;is to a proper list
(consp (cdr (cons nil nil))) ;NIL
(setf pair (cons 'a 'b))
(proper-list? pair) ;NIL
(proper-list? (cons pair nil)) ; ((A.B) . nil) => ((A.B)) => true

					;returns the same thing
'(a . (b . (c . nil)))
(cons 'a (cons 'b (cons 'c 'd)))

					;many ways to define this
					;just dot notation
'(a b)
'(a . (b . nil))
'(a . (b))
'(a b . nil)

					;a-lists a-list assoc-lists
(setf trans '((+ . "add") (- . "subtract")))
(assoc '+ trans)
(assoc '- trans)
(assoc '* trans)

					;our version of assoc list
(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
	 (if (eql key (car pair))
	     pair
	     (our-assoc key (cdr alist))))))


					;find the shortest path in a network
(setf min '((a b c) (b c) (c d)))
(assoc 'a min) ; (a b c)
					;so for the node a it'll give you all the nodes that touch it
(cdr (assoc 'a min)) ;(b c)

					;(defun shortest-path (start end net)
					;  (bfs end (list (list start)) net))

;;one that does the searching
					;(defun bfs (end queue net)
					;  (if (null queue)
					;      nil
					;      (let ((path (car queue))) ;say (a) for example
					;        (let ((node (car path))) ;say a for example
					;          (if (eql node end)
					;              (reverse path) ;
					;              (bfs end
					;                   (append (cdr queue) ;keep adding to the end of the end of the queue
					;                           (new-paths path node net))
					;                   net))))))

					;so it takes the current path and the node alont with the network
					;finds the node and the path to get to that place and returns it
					;if the path (b c) - node 'a - net ((a b c) (b c) (c d))
					;(cons '(b c) '())
					;(defun new-paths (path node net)
					;  (mapcar #'(lambda (n)
					;              (cons n path)) ;will give you a list of n along with the rest of the path 
					;          (cdr (assoc node net))));the nodes associaded

					;breadth-first
					;'a 'd ((a b c) (b c) (c d))
(defun shortest-path (start end net)
  (bfs end (list (list start)) net)) ; 'D ((A)) ((A B C) (B C) (C D))

					;now for the breadth-first
					;breadth-first - takes and endnode a queue of unexplored nodes and the network
					;see if node is the end node
					;if not append the nodes children to the end of the queue (to be searched) and then research it
					;this way node is found. but we need the path as well so watch
(defun bfs (end queue network)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path); then we've found the path (in backwards order)
	      (bfs end ;recurse 
		   (append (cdr queue) ;the rest of the queue
			   (new-paths path node network)) ;and the added child nodes (with paths)
		   network))))))
					;path (A)  - node 'A  - network ((A b c))
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net)))) ; (B C)
					;everything in this list gets gets the function applied to it

					;Exercises Chapter 3 chap 3 questions
					;2.write a version of union that preserves the order of the elements in the original lists:
					;(defun new-union (l1 l2)
					;  (cond
					;    ((null l1) l2)
					;    ((null l2) nil)
					;    ((not (eq nil (member (car l2) l1)))
					;     (cons l1
					;           (cons (car l2)
					;                 (new-union l1 (cdr l2))))))
					;    (t))

					;this doesn't work and neither does removing the last cons work
(defun new-union(l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) nil)
    ((eq nil (member (car l2) l1))
     (cons l1
	   (cons (car l2)
		 (new-union (cons l1
				  (car l2))
			    (cdr l2)))))
    (t
     (cons l1
	   (new-union l1
		      (cdr l2))))))
					;works
(defun new-union (l1 l2)
  (cond
    ((null l1) l2) ;will usually not be the case
    ((null l2) l1)
    ((eq nil (member (car l2) l1))
     (new-union (append l1
			(list (car l2)))
		(cdr l2)))
    (t
     (new-union l1 (cdr l2)))))

					;try another way with pushnew
					;works
(defun new-union-helper (l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) l1)
    (t
     (new-union-helper (pushnew (car l2) l1)
		       (cdr l2)))))
(defun new-union (l1 l2)
  (reverse (new-union-helper (reverse l1) l2)))

					;now use mapcar
					;works
(defun new-union-helper (l1)
  (let ((l2 nil))
    (mapcar #'(lambda (a)
		(pushnew a l2)) ;doesn't work with adjoin 
	    l1)
    (reverse l2)))
(defun new-union (l1 l2)
  (new-union-helper (append l1 l2)))

					;3; takes an element and then returns it if it's in the list
(defun occurance-helper (a l)
  (let ((curr-elem (assoc a l)))
    (if (not (eql nil curr-elem))
	(cons a (1+ (cdr curr-elem)))
	(cons a 1))))

(defun occurance (l)
  (let ((l2 nil))
    (dolist (obj l)
      (push (occurance-helper obj l2) l2))
    l2))
(defun occurance (l)
  (let ((l2 nil))
    (dolist (obj l)
      (push (occurance-helper obj l2) l2))
    (sort l2 #'> :key #'cdr )))

					;incf instead of 1+ because incf actually changes the meaning
(defun occurance2 (l)
  (let ((res nil))
    (dolist (obj l)
      (if (assoc obj res)
	  (incf (cdr (assoc obj res)))
	  (push (cons obj 1) res)))
    (sort res #'> :key #'cdr)))

					;try it again with mapcar!
(defun occurance-mapcar(l)
  (let ((res nil))
    (dolist (obj l)
      (mapcar #'occurance-helper (list obj) (list (list res))))
    res))



					;4 why does (member '(a) '( (a) (b))) return false
					;member by default works with equal! we want it to work with equal or equalp
					;i didn't know the answer to this one member needs to compare things right eql compares thing but since they're technically not the same object
					;explanation of eql and equal and also see page 44
(member '(a) '((a) (b))) ; NIL
(member '(a) '((a) (b)) :test #'equal) ; now it works! returns '(a) (b)

					;5 (pos+ '(7 5 1 4)) => '(7 6 3 7) ;bascially the number in the list plus 
					;using recursion
(defun pos+ (l)
  (pos+-helper 0 l nil))
					;using push
(defun pos+-helper (i l res)
  (cond
    ((null l) (reverse res))
    (t
     (pos+-helper (1+ i)
		  (cdr l)
		  (push (+ (car l) i) res)))))
					;using append 
(defun pos+-helper (i l res)
  (cond
    ((null l) res)
    (t
     (pos+-helper (1+ i)
		  (cdr l)
		  (append res (list (+ (car l) i))))))) 

					;do this using mapcar
					;and an helper function
(defun make-0-to-n-list (n pos l)
  (if (eql 0 n)
      l
      (make-0-to-n-list (1- n) (1+ pos) (append l (list pos)))))
					;(defun pos+ (l)
					;  (let ((poslist (make-0-to-n-list (length l) 0 nil)))
					;    (mapcar #'+ poslist l)))
(defun pos+ (l)
  (mapcar #'+ (make-0-to-n-list (length l)
				0
				nil)
	  l))
					;now do it with iterations
(defun pos+ (l)
  (let ((tup (copy-list l)))
    (do ((i 0 (1+ i))) ;when to start first thing do macro takes 
	((>= i (length tup)) tup) ;when to stop is next thing do macro takes
      (setf (nth i tup) (+ (nth i tup) i)))))

					;my first guess but this is wrong since we used cons lol
(defun govern-cons (x y)
  (cons y x))

					;see this way makes more sense
(defun govern-cons (x y)
  (let ((l (nil . nil)))
    (setf (cdr l) x
	  (car l) y)
    l))
					;make list but we need to use something like &rest
					;instead of cdr we need car
(defun govern-length (l)
  (cond
    ((null l) 0)
    (t
     (1+ (govern-length (cdr l))))))

(defun govern-member (a ls)
  (cond
    ((null ls) nil)
    ((eq a (car ls)) ;switch the cdr and the car here 
     (cdr ls))
    (t 
     (govern-member a (cdr ls)))))

					;3-7 modify the program in figure 3.6 to use fewer cons cells. (hint use dotted lists)
					;original
(defun compress (x)
  (if (consp x) 
      (compr (car x)  1 (cdr x))
      x))
					;same as original
(defun compr (elem n lst)
  (if (null lst)
      (list (n-elts elem n))
      (let ((next (car lst)))
	(if (eql next elem)
	    (compr elem (1+ n) (cdr lst))
	    (cons (n-elts elem n)
		  (compr next 1 (cdr lst))))))) 

					;make it so that this is a 
(defun n-elts (elem n)
  (if (> n 1)
      (cons n elem) ;see instead of making them all list to save space we can make them all dotted lists
      elem))

					;define a function that takes a list and prints it in dot notation
(defun showdots (l)
  (cond
    ((eq (car (last l))
	 (car l))
     (format nil "(~A . ~A)" (car l) (cdr l)))
    (t
     (format nil "(~A . ~A)" (car l) (showdots (cdr l))))))

					;use another method
					;doesn't work format t behaves funny
(defun showdots (l)
  (if (atom l)
      (format t "~A" l) ;nil is an atom so it returns nil
      (progn
	(format t "(~A . ~A )" (showdots (car l))
		(showdots (cdr l))))))

					;attempt 2
(defun showdots (l)
  (if (atom l)
      (format t "~A" l)
      (progn
	(format t "(")
	(showdots (car l))
	(format t " . ")
	(showdots (cdr l))
	(format t ")"))))


					;3-9 from the errata it says longest path with no duplicates
(defparameter *matching-paths* nil)
(defun longest-path (start end net)
  (find-all-paths end (list (list start)) net *matching-paths*))
					;network ((a b c) (b c) (c d))
(defparameter *network* '((a b c) ( b c) (c d)))

(defun find-all-paths (end queue net dest-paths)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end) ;if we find something that matches
	      (progn
		(push (reverse path) dest-paths) ;append it to the list of matching paths
		(find-all-paths end
				(cdr queue)
				net
				dest-paths))
	      (find-all-paths end
			      (append (cdr queue)
				      (new-paths path node net))
			      net
			      dest-paths))))))
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path)) ;say path is (A) then (B A) (C A)
	  (cdr (assoc node net)))) ;(B C)

(defun bfs2 (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (cond
	    ((eq node end)
	     (push (reverse path) *matching-paths*))
	    (t
	     (bfs2 end
		   (cdr queue)
		   (append (cdr queue)
			   (new-paths path node net)))))))))
(defun longest-path2 (start end net)
  (bfs2 end (list (list start)) net))

					;another attempt
(defun bfs3 (end queue net curr-path)
  (if (null queue)
      curr-path
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (progn
		(setf curr-path (reverse path))
		(bfs3 end
		      (cdr queue)
		      net
		      curr-path))
	      (bfs3 end
		    (append (cdr queue)
			    (new-paths path node net))
		    net
		    curr-path))))))
(defun longest-path3 (start end net)
  (bfs3 end (list (list start)) net *matching-paths*))



  ;;;;CHAPTER 4 chapter 4 chap 4
					;arrays
					;make an array with the make-array function takes a list and can take an initial value
(setf arr (make-array '(2 3) :initial-element nil))
					;reference an array
(aref arr 0 0) ;=> nil since that's what we set it up to be in the previous step
					;set specific indices of an array
(setf (aref arr 0 1) 'B) ;pick (aref) the elemnt and then setf it
					;it's literally typed like this in lisp the literal version of an array ie in the terminal type this
					;#2A((NIL B NIL) (NIL NIL NIL))
					;same as if we typed wha tarray was
(setf *print-array* t) ;and set it to false to see the actual location of the arrray

					;to make a one dim array - aka vector!!!- then just give a number instead of the array dimensions
(setf vec (make-array 4 :initial-element nil))
					;one d array is calle da vector! so call it with a vector
(vector "a" 'b 3 18)
					;svref stands for sv stands for "simple vector" - simple array not adjustable nor displaced nor has a fill-pointer. arrays are simple by default. simple vector is simple array one dim and elements of any type
(svref vec 0) 
(setf (svref vec 0) 20)

  ;;;; Utilities for operations on sorted vectors.

  ;;; Finds an element in a sorted vector. 

(defun bin-search (obj vec)
  (let ((len (length vec)))
    ;; if a real vector, send it to finder
    (and (not (zerop len))
	 (finder obj vec 0 (1- len)))))

(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (1+ end)))
  (let ((range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start)) ;or end in this case
	    obj
	    nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let ((obj2 (aref vec mid)))
	    (if (< obj obj2)
		(finder obj vec start (1- mid))
		(if (> obj obj2)
		    (finder obj vec (1+ mid) end)
		    obj))))))); if it's not less than and not > it must be the object!
(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))


;;these are all distructive
(defun reverse-vector (vec)
  (reverse-vec-helper vec 0 (truncate (/ (length vec) 2)) (1- (length vec))))

(defun reverse-vec-helper (vec low mid high)
  (do ((i low (+ 1 i)))
      ((= i mid))
    (progn
      (let* ((temp (aref vec i)))
	(setf (aref vec i) (aref vec (- high i)))
	(setf (aref vec (- high i)) temp))))
  vec)

(defun reverse-vec2 (vec)
  (reverse-vec-helper2 vec 0 (1- (length vec))))

(defun reverse-vec-helper2 (vec st end)
  (cond
    ((>= st end) vec)
    (t
     (let ((temp (aref vec end)))
       (progn
	 (setf (aref vec end) (aref vec st))
	 (setf (aref vec st) temp)
	 (reverse-vec-helper2 vec (1+ st) (1- end)))))))



;;strings are vectors of characters
;;"string" #\c is the character c
(char< #\c #\d) ;T
(char> #\a #\A) ;T
(char<=  #\B #\a); T
(char/= #\a #\a);Nil ;this checks if they're different 
;;returns the code for the character
(char-code #\a) ;97
					;returns the character for the code
(code-char 97) ; a
(sort "elbow" #'char<) ;accending order
(sort "asdflksaxcvjklj" #'char>) ;descending order 
;;strings are vectors so sequence and array functions worn on them
(aref "abc" 1) ;#\b
;;char is faster than aref
(char "abc" 1); #\b
;;use setf with char (or aref) to replace elements
(defun my-str-manipulator (some-str)
  (let ((str (copy-seq some-str)))
    (setf (aref str 3) #\K) ;or (setf (char str 3)) #\k
    str))
(equal "fred" "fred") ;T
(equal "fred" "Fred") ;nil
(string-equal "fred" "FRED"); T
;;build a string with format
(format nil "~A or ~A" "truth" "dare")
					;concatenate takes a symbol and for the type of result, puls
(format nil (concatenate 'string "blah" "blue" "butts"))
;;the type sequence includes both lists and vectors
;;lists are sequences as are vectors (and a string is vector of chars)
(mirror? '(a b b a)) ; T
(mirror? "abba") ; T since both sequences!

					;get anything from any sequence
(elt '(a b c) 1)
(elt "abcd" 2) ;messes up if indexing out of range

					;using elt we could write a version of mirror? that would be faster
(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (do ((forward 0 (1+ forward)) ;2 variables set up forward
	      (back (1- len) (1- back))) ;another variable set up back starts at (- len 1) and we decrement it by one every time 
	     ((or (> forward back) ;stop when forward is bigger than end or when
		  (not (eql (elt s forward)
			    (elt s back)))) ;the stopping conditions
	      (> forward back)))))) ;the second part of this is just to print something when the do macro is done
  ;;;lists only allow sequential access where as vecotrs allow random access so it's just as cheap to reach
  ;;;one element is as easy to get to as is any other element 
					;:key  identity T
					;:test eql 
					;:from-end nil
					;:start 0
					;:end nil
(position #\a "fantasia") ; 1
(position #\a "fantasia" :start 3 :end 5) ; 4
(position #\a "fantasia" :from-end t) ; with it set to t it'll be position number 7 from the start (not like 0 from end or anything weird)
(position 'a '((c d) (a b)) :key #'car) ;answer is 1!!! there are 2 total elements in there
;;car will be applied to each element of a sequence before it's considered
  ;;;so we ask for pos of first element whose car is the symbol a 
(position 'a '((b a) (c d a) (a b c)) :key #'car) ; 2
(position 'a '((b a) (c d a) (a b c)) :key #'cadr); 0
(position 'a '((b a) (c d a) (a b c)) :key #'caddr);1
(position 'a '((b a) (c d a) (Z b c)) :key #'car); nil
;;:test takes 2 args in this case pg 64
(position '(a b) '((a b) (c d))) ; will be false since by default it uses eql!!!!! 
(position '(a b) '((a b) (c d)) :test #'equal) ;0 ie that's the position number
;;can be a function of 2 args as well
;;see how it compares to multiple things
(position 3 '(1 0 7 5) :test #'< ) ; 2
(position 3 '(8 9 5 6 4 1 7 5) :test #'> ) ;5 the fith position it's greater (> 3 1) == T 

;;this way didn't work 
;;couldn't find the #\  the space is needed 
					;(defun second-word (sting)
					;  (let ((p1 (+ (position #\ sting) 1)))
					;    (subseq sting p1 (position #\ sting :start p1))))

;;try with the sapce character
(defun second-word (str)
  (let ((p1 (+ (position #\space str) 1)))
    (subseq str p1 (position #\space str :start p1))))
;;use
(second-word "Form follows function.")
  ;;;see if an element satisfies a predicate of one argument
;;takes a function and a sequence and returns the position of the first element satisfying hte function:
(position-if #'oddp '(0 2 3 4 5)) ;2 since that's the position of the number 3 in that statment
					;find is like member and member-if for sequences
(find #\a "cat") ;returns #\a and not the rest of the characters
(find-if #'characterp "1ham") ;1 if we used "" then it would return nil - doesn't take :test
;;find with :key is better than find-if
(defparameter *some-list* '((blue)(bunny)(complete)))
(find-if #'(lambda (x)
	     (eql (car x) 'complete))
	 *some-list*)
(find 'complete *some-list* :key #'car)
;;leaves each last occurance of any element of a sequence
(remove-duplicates "abracadabra")

(reduce #'cons '(a b c d))
(reduce #'intersection '((b r a d ' s) (b a d) (c a t))) ; see the only thing common in all of them is a 
  ;;;;4.5 parsing dates
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str
			       :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\space ))))



;;see how this get's the first thing that isn't an alpha-char-p (like a space or a . or a $ or something)
(position-if #'(lambda (c) 
		 (not (funcall 'alpha-char-p c))) 
	     "9aas8dasdlj12#" 
	     :start 1);4
(position-if #'(lambda (c) 
		 (not (funcall 'alpha-char-p c))) 
	     "9aas8dasdlj12#" 
	     :start 5);11

;;notice
(subseq "abc" 1 nil) ; "bc" see how with nil it's the same thing
(defun tokens1 (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str
			       :start p1)))
	  (print p2))
	nil)))
;;use - also notice that #'constituent or 'constituent works with funcall
(tokens "ab12 3cde.f
	  gh" #'constituent 0)

  ;;;date parsing "13 Jun 1983"
(defun parse-date1 (str month-parser)
  (let ((tok (tokens str #'constituent 0)))
    (list (parse-integer (first tok))
	  (funcall month-parser  (second tok))
	  (parse-integer (third tok)))))
;;one dim array - ie a vector
(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sept" "oct" "nov" "dec"))
;;define parse-month
(defun parse-month (str)
  (let ((pos (position str month-names :test #'string-equal)))
    (if pos
	(1+ pos) ;jan is the 1st month noth the 0th month
	nil)))
;;we use string-equal so that means that it's not case sensitive
(defun parse-month1 (str)
  (let ((pos (position str month-names :test #'string-equal)))
    pos))
(defun parse-date (str)
  (let ((tok (tokens str #'constituent 0)))
    (list (parse-integer (first tok))
	  (parse-month   (second tok))
	  (parse-integer (third tok)))))
;;use
(parse-date "13 Jun 1984")
(parse-date1 "13 Jun 1984" #'parse-month1)

;;use this to debug
(defun read-integer-1 (str)
  (if (every #'digit-char-p str) ;only proceed if every thing in string is a char
      (let ((accum 0))
	(dotimes (pos (length str)) ;dotimes takes a (dotimes (x 5 "yo") (printf "hello")) ;
	  (setf accum (+ (* accum 10)
			 (digit-char-p (char str pos)))) ;with dotimes the value of pos increases each time
	  (print pos))
	accum)
      nil))
(defun read-integer-1 (str)
  (if (every #'digit-char-p str) ;only proceed if every thing in string is a char
      (let ((accum 0))
	(dotimes (pos (length str)) ;dotimes takes a (dotimes (x 5 "yo") (printf "hello")) ;
	  (setf accum (+ (* accum 10)
			 (digit-char-p (char str pos))))) ;with dotimes the value of pos increases each time
	accum)
      nil))

  ;;;4.6 structures
(defun block-height (b) (svref b 0)) ;use (block-height #(1 2 3)) 2-d array looks like this #2A(1 1 2 3 blah)
					;defstruct makes a structure
					;think of a structure as a vector that all kinds of functions get defined for you
(defstruct point
  x
  y)
					;make-point point-p copy-point point-x point-y
(setf p (make-point :x 0 :y 0))
(point-x p) ; 0
(point-y p) ; 0
(setf (point-y p) 2) ;and check p now
					;defiing a structure also defines a type of that name each point will be of type point, then structure then atom and then t.
					;use point-p to see if something is a point as well as using the typep

(defstruct polemic
  (type (progn
	  (format t "What kind of polemic was it? ")
	  (read)))
  (effect nil))
					;make-polemic polemic-p copy-polemic polemic-type polemic-effect
;;use
					;(setf pol1 (make-polemic)); this will make with whatever you say as type after the ;#S(POLEMIC :TYPE NEW :EFFECT NIL) ;type and effect are the keyworkds 

(defstruct (point1 (:conc-name p) ;says call me p instead of point1- gets replaced by p, and is more confusing
		   (:print-function print-point)) ;says for printing use the function print-point (which is defined below) ;takes structure to be printed, place where is to be printed and a third arg which we can ignore
  (x 0)
  (y 0))
;;the above says call it p instead of point1- also the make-point1 and py and px will be used-
(defun print-point1 (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))
;;use
(setf pt1 (make-point1)) 
(setf (px pt1) 3)
(setf (py pt1) 'blah)

  ;;;binary search tree 4.7
;;a bst (is made of nodes - has a left a right) and can either be null or be null or anohter bst 
;;node has left or right
					;(defstruct (node (:print-function ;special function needs an object, a stream (format function takes the stream) and depth (to be ignored)
					;		   (lambda (n s d)
					;		     (format s "#<~A>" (node-elem n))))) ;all the functions for the node see point1 example above ; node-elt is like node-l or node-r except the main element itself
					;   elem (l nil) (r nil)) ;all the variables for the node

(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elem n)))))
  elem (l nil) (r nil))


;;we are re-createing this thing making the nodes and so on
;;so the thing on the left will be smaller so insert it on the left
;;this creates a copy of the bst (doesn't modify the original bst) 
(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elem obj)
      (let ((elem (node-elem bst)))
	(if (eql obj elem)
	    bst
	    (if (funcall < obj elem) ;gets passed in as a function #'< in most cases (funcall #'< 3 4)
		(make-node
		 :elem elem
		 :l (bst-insert obj (node-l bst) <) ;the left node will be the one that has the new obj
		 :r (node-r bst)) ;the right node will be same as it was for this node originally
		(make-node
		 :elem elem
		 :r (bst-insert obj (node-r bst) <) ;the obj is bigger so it belongs on the right - so we remake the node with the left side being the same and the object placed on the right - if it fits that is
		 :l (node-l bst))))))) ;this time the left side should be the same

(defun bst-find (obj bst <)
  (if (null bst) ;means we haven't found it
      nil
      (let ((elem (node-elem bst)))
	(if (eql obj elem)
	    bst
	    (if (funcall < obj elem)
		(bst-find obj (node-l bst) <)
		(bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(setf nums nil)
					;(dolist (x '(5 8 4 2 1 9 6 7 3)) ;goes through each one and 
					;  (setf nums (bst-insert x nums #'<))
					;  (print x));goes one at a time and prints it
(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums* (bst-insert x nums #'<)))
;;use
nums ;see how it uses the node 's :print-function notice how the current node is the one that gets printed
(bst-find 3
	  nums 
	  #'(lambda (x y) 
	      (progn
		(format t "comparing ~A to ~A ~%" x y)
		(< x y))));;this is why it's a function 


(setf *nums1* nil)
(dolist (x '(4 5 6))
  (setf *nums1* (bst-insert x *nums1* #'<)))


(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elem (node-elem bst)))
	(if (eql obj elem)
	    (percolate bst)
	    (if (funcall < obj elem)
		(make-node
		 :elem elem
		 :l (bst-remove obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elem elem
		 :l (node-l bst)
		 :r (bst-remove obj (node-r bst) <)))))))
;;need to hande the case when a branch has things underneath it
;;something that percolates bubbles up
(defun percolate (bst)
  (cond
    ((null (node-l bst))
     (if (null (node-r bst))
	 nil
	 (rperc bst)))
    ((null (node-r bst))
     (lperc bst))
    (t 
     (if (zerop (random 2)) ;returns either 1 or 0 
	 (lperc bst)
	 (rperc bst)))))
;;moves the right-side up
(defun rperc (bst)
  (make-node :elem (node-elem (node-r bst))
	     :l (node-l bst)
	     :r (percolate (node-r bst)))) ;notice it calls percolate again so that everyting is once again movedup
;;moves the left side up
(defun lperc (bst)
  (make-node :elem (node-elem (node-l bst))
	     :r (node-r bst)
	     :l (percolate (node-l bst))))

;;use
(bst-find 4 nums #'<) ; <4>
(setf nums (bst-remove 5 nums #'<))
(bst-find 4 nums #'<)

(defun bst-traverse (fn bst)
  (when bst ;if condition is true it will do all of the following
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elem bst))
    (bst-traverse fn (node-r bst))))
;;use
(bst-traverse #'princ nums) ;princ just prints one object
(bst-traverse #'(lambda (x)
		  (print  x))
	      nums)

;;;;;;;;;;;;;;sidetrack;;;;;;;;;;;;;;;
;;do a breadth first traverse - make it work with numbers first -- see this doesnt' work but we can learn from it
(defun bst-bft-helper (nodes)
  (cond
    ((null nodes) nil)
    (t
     (let ((curr (car nodes))
	   (rest (cdr nodes)))
       (progn
	 (print curr)
	 (bst-bft-helper (append1 rest (remove-if #'null (list (node-elem (node-r nums))
							       (node-elem (node-l nums)))))))))))

;;the traversal function without depths and knowing who the child is
(defun bst-breadth-first (bst fn)
  (bst-bft-helper (list bst) fn))
;;no this won't work - see how the nums thing needs to be there
(defun bst-bft-helper (nodes fn)
  (cond 
    ((null nodes) nil)
    (t
     (let ((curr (car nodes))
	   (rest (cdr nodes)))
       (progn 
	 (funcall fn curr)
	 (bst-bft-helper (append rest
				 (remove-if #'null (list (valid-node (node-l curr))
							 (valid-node (node-r curr)))))
			 fn))))))

;;returns either the current node back or returns nil
;;see we don't actually need this
(defun valid-node (node)
  (cond
    ((or (null node)
	 (null (node-elem node)))
     nil)
    (t
     node)))

;;ignore this one
;;another one with the - n current node - depth - acc (2 (a b)) means that at level 2 the 
(defun bst-bft-depth-heler (n d acc)
  (cond
    ((null n) acc)
    (t
     (let ((curr (car n)))))))

;;too complex			
(defun new-children (child-nodes d)
  (let* ((acc (1+ d)))
    (mapcar #'(lambda (n)
		(remove-if #'null (list (node-l n)
					(node-r n))))
	    child-nodes)))

;;correct definition
;;given a list of nodes 
(defun new-children (child-nodes)
  (mapcar #'(lambda (n)
	      (remove-if #'null (list (node-l n) (node-r n))))
	  child-nodes))

;;given a node find all the children 
(defun new-childs (node)
  (remove-if #'null (list (node-r node) (node-l node))))

;;given a list of nodes push them both out using an accumulator
(defun childs-childs (nodes &optional d)
  (let ((acc nil))
    (dolist (n nodes)
      (push (new-childs n) acc))
    acc))

(childs-childs (list nums)) ;((#<8> #<2>))
(new-children (list nums))  ;((#<2> #<8>))

;;say we have alist like so ( a (b)) how do we make it (a b)
;;use reduce !
(reduce #'append '((a b) (d e)))
(reduce #'append '((a b) nil))
(reduce #'append (new-children (list nums)))
(new-childs nums)
(new-children (list nums nums nums)) ;takes any list of nodes and finds all their children but returns them in a ( (a b) (c d) nil) type of list
(reduce #'append (new-children (list nums nums nums))) ;this takes that weird list and makes it just a list of the child nodes
(new-children (reduce #'append (new-children (reduce #'append (new-children (reduce #'append (new-children (list nums nums nums))))))));see how if we reduce this it goes back down 


;;breadth first traversal - breadth first search binary tree
(defun bst-bf-lists (node fn)
  (bst-bft-helper2 (list node) fn))
					;Cond has an implicit progn
					;takes a list of nodes and then finds a list of it's children continues this until there are no more children
(defun bst-bft-helper2 (nodes fn)
  (cond
    ((null nodes) nil)
    (t
     (funcall fn nodes) ;print the current list of nodes
     (bst-bft-helper2 (reduce #'append (new-children nodes))
		      fn))))
;;takes a list of child nodes and for each of them will find all the children
;; ((#<4> #<9>)) for example - notice with bst-bft-helper2
(defun new-children (child-nodes)
  (mapcar #'(lambda (n)
	      (remove-if #'null (list (node-l n) (node-r n))))
	  child-nodes))

;;see how this prints out all the ones 
(bst-bf-lists nums #'princ)
(bst-bf-lists nums #'print)

;;is there a function we can pass that will also print out the level
(let ((depth 0)
      (depthlist '()))
  (defun reset-depth ()
    (setf depth 0))
  (defun reset-depthlist ()
    (setf depthlist '()))
  (defun ret-depthlist()
    depthlist)
  (defun add-depths (node-list)
    (cond
      ((null node-list) nil)
      (t
       (push (append (list (1+ depth)) node-list) depthlist)
       (setf depth (1+ depth))))))

;;this shit
(defun show-depths (bst)
  (progn
    (reset-depth)
    (reset-depthlist)
    (bst-bf-lists bst #'add-depths)))

(show-depths nums)
(ret-depthlist) ;;((4 #<1> #<7>) (3 #<3> #<6> #<9>) (2 #<2> #<8>) (1 #<4>))
(reverse (ret-depthlist)) ;;((1 #<4>) (2 #<2> #<8>) (3 #<3> #<6> #<9>) (4 #<1> #<7>))

;;;more attempts gone awry
					;(defun new-children1 (child-nodes d)
					;  (let ((acc (list (1+ d))))
					;    (mapcar #'(lambda (n)
					;		      (cons n acc))
					;	    (mapcar #'(lambda (n)
					;			(remove-if #'null (list (node-l n)
					;						(node-r n))))
					;		    child-nodes))))

;;no this won't work - see how the nums thing needs to be there
(defun bst-bft-helper1 (nodes fn)
  (cond 
    ((null nodes) nil)
    (t
     (let ((curr (car nodes))
	   (rest (cdr nodes)))
       (progn 
	 (funcall fn curr)
	 (bst-bft-helper (append rest
				 (remove-if #'null (list (node-l curr)
							 (node-r curr))))
			 fn))))))
(defun bst-breadth-first1 (bst fn)
  (bst-bft-helper1 (list bst) fn))
					;see how this is the same the whole valid node thing is bs
(bst-breadth-first1 nums #'princ)

;;;;;;;;;;;;;sidetrack;;;;;;;;;;



 ;;;Hash Tables
;;like fast associated pairs - assoc - used when under 10 elements else use this
(setf ht (make-hash-table)) ;hash tables fast when over 10 values and return 2 value
(gethash 'color ht) ;2 return values
;;to save a value use setf with gethash
(setf (gethash 'color ht) 'red) ; 'RED and T because it's both of those things
(setf bugs (make-hash-table)) ;function that returns a hash table
;;push is abbreviation for setf
(push "Doesn't take any keyword arguments"
      (gethash #'our-member bugs)) ;see how we used a function as the key - keys can be of any type
(gethash #'our-member bugs) ; "Doesn't...." T  ; shows us the value the string "doesnt take keyword.." and also returns true to show that it found something
					;(gethash #'made-up-function bugs) ;gives an error - not nil - since #'made-up-function hasn't been defined
(gethash #'+ bugs) ; NIL NIL
;;use hashtables to represent sets
(setf fruit (make-hash-table)) ;#'make-hash-table returns a hash table
(setf (gethash 'apple fruit) t) ;making it all true see what you're doing in the set means it's true
;;this way an empty set is represented by a new make-hash-table call
(push t (gethash 'apricot fruit))
(setf (gethash 'banana fruit) t)
(gethash 'apricot fruit) ; T T
;;to remove things from a hash use remhash
(remhash 'apricot fruit) ; T  
(gethash 'apricot fruit) ; nil nil 
 ;;;maphash cool function - takes a function with two arguments - takes a hash table
;;lets see a cool example
(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant
      (gethash 'style ht) 'awesome)
(maphash #'(lambda (k v)
	     (format t "~A = ~A~%" k v))
	 ht)
;;another example
;;see how the function is called on every key value pair of the function - no order specified
(maphash #'(lambda (key value)
	     (format t "~A = ~A~%" (concatenate 'string  
						(symbol-name key) "blah")
		     value))
	 ht)
					;see it always returns nil as the last argument try it.
					;an accumulator function will store all these values
;;use the :size keyword if you know how big you want it to be - make it really big - or small - or say it's the number of elements on average that it will be able to accomodate before being expanded
(make-hash-table :size 5)
;;hash tables need to have some sort of equity operator -- like all things right now they default to eql
;;eql is the equity operator but the :test argument can calso work with
;;eq
;;equal
;;equalp
;;example!
;;see with hash-tables this is one of the tradeoffs in a list we would just send in the :test #'equal
;;but for hash-tables them being faster we need to set the equality when they're made
(setf writers (make-hash-table :test #'equal))
(setf (gethash '(ralph waldo emerson) writers) t)

(setf arry1 (make-array '(3 3)
			:initial-contents
			'((a b c)
			  (d e f)
			  (g h i))))
;;just as a test
(setf arry2 (make-array '(3 3)
			:initial-element nil))
 ;;;;chapter 4 exercises chap 4 exercises
;;4-1
(defun quarter-turn (an-arry)
  (cond
    ((null an-arry) nil)
    (t
     (let ((ret-arry (make-array (array-dimensions an-arry)
				 :initial-element nil ))
	   (size (car (array-dimensions an-arry))))
       (do ((curr 0 (1+ curr))
	    (currz (1- size) (1- currz)))
	   ((>= curr size) ret-arry)
	 (do ((i 0 (1+ i)))     ;setup all the vars
	     ((or (> i (1- size))  ;setup the stopping conditions
		  nil))
	   (progn
	     (setf (aref ret-arry i currz) (aref an-arry curr i)))))))))
;;check the value of arry2 and notice it's changed
(setf arry2 (quarter-turn arry1))

;;4-2
;;use reduce to 
(reduce #'cons '(a b c d)) ;  (((a . b) . c) . d)
(defparameter *test-list* nil)
(setf *test-list* (reduce #'cons '(a b c d) :from-end t :initial-value nil))
;;4-2a
					;use this function
(defun copy-list-with-reduce (list-to-copy)
  (reduce #'cons list-to-copy :from-end t :initial-value nil))
;;4-2b - reverse someting for lists
(defun reverse-cons-helper (a b)
  (cons b a))
(defun reverse-list-with-reduce (list-to-reverse)
  (reduce #'reverse-cons-helper list-to-reverse :initial-value nil))

					;using lambda instead of the one-time function reverse-cons-helper
(defun reverse-list-with-reduce2 (list-to-reverse)
  (reduce #'(lambda (a b)
	      (cons b a))
	  list-to-reverse
	  :initial-value nil))


;;4-3
;;make trinary search tree
(defstruct (tri-node (:print-function
		      (lambda (tn s d) ;tri-node stram depth (to ignore)
			(format s "#<~A>" (tri-node-elem tn)))))
  elem (l nil) (m nil) (r nil))
;;4-3a
					;make a tri-node
(defparameter *trinode1* nil)
(defparameter *trinode2* (make-tri-node))
(setf *trinode1* (make-tri-node :elem 10))
(setf (tri-node-l *trinode1*) (make-tri-node :elem 7))
(setf (tri-node-m *trinode1*) (make-tri-node :elem 8))
(setf (tri-node-r *trinode1*) (make-tri-node :elem 9))
(setf (tri-node-l (tri-node-l *trinode1*)) (make-tri-node :elem 77))
(setf (tri-node-l (tri-node-l (tri-node-l *trinode1*))) (make-tri-node :elem 1000))

					;first attempt
(defun copy-tst (t-orig t-copied)
  (cond
    ((null t-orig) nil)
    (t
     (setf t-copied (make-tri-node  ;doesn't work becuase how does it know what t-copied is - it should still retun the node that is made, just can't assign it to t-copied
		     :elem (tri-node-elem t-orig)
		     :l (tri-node-l t-orig)
		     :r (tri-node-r t-orig)
		     :m (tri-node-m t-orig))))))

(copy-tst *trinode1* *trinode2*)
(setf *trinode2* (make-tri-node :elem (tri-node-elem *trinode1*)
				:l (tri-node-l *trinode1*)
				:r (tri-node-r *trinode1*)
				:m (tri-node-m *trinode1*)))
;;when we do it this way they both point to the same thing
(defun copy-tst-attempt2 (tst-orig)
  (cond
    ((null tst-orig) nil)
    (t
     (make-tri-node :elem (tri-node-elem tst-orig) ;this is SUPPOSE TO POINT TO A numbrer!!!
		    :l    (tri-node-l tst-orig);;this is SUPPOSE TO POINT TO A TRI-NODE!!! NOT A NUMBER!j ;just copying it over
		    :r    (tri-node-r tst-orig) ;just copying it over instead of making a copy
		    :m    (tri-node-m tst-orig))))) ;just copying it over

(setf *trinode2* (copy-tst-attempt2 *trinode1*))
;;both are the same
(tri-node-l (tri-node-l *trinode1*))
(tri-node-l (tri-node-l *trinode2*))
(setf (tri-node-l (tri-node-l *trinode1*)) (make-tri-node :elem 15))
(tri-node-l (tri-node-l *trinode1*)) ;this will change to 15
(tri-node-l (tri-node-l *trinode2*)) ;this will also change to 15

;;the reason this happens is because we don't have a make-tri-node, but instead we're just copying it over for the children!!!
;;see how if we change one they both change

(setf (tri-node-l (tri-node-l *trinode2*)) (make-tri-node :elem 200))

(tri-node-l (tri-node-l *trinode2*)) ;both are 200 
(tri-node-l (tri-node-l *trinode1*)) ;boht are 200

 ;;;this is where eql comes useful
;;we want to see if the two nodes point to the same object this is how
(eql (tri-node-l (tri-node-l *trinode1*))
     (tri-node-l (tri-node-l *trinode1*))) ;T SEE HOW IT'S THE SAME THING!!!THING

;;the attempt that works
(defun copy-tst-attempt3 (tst-orig)
  (cond
    ((null tst-orig) nil)
    (t
     (make-tri-node :elem (tri-node-elem tst-orig)
		    :l (copy-tst-attempt3 (tri-node-l tst-orig))
		    :m (copy-tst-attempt3 (tri-node-m tst-orig))
		    :r (copy-tst-attempt3 (tri-node-r tst-orig))))))

(defparameter *trinode3* (make-tri-node))

(setf *trinode3* (copy-tst-attempt3 *trinode1*))

(eql (tri-node-l (tri-node-l *trinode1*))
     (tri-node-l (tri-node-l *trinode3*))) ;NIL (see how they're not the same)

(setf (tri-node-l (tri-node-l *trinode2*)) (make-tri-node :elem -99))
(tri-node-l (tri-node-l *trinode3*)) ;now they're different
(tri-node-l (tri-node-l *trinode1*)) ;now they're different
					;add something to some node of the original tri-node and see the result
(setf (tri-node-l (tri-node-l (tri-node-l *trinode1*))) (make-tri-node :elem 1000))

;;3b - find functionality for tri-node
(defun find-tri-node (obj tri-search-tree)
  (cond
    ((null tri-search-tree) nil)
    ((eql obj (tri-node-elem tri-search-tree))
     obj)
    (t
     (progn
       (find-tri-node obj
		      (tri-node-l tri-search-tree))
       (find-tri-node obj
		      (tri-node-r tri-search-tree))
       (find-tri-node obj
		      (tri-node-m tri-search-tree))))))
(find-tri-node 1000 *trinode1*) ;nil
(find-tri-node 8 *trinode1*) ;will be found since it's in the middle path
;;notice the first one will always return nil..unless it's some fluke and it ends up being in theh middle path or the first thing

;;what we want to do is only search another thing if the first one is nil
(defun find-tri-node2 (obj tri-search-tree)
  (cond
    ((null tri-search-tree) nil)
    ((eql obj (tri-node-elem tri-search-tree))
     tri-search-tree)
    (t
     (or (find-tri-node2 obj (tri-node-l tri-search-tree))
	 (find-tri-node2 obj (tri-node-m tri-search-tree))
	 (find-tri-node2 obj (tri-node-r tri-search-tree))))))
					;use
;;see how they all work this time especially after you return the object htis time
;;the object gets printed correctly because of the :print-function method that you defined in struct
;;since it prints the first elem
(find-tri-node2 1000 *trinode1*)
(find-tri-node2 1000 *trinode3*)
(find-tri-node2 -99 *trinode2*)

;;fucntion that takes a bst and returns list of elements from greatest to least
(defparameter nums2 nil)

(dolist (x '(5 8 4 2 1 9 6 7 3))
  (print x)
  (setf nums2 (bst-insert x nums2 #'<)))

;;reverse-traverse
;;highest-to lowest
(defun bst-reverse-traverse (fn bst)
  (when bst
    (bst-reverse-traverse fn (node-r bst))
    (funcall fn bst)
    (bst-reverse-traverse fn (node-l bst))))
;;notice the difference between these two
(defun bst-reverse-traverse (fn bst)
  (when bst
    (bst-reverse-traverse fn (node-r bst))
    (funcall fn (node-elem bst))
    (bst-reverse-traverse fn (node-l bst))))

(defun bst-adjoin (bst obj <)
  (if (null bst)
      (make-node :elem obj)
      (let ((elem (node-elem bst)))
	(if (eql elem obj)
	    bst
	    (if (funcall < obj elem)
		(make-node 
		 :elem elem
		 :l (bst-adjoin (node-l bst) obj <)
		 :r (node-r bst))
		(make-node
		 :elem elem
		 :l (node-l bst)
		 :r (bst-adjoin (node-r bst) obj <))))))) 

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elem obj)
      (let ((elem (node-elem bst)))
	(if (eql obj elem)
	    bst
	    (if (funcall < obj elem) ;gets passed in as a function #'< in most cases (funcall #'< 3 4)
		(make-node
		 :elem elem
		 :l (bst-insert obj (node-l bst) <) ;the left node will be the one that has the new obj
		 :r (node-r bst)) ;the right node will be same as it was for this node originally
		(make-node
		 :elem elem
		 :r (bst-insert obj (node-r bst) <) ;the obj is bigger so it belongs on the right - so we remake the node with the left side being the same and the object placed on the right - if it fits that is
		 :l (node-l bst)))))))

;;can we do adjoin without remaking the whole thing?
(defun bst-adjoin2 (obj bst <)
  (if (null bst)
      (make-node :elem obj)
      (if (eql obj (node-elem bst))
	  (node-elem bst)
	  (if (funcall < obj (node-elem bst))
	      (bst-adjoin2 obj (node-l bst) <)
	      (bst-adjoin2 obj (node-r bst) <)))))
;;no ofcouse it doesn't because we ony ever make one node

(defparameter *nums3* nil)
(dolist (x '(5 8 4 2 1 9 6 7 3))
  (print x)
  (setf *nums3* (bst-adjoin2 x *nums3* #'<)))
;;look at the value of *nums3* see how it just went back to orig! 

 ;;;exercise 4-6

;;wrong attempt
					;(defun assoc-to-hashtbl (assoc-list hashtbl)
					; (cond
					;   ((null assoc-list) hashtbl)
					;   (t
					;     (assoc-to-hashtbl (cdr assoc-list)
					;                       ()))))
;;mapcar takes a function and a list
;;use this!!
(mapcar #'(lambda (n)
	    (princ n))
	trans)

(defun assoc-to-hashtbl (a-list)
  (progn
    (let ((a-ht (make-hash-table)))
      (assoc-to-hashtbl-helper a-list a-ht)
      a-ht)))
;;notice how with push something weird happens it pushes a list always!!
(defun assoc-to-hashtbl-helper (a-list a-hashtbl)
  (cond
    ((null a-list) a-hashtbl)
    (t
     (mapcar #'(lambda (alist)
		 (setf   (gethash (car alist) a-hashtbl) (cdr alist)))
	     a-list))))
;;with setf it doesn't make it into a list!
;;and it works! 
(defun assoc-to-hashtbl-helper (a-list a-hashtbl)
  (cond
    ((null a-list) a-hashtbl)
    (t
     (mapcar #'(lambda (alist)
		 (setf   (gethash (car alist) a-hashtbl) (cdr alist)))
	     a-list))))

(defparameter *test-hash* (assoc-to-hashtbl trans))
(gethash '+ *test-hash*) 

 ;;;exercise 4-7
;;go from hashtable to alist
(defun hashtbl-to-assoc (hashtbl)
  (progn
    (let ((alist nil))
      (maphash #'(lambda (k v)
		   (push (cons k v) alist))
	       hashtbl)
      alist)))

(defparameter *test-alist* (hashtbl-to-assoc *test-hash*))
;;both will be the same
(assoc '+ *test-alist*) 
(assoc '+ trans) 

 ;;;;chapter 5 - Control
 ;;;blocks - progn block and tagbody

;;progn returns the last thing
(progn 
  (format t "a")
  (format t "b")
  (+ 11 12))
;;block - a progn with an emergency exit - first symbol is the name of the block 
;; you can use return-from with the blocks name
(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this line."))
;;implicitly we can use the block with a nil
;;return macro returns it's argument as the value of an enclosing block named nil
(block nil
  (return 27))
;;a lot of operators body of expressions implicity enclose the body in a block named nil.
;;implicilty enclose the body in a block named nil. 
;;all iterations
(dolist (x '(a b c d e f))
  (format t "~A " x)
  (if (eql x 'c)
      (return 'done)))
;;even defun is implicitly enclosed in a block with the name as the same name as the function!
(defun foo ()
  (return-from foo 20))
;;if i change the return-from to return then everything messes up!
;;since there is no nil block (but instead a block named foo we can return using return-from instead of return)
(defun read-integer2 (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
	(if i
	    (setf accum (+ (* 10 accum) i))
	    (return-from read-integer nil))))
    accum))
;;use a tagbody example never used explicitly in most code - mostly something other operators are built on
(tagbody
   (setf x 0)
 top
   (setf x (+ 1 x))
   (format t "~A " x)
   (if (< x 10) (go top)))

;;let takes a body of code, establishes new variables for use in the body
;;creates new lexical contexts
(let ((x 7)
      (y 2))
  (format t "Number")
  (+ x y))

;;let is like a lambda call. so lambda is like the same thing as calling something by the function name
;;lambda is like the name of a function, so we can use let like a lambda
((lambda (x)
   (+ x 1)) ;is like a function call
 3) ; is the argument

((lambda (x y)
   (format t "Number")
   (+ x y))
 7 ;an argument to the function
 2) ;an arugment to the function

;;wrote the same thing out as a function so i wouldn't have to type that shit out
;;let and lambda
(defun show-let()
  ((lambda (x y)
     (format t "Number")
     (+ x y))
   7
   2))
;;see this won't do what we think
(let ((x 2)
      (y (+ x 1)))
  (+ x y))

(defun show-let2 ()
  (let ((x 2)
	(y (+ x 1)))
    (+ x y)))
;;the above is the same as
((lambda (x y) (+ x y)) 2
 (+ x 1))
;;see how it can't be the same! 
(defun show-let3()
  (let* ((x 2)
	 (y (+ x 1)))
    (format t "~A ~A" x y)))
;;it's the same thing as
;;so a bunch lets within lets
(defun show-let3a()
  (let ((x 1))
    (let ((y (+ x 1)))
      (+ x y))))

(defun show-let4()
  (let ((x 3)
	(y 2))
    (format t "~A ~A" x y)))

((lambda (x y) (format t "~A ~A" x y)) 3
 2)
					;initial values always go to nil
(defun show-let5()
  (let (x y)
    (list x y)))
;;takes a pattern - one or more vars arranged in the form of a tree - and binds them to the corresponding
;;parts of some actual tree
(destructuring-bind (w (x y) . z) '(a (b c) d e lol blah)
  (list w x y z))
(destructuring-bind (w (x y) . z) '(a (b c) d e lol blah)
  (list w z)); (a (d e lol blah))


;;generilzation of a let
;;a let does binding
;;something to a variable
;;this thing binds to multiple variables
;;binds one parse tree (looks like a list) to another parse tree (which is also a list in lisp)
;;PARSE TREES ARE LISTS!!!!!!
(defun test-destructruing-binding ()
  (destructuring-bind (w (x y b) . z) '(a (b c c) d e f g)
    (list b w x y z)))

 ;;;5.3
;;body will be evaluated if test expression is true
;;when is like a cond
;;has an implicit progn block
(defun test-when (num)
  (when (oddp num)
    (format t "Hmm, that's odd. ~%")
    (format t "it's like a progn block that only gets excuted when statement is true")
    (+ num 1)))
;;returns nil otherwise

(defun test-when2 (num)
  (if (oddp num)
      (progn
	(format t "Hmm, that's odd. ~%")
	(format t "it's like a progn block that only gets excuted when statement is true")
	(+ num 1))))

 ;;;unless is like a not when - only when everything is false

;;cond has an implicit progn associated as demonstated
(defun our-member1 (obj lst)
  (cond
    ((atom lst) 
     (format t "ya ")
     (format t "ba "))
    ((eql (car lst) obj)
     lst)
    (t
     (our-member obj (cdr lst)))))

(cond (99)) ;returns 99 bad technique to use this

(defun month-length (mon)
  (case mon ;key or list of keys
    ((jan mar may jul aug oct dec) 31) ;zero or more expressions ;;compared using eql
    ((apr jun sept nov) 30) ;see if the key's match the 
    (feb (if (not (null t)) 28 29))))

(case 99 (99)) ;;nil
(case 99 ((99) 1)) ; 1

 ;;;5-4 iteration
;;since do 
 ;;;did more on the laptop at home
(defun show-squares2 (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i ))))

;;if update form refers to a variable that has its own update form does it get the updated value or the value from the previous iteration
(defun show-do ()
  (let ((x 'a))
    (do ((x 1 (+ x 1)) ;(vaiable initial update)
	 (y x x)) ;;the initial value of x is 'a instead of 1 - once again - (variable initial update)
	((> x 5)) 
      (format t "(~A ~A) " x y))))
;;(1 A) (2 1) (3 2) (4 3) (5 4)

 ;;;do* works like let* - do star - dostar
(defun show-do*()
  (let ((x 'a))
    (do* ((x 1 (+ x 1))
	  (y x x)) ;;this time instead of lexical scoping it uses the previous value of x - in this case one.
	 ((> x 5))
      (format t "(~A ~A) " x y))))
;;(1 1) (2 2) (3 3) (4 4) (5 5)


;;dolist
(defun dolist-test ()
  (dolist (x '(a b c d) 'done)
    (format t "~A " x)))

;;dotimes
 ;;;the last thing it returns is the last var
 ;;; 0 to in this case 4
(defun dotimes-test()
  (dotimes (x 5 x) ;;for an n, it iterates over the integers from 0 to n-1
    (format t "~A " x )))
;;the last thing it does is return's x in the previous case



;;this should be an interesting one to figure out
;;most prog languages you have one variable that generates successive values
;;and we always use one var to accumulate a result
(defun factorial-do (n)
  (do ((j n (- j 1)) ;;fist argument (variable initial update)
       (f 1 (* f j)));;first arument (variable initial update)
      ((= j 0) f) ;second argument ((stopping condition) return-value)
    (format t "j = ~A  f = ~A~%" j f))) ;body uses the variables from the do loop 

					;in this case do has an empty body! (nothing after the final f) - and this is quite common - unlike previous example
(defun factorial-do (n)
  (do ((j n (- j 1))
       (f 1 (* j f)))
      ((= 0 j) f)))

;;mapc - does not cons up the new list as a return value - only reason to use it is to have side-effects
;;more flexible than dolist becuase it can traverse multiple lists in parallel:

(defun mapc-test ()
  (mapc #'(lambda (x y)
	    (format t "~A ~A  " x y))
	'(hip flip slip)
	'(hop flop slop)))
;;print value - HIP HOP  FLIP FLOP  SLIP SLOP 
;;return value is the second argument - '(HOP FLOP SLOP)

;;5.5
;;multiple return values
(get-decoded-time) ; returns multiple values

(values 'a nil (+ 2 4));; gives 3 return values 
					;A ;
					;NIL ;
					;6
(lambda ()
  (values 1 (+ 2 3)))


;;if a values expression is the last thing to be evaluated in the body of a function its return value
;;becomes those of the function...multiple values are passed on intact through any number of returns
(lambda ()
  ((lambda ()
     (values 1 2))))

(defun lambda-test(n m)
  ((lambda (x)
     ((lambda(y)
	(values x y)) m)) ;;value (of m) passed to the inner lambda
   n)) ;;value passed to the first lambda

					;use
(lambda-test 2 'a)

(defun lambda-test(n m)
  ((lambda (x)
     ((lambda (y)
	(values x y))
      m)) ;m is passed to the inner function
   n)) ;n is passed to x
;;also notice the order the return values come in

(let ((x (values 1 2)))
  x); => 1 (only one value gets returned)

(values) ;returns no values - not even NIL

(let ((x (values)))
  x) ;nil

 ;;;operations with multiple values - Multiple return values
;;to receive multiple values, we use multiple-value-bind
(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z)) ;even (1 2 3) can be passed
(multiple-value-bind (x y z) (values '(a b c) 2 3)
  (list x y z)) ;even lists can be passed 
(multiple-value-bind (x y z) (values 1 2)
  (list x y z)) ;(1 2 NIL)

;;more values than variables then the extra will be discarded - so to print just the time
(multiple-value-bind (s m h d m y) (get-decoded-time)
  (format t "~A:~A:~A - ~A/~A/~A" h m s m d y))

					;pass multiple values as arguments to a second function
(multiple-value-call #'+ (values 1 2 3)) ;passes 1 2 3 to the + function
(multiple-value-call #'list (values 1 2 3)) ;passes 1 2 3 to the list function
(multiple-value-call #'list (get-decoded-time)) ;passes each return value of get-decoded-time to the list function 
(multiple-value-list (values 'a 'b 'c)) ;turns them into list

;;it's the same thing
(equalp (multiple-value-list (values 'a 'b 'c)) (multiple-value-call #'list (values 'a 'b 'c)))

 ;;;5.6 Aborts
;;catch and a throw
;;catch takes a tag (which can be any kind of object), followed by a body of expressions
(defun super()
  (catch 'abort1
    (sub) ;;a throw causes a catch expression to return immediately.
    (format t "we'll never see this.")))

;;there is no catcher for the tag 'ABORT
(defun sub ()
  (throw 'abort1 99))

;;it never comes back 
(defun error-example ()
  (progn
    (error "oops!")
    (format t "After the error.")))
;;x = 1
(setf x 1)

;;unwind-protect takes any number of arugments and returns the value of the first. 
(catch 'abort 
  (unwind-protect
       (throw 'abort 99)
    (setf x 2)))
;;x = 2

(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))
(apply #'+ mon) ;365
(apply #'+ (reverse mon));365
(setf nom (reverse mon))

;;takes a list every time - change the apply statemtn to a print and see the result
(setf sums (maplist #'(lambda (x)
			(apply #'+ x))
		    nom))

(setf sums (reverse sums))


;;sidebar
(defun show-mapcar()
  (mapcar #'(lambda (x)
	      (princ x))
	  '(a b c d))); 
(show-mapcar)
					;ABCD
					;(A B C D)

;;sidebar2 - maplist always takes a list - always the CDR of the list
(defun show-maplist()
  (maplist #'(lambda (x)
	       (princ x))
	   '(a b c d)))
(show-maplist)
					;(A B C D)(B C D)(C D)(D)
					;((A B C D) (B C D) (C D) (D)) - and that is what it returns

(defun show-mapcar2()
  (mapcar #'(lambda (a)
	      (funcall #'+ a 1)) ;because it's a map CAR get it!!! it iterates over the CAR OF THE LIST
	  '(1 2 3 4)))
(show-mapcar2)
;;the function call is lookng for an arg not a list. (remember apply looks for a list)
;;(2 3 4 5) increments all the elements and puts them in a list


;;doing what we did on the above doesn't work
(defun show-maplist2()
  (maplist #'(lambda (a)
	       (apply #'+ a)); maplist always returns cds - so x is like ( (1 2 3 4) (2 3 4) (3 4) (4) )
	   '(1 2 3 4)))
;;the apply is looking for a list  - maplist provides it with successive cdrs of the list
;;(10 9 7 4)

;;okay test time
(defparameter *montest* '(1 10 100 9000))
;;say we did it without the reverse
(apply (symbol-function '+) *montest*); 9111
(apply #'+ *montest*); 9111
;;say these represet the number of days in the first 4 seasons of an ancient planet
(setf sums2 (maplist #'(lambda (x)
			 (apply #'+ x)); apply takes a LIST! - x is always a list becuase that's what maplist provides - maplist gives CDRS REMEMBER!!
		     *montest*));;(9111 9110 9100 9000)

(setf sums3 (maplist #'(lambda (x)
			 (apply #'+ x))
		     (reverse *montest*)));;(9111 111 11 1)

;;the symbol-value of the following
(print (symbol-value 'sums3))

;;we neeed the months
(push 0 sums); use that as defconstant
;;(0 31 59 90 120 151 181 212 243 273 304 334 365)

;;the first date is Janurary 1st, 2000

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365)) ;;this will be an array

(defconstant yzero 2000)

;;defintion of a leap year
(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (mod y 100))))))

;;converts the date to the number
(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (- m 1)) ;get the number of days til the start of the month
     (if (and (> m 2) (leap? y)) 1 0))) ;if it's greater than feburary on a leap year we need to account for the 

(defun year-days (y)
  (if (leap? y) 366 365))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
	(dotimes (i (- y yzero) d);(initial-value-zero number-of-times-to-do-this return-value)
	  (incf d (year-days (+ yzero i))))
	(dotimes (i (- yzero y) (- d))
	  (incf d (year-days (- yzero i)))))))

;;i starts at zero
(dotimes (i 4 (- i)) ;(dotimes initial num-of-times-to-loop return-value)
  (print i))
					;0 
					;1 
					;2 
					;3 
					;-4 ;;see how it prints the last arg the -i (and see how the couter increments at the end anyway)

;;to use
(date->num 1 1 2000)
(date->num 30 12 2012)
(date->num 1 1 2013) ;4749
(date->num 32 12 2012);4749 ;notice the bug here

;;mod function
(mod 23 5) ;3
(mod 25 5) ;0

;;test a bunch of dates to see if they're leap years using the definition - see if it's a 
(mapcar #'leap? '(1904 1900 1600 1700)) ;(T NIL T NIL)


(defun num->date (n)
  (multiple-value-bind (y left) (num-year n) ;see how in this case num-year must be returning two things
    (multiple-value-bind (m d) (num-month left y) ;; returns months and days left
      (values d m y))))

;;do* means instead of lexical scoping it uses the value of the previous iteration
;;returns the year and the number of days left into that year
(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (1- y)) ;(var initial update)
	    (d (- (year-days y)) (- d (year-days y)))) ;(var initial update)
	   ((<= d n) (values y (- n d) d n))) ;second argument for the do* macro - (stopping-condition return-value) - the return-value expression is by choice
      (do* ((y yzero (1+ y))
	    (prev 0 d); the previous value of d 
	    (d (year-days y) (+ d (year-days y))))
	   ((> d n) (values y (- n prev) d prev n)))))

;;returns the month and the day
(defun num-month (n y)
  (if (leap? y)
      (cond ((= 59 n) (values 2 29))
	    ((> n 59) (nmon (- n 1))) ;;if n > 59 ;;there was an error here check conditions
	    (t        (nmon n)))
      (nmon n)))

;;we add the 1 at the end becuase the year will usually be to jan 1 of that year. so the days will be 
(defun nmon (n)
  (let ((m (position n month :test #'<))) ;-for example (eq n 364) will give you 12 (not 11) - becuase 12 == 365 and 11 == 334 which is what we need
    (values m (+ 1 (- n (svref month (- m 1)))))))  ;
;;test it
					;(0 31 59 90 120 151 181 212 243 273 304 334 365)
(position 31 month);1
(position 30 month);NIL
(position 32 month);NIL
(position 59 month);2
(position 60 month);NIL
;;we don't want the nils this one just tests to see if they're exactly equal! we need a different test
(position 30 month :test #'<) ; 1
(position 31 month :test #'<); 2



(svref month 11); could mean the start of december -- or after november ends how many days are used

;;for example
(nmon 364) ; m = 12  - and what's left is 364 - 334 
(nmon 0)
					;(num->date 0) ;messes up
(num->date 1) ; 1 1 2000; day month year

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))


;;now it works!!!!
(num->date 0)
(num->date 1) 

(position 35 month :test #'<) ;2
(position 235 month :test #'<) ;8
(position 30 month :test #'<) ;1
(position 2 month :test #'<);1
(position 31 month :test #'<);2
(position -1 month :test #'<);0 

;;Chapter 5 exercises - CHAP 5 - chap 5
;; 5-1
					;5-1a
(defun test-ex-5-1-a()
  (let ((x (car y)))
    (cons x x)))

;;wrong - x equals the (car y)
(defun test-5-1-a-answer1()
  ((lambda (x y)
     (cons x x))
   x
   (car y)))

;;right
(defun test-5-1-a-answer2 ()
  ((lambda (x)
     (cons x x))
   (car y)))

;;5-1b
(setf w -10)
(setf z 100)
;;we want it to behave like o
;;letstar let star
(defun test-ex-5-1-b()
  (let* ((w (car (list x)))
	 (y (+ w z)))
    (cons w y))) ;(2 . 102)

(defun test-ex-5-1-bb()
  (let ((w (car (list x)))
	(y (+ w z)))
    (cons w y))) ;(2 . 90)

(defun test-ex-5-1-bbb()
  (let ((w (car (list x))))
    (let ((y (+ w z)))
      (cons w y)))) ;(2 . 102)

(defun test-5-1-b-answer()
  (lambda (w)
    (lambda (y)
      (values (cons w y))
      (+ w z))
    (values (car (list x)))))

((lambda (w)
   (lambda (y)
     (print (cons w y)))
   (+ w z))
 (car (list x)))

;;see why this does work and the other doesn't? this one ((lambda xxxxx)) the other one was just (lambda xxx)
(defun test-let*2()
  ((lambda (w)
     ((lambda (y)
	(cons w y))
      (+ w z)))
   (car (list x)))) ; (2 . 102)

;;ex 5-2 
#| block comment lols - note how this would mess up if we ever encouter the case where x is not in y |#
(defun mystery-aj (x y)
  (cond
    ((null y) nil)
    ((eq x (car y)) 0)
    (t (+ 1 (mystery x
		     (cdr y))))))
;;note the differene between my version and the correct version 
(defun mystery (x y)
  (cond
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery x (cdr y)))) ; let the value returned from this 
	 (and z (+ z 1)))))) ; (and nil 3) gives NIL!! so instead of blowing up ie in the (eq (null y) t)

					;one of the awseome things about lisp is that you can store these values - just like we did with the let statment there and we only evaluate it if 

;;ex 5-3
(defun weird-square (n)
  (cond
    ((null n) nil)
    ((integerp n)
     (cond
       ((and (> n 0) (<= n 5))
	nil)
       (t
	(* n n))))
    (t (* n n))))

;;ex 5-4 - write a def of month-num using case instead of svref
(defun month-num-stupid (m y)
  (+ (case (- m 1 )
       (0 0)
       (1 31)
       (2 59)
       (3 90)
       (4 120)
       (5 151)
       (6 181)
       (7 212)
       (8 243)
       (9 273)
       (10 304)
       (11 334)
       (12 334))
     (if (and (leap? y)
	      (> m 2))
	 1
	 0)))

;;precedes with lisp
(defun precedes (m v)
  (precedes-helper m v nil))

(defun precedes-helper (m v prev)
  (cond
    ((null v) nil)
    ((and (eq (car v) m)
	  (not (null prev)))
     (cons prev
	   (precedes-helper m (cdr v) m)))
    (t 
     (precedes-helper m (cdr v) (car v)))))

					;m is object we're looking for
					;v is the vector
					;i is the current index 
(defun precedes-helper2 (m v i)
  (cond
    ((>= i (length v)) nil)
    ((and (eq (aref v i) m)
	  (<= 0 (1- i)))
     (cons (aref v (1- i))
	   (precedes-helper2 m v (1+ i))))
    (t
     (precedes-helper2 m v (1+ i)))))

(defun precedes2 (m v)
  (precedes-helper2 m v 0))

(defun precedes-call(m v f i prev)
  (if (not (null i))
      (funcall f m v i)
      (funcall f m v prev)))
;;see they work!!!
(precedes-call 'a '(a b a c a a) #'precedes-helper nil nil)
(precedes-call #\a "abacaa" #'precedes-helper2 0 nil)
;;this works tooooo!
(defun precedes-iter (m v)
  (let ((prevs nil))
    (if (> (length v) 1)
	(do ((i 1 (1+ i)))
	    ((>= i (length v)) prevs)
	  (if (eq m (aref v i))
	      (setf prevs (append (list (aref v (1- i))) prevs)))))))
;;use
(precedes-iter #\a "abcdefauacaka") ;;strings are 1-d array of characters - aka a vector!!! awwwww shit playa

;;ex 5-6
;;recursive defintion
(defun intersperse (m n)
  (cond
    ((null n) nil)
    (t
     (cons (car n)
	   (if (> (length n) 1)
	       (cons m (intersperse m (cdr n)))
	       nil)))));; works
;;use
(intersperse '- '(b a))
(intersperse '- '(b a c d e f))

;;works as well
(defun intersperse-iter (m n)
  (let ((prevs nil))
    (do ((i 0 (1+ i)))
	((>= i (length n)) (reverse prevs))
      (if (and (> i 0)
	       (< i (1- (length n))))
	  (progn
	    (push m prevs)
	    (push (nth i n) prevs))
	  (push (nth i n) prevs)))))

;;ex 5-7

;;recursive
(defun diff-one (n)
  (if (null n)
      nil
      (diff-one-helper n nil)))

(defun diff-one-helper (n prev)
  (cond
    ((null n)
     t)
    ((or (null prev)
	 (eq 1 (- (car n) prev)))
     (diff-one-helper (cdr n) (car n)))
    (t
     nil)))
;;use
(diff-one '(1 2 3)) ;T
(diff-one '(1 3 4)) ;nil


(defun diff-one-mapc (n)
  (mapc #'(lambda (x y)
	    (if (not (eq 1 (abs (- y x))))
		(return-from diff-one-mapc nil)))
	n
	(cdr n))
  t)

(setf *te* '(1 2 3))

(mapc #'(lambda (x y)
	  (print x)
	  (print y))
      *te*
      (cdr *te*))

(diff-one-mapc '(1 2 3))
(diff-one-mapc '(1 3 4))

;;works
(defun diff-one-do (n)
  (do ((j 0 (1+ j)))  ;frist arg (var init update)
      ((>= (1+ j) (length n)) t) ;(second arg (stopping-cond return-val)
    (if (not (eq 1 (abs (- (nth j n) (nth (1+ j) n))))) ;third arg (body)
	(return-from  diff-one-do nil))))

;;make a vector for the next part
(setf *test-vec* (vector 1 2 3 4 21 5 0 -17))
;;ex 5-8 - single recursive function that reutrns max and min
(defun min-max-func (v)
  (min-max-helper v 0 (svref v 0) (svref v 0)))

(defun min-max-helper (v i mi mx)
  "retuns the min and max of a vector. takes the vector the index and the current min and max"
  (cond
    ((>= i (length v)) (values mi mx))
    (t
     (let ((curr (aref v i)))
       (if (>= mi curr)
	   (setf mi curr))
       (if (<= mx curr)
	   (setf mx curr))
       (min-max-helper v (1+ i) mi mx)))))

;;use
(min-max-func *test-vec*)

;;5-9
;;a network
(setf *a-network* '((a b c q l) (b c) (c d e) (e c x) (d c)))
(setf *could-be-any-path* (car (list (list 'could-be-any-path))))
(setf *a-node-in-the-network* 'a)

;;just starts the search
(defun short-path (st end network)
  (breadth-first end (list (list st)) network))

(defun breadth-first (end queue net)
  (cond
    ((null queue) nil)
    (t
     (let ((path (car queue)))
       (let ((node (car path)))
	 (if (eql node end)
	     (reverse path)
	     (breadth-first end
			    (append (cdr queue)
				    (new-found-paths path node net)) ;(append list list)
			    net)))))))

(defun new-found-paths (path node net)
  (mapcar #'(lambda (n) ;takes a function
	      (cons n path))
	  (cdr (assoc node net)))) ;also takes a list - (of nodes in this case)

(cdr (assoc 'a *a-network*)) ;(B C Q L)

;;test the new-found-paths
(new-found-paths *could-be-any-path*
		 *a-node-in-the-network*
		 *a-network*)

(short-path 'd 'x *a-network*)

(defun breadth-first (end queue net)
  (cond
    ((null queue) nil)
    (t
     (let* ((path (car queue))
	    (node (car path)))
       (if (eql node end)
	   (return-from breadth-first (reverse path))
	   (breadth-first end
			  (append (cdr queue)
				  (new-found-paths path node net)) ;(append list list)
			  net))))))

;;test this - test *a-second-network* with '(a b c d e f) in order and see the difference (notice that it actually does keep searching)
(setf *a-second-network* '((a f b c d e) (b x) (c x) (d x) (e f x) (f e x) (x b c d e f)))
(setf *a-third-network* '((a c) (c b d) (d e f) (f g) (g h) (h i j)))

(cdr (assoc 'a *a-second-network*)); (B C D E F)
(short-path 'a 'x *a-second-network*) ;;goes (A D X) -- we want (A B X) -- the first correct answer
;;no actually gives (A B X)
;;(trace breadth-first)
;;(trace new-paths)
					;[32]> (trace breadth-first)
;; Tracing function BREADTH-FIRST.
					;(BREADTH-FIRST)
					;[33]> (trace new-found-paths )
;; Tracing function NEW-FOUND-PATHS.
;;(NEW-FOUND-PATHS)
;;[34]> (short-path 'a 'x *a-second-network* )
;;1. Trace: (BREADTH-FIRST 'X '((A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;2. Trace: (NEW-FOUND-PATHS '(A) 'A '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;2. Trace: NEW-FOUND-PATHS ==> ((F A) (B A) (C A) (D A) (E A))
;;2. Trace: (BREADTH-FIRST 'X '((F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;3. Trace: (NEW-FOUND-PATHS '(F A) 'F '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;3. Trace: NEW-FOUND-PATHS ==> ((E F A) (X F A))
;;3. Trace: (BREADTH-FIRST 'X '((B A) (C A) (D A) (E A) (E F A) (X F A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;4. Trace: (NEW-FOUND-PATHS '(B A) 'B '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;4. Trace: NEW-FOUND-PATHS ==> ((X B A))
;;4. Trace: (BREADTH-FIRST 'X '((C A) (D A) (E A) (E F A) (X F A) (X B A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;5. Trace: (NEW-FOUND-PATHS '(C A) 'C '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;5. Trace: NEW-FOUND-PATHS ==> ((X C A))
;;5. Trace: (BREADTH-FIRST 'X '((D A) (E A) (E F A) (X F A) (X B A) (X C A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;6. Trace: (NEW-FOUND-PATHS '(D A) 'D '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;6. Trace: NEW-FOUND-PATHS ==> ((X D A))
;;6. Trace: (BREADTH-FIRST 'X '((E A) (E F A) (X F A) (X B A) (X C A) (X D A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;7. Trace: (NEW-FOUND-PATHS '(E A) 'E '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;7. Trace: NEW-FOUND-PATHS ==> ((F E A) (X E A))
;;7. Trace: (BREADTH-FIRST 'X '((E F A) (X F A) (X B A) (X C A) (X D A) (F E A) (X E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;8. Trace: (NEW-FOUND-PATHS '(E F A) 'E '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;8. Trace: NEW-FOUND-PATHS ==> ((F E F A) (X E F A))
;;8. Trace: (BREADTH-FIRST 'X '((X F A) (X B A) (X C A) (X D A) (F E A) (X E A) (F E F A) (X E F A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;8. Trace: BREADTH-FIRST ==> (A F X)
;;7. Trace: BREADTH-FIRST ==> (A F X)
;;6. Trace: BREADTH-FIRST ==> (A F X)
;;5. Trace: BREADTH-FIRST ==> (A F X)
;;4. Trace: BREADTH-FIRST ==> (A F X)
;;3. Trace: BREADTH-FIRST ==> (A F X)
;;2. Trace: BREADTH-FIRST ==> (A F X)
;;1. Trace: BREADTH-FIRST ==> (A F X)
;;(A F X)

;;from line 3 it already found (X F A) but it coundn't do anything because
;;1 it appends it to the end of the list
;;2 the function new-found-paths doesn't stop when it finds what it's looking for!

(short-path 'a 'j *a-third-network*)

;;try this
(mapcar #'(lambda (n) 
	    (cons n *could-be-any-path* )) 
	'(b c d e f))
;;result
;;((B COULD-BE-ANY-PATH) (C COULD-BE-ANY-PATH) (D COULD-BE-ANY-PATH)
;; (E COULD-BE-ANY-PATH) (F COULD-BE-ANY-PATH))


;;so what happens if we switch the order?
;;nothing good really but look at the stack trace difference 
 ;;;really infinite path  
(defun breadth-first (end queue net)
  (cond
    ((null queue) nil)
    (t
     (let* ((path (car queue))
	    (node (car path)))
       (if (eql node end)
	   (return-from breadth-first (reverse path))
	   (breadth-first end
			  (append (new-found-paths path node net) 
				  (cdr queue)) ;(append list list)
			  net))))))
;;see how this doesn't work!  a finds f - f finds e - e finds a  and the cycle repeats itself!!!
					;1. Trace: (SHORT-PATH 'A 'X '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;2. Trace: (BREADTH-FIRST 'X '((A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;3. Trace: (NEW-FOUND-PATHS '(A) 'A '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;3. Trace: NEW-FOUND-PATHS ==> ((F A) (B A) (C A) (D A) (E A))
					;3. Trace: (BREADTH-FIRST 'X '((F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;4. Trace: (NEW-FOUND-PATHS '(F A) 'F '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;4. Trace: NEW-FOUND-PATHS ==> ((E F A) (X F A))
					;4. Trace: (BREADTH-FIRST 'X '((E F A) (X F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;5. Trace: (NEW-FOUND-PATHS '(E F A) 'E '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;5. Trace: NEW-FOUND-PATHS ==> ((F E F A) (X E F A))
					;5. Trace: (BREADTH-FIRST 'X '((F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;6. Trace: (NEW-FOUND-PATHS '(F E F A) 'F '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;6. Trace: NEW-FOUND-PATHS ==> ((E F E F A) (X F E F A))
					;6. Trace: (BREADTH-FIRST 'X '((E F E F A) (X F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
;;7. Trace: (NEW-FOUND-PATHS '(E F E F A) 'E '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;7. Trace: NEW-FOUND-PATHS ==> ((F E F E F A) (X E F E F A))
					;7. Trace: (BREADTH-FIRST 'X '((F E F E F A) (X E F E F A) (X F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;8. Trace: (NEW-FOUND-PATHS '(F E F E F A) 'F '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;8. Trace: NEW-FOUND-PATHS ==> ((E F E F E F A) (X F E F E F A))
					;8. Trace: (BREADTH-FIRST 'X '((E F E F E F A) (X F E F E F A) (X E F E F A) (X F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;9. Trace: (NEW-FOUND-PATHS '(E F E F E F A) 'E '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;9. Trace: NEW-FOUND-PATHS ==> ((F E F E F E F A) (X E F E F E F A))
					;9. Trace: (BREADTH-FIRST 'X '((F E F E F E F A) (X E F E F E F A) (X F E F E F A) (X E F E F A) (X F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A)) '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;10. Trace: (NEW-FOUND-PATHS '(F E F E F E F A) 'F '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;10. Trace: NEW-FOUND-PATHS ==> ((E F E F E F E F A) (X F E F E F E F A))
					;10. Trace: 
					;(BREADTH-FIRST 'X '((E F E F E F E F A) (X F E F E F E F A) (X E F E F E F A) (X F E F E F A) (X E F E F A) (X F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A))
					; '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;11. Trace: (NEW-FOUND-PATHS '(E F E F E F E F A) 'E '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;11. Trace: NEW-FOUND-PATHS ==> ((F E F E F E F E F A) (X E F E F E F E F A))
					;11. Trace: 
					;(BREADTH-FIRST 'X '((F E F E F E F E F A) (X E F E F E F E F A) (X F E F E F E F A) (X E F E F E F A) (X F E F E F A) (X E F E F A) (X F E F A) (X E F A) (X F A) (B A) (C A) (D A) (E A))
					; '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;12. Trace: (NEW-FOUND-PATHS '(F E F E F E F E F A) 'F '((A F B C D E) (B X) (C X) (D X) (E F X) (F E X)))
					;12. Trace: NEW-FOUND-PATHS ==> ((E F E F E F E F E F A) (X F E F E F E F E F A))

					;back to normal
(defun breadth-first (end queue net)
  (cond
    ((null queue) nil)
    (t
     (let* ((path (car queue))
	    (node (car path)))
       (if (eql node end)
	   (reverse path)
	   (breadth-first end
			  (progn 
			    (let ((new-queue (new-found-paths path node net end)))
			      (if (eq end (caar new-queue))
				  (return-from breadth-first (reverse new-queue)) ;;see how this is so much more complicated than using a catch throw
				  (append (cdr queue)
					  new-queue))))
			  net))))))

(defun new-found-paths (path node network end-node)
  (mapcar #'(lambda (n)
	      (if (eql n end-node)
		  (return-from new-found-paths (list (cons n path)))
		  (cons n path)))
	  (cdr (assoc node network))))
 ;;;using the trace command  
					;to debug untrace these and re-load
					;(trace new-found-paths)
					;(trace breadth-first)
					;(trace short-path)
;;now test it again and see that it's correct
(print "now running this command -> (short-path 'a 'x *a-second-network*)")
(short-path 'a 'x *a-second-network*)


 ;;;;chapter 6 - chap 6 - chap6 - Functions
 ;;;Global functions

;;checks to see if there is a function with a given symbol name
(fboundp '+) ; T
(fboundp 'somefakename) ; NIL

;;returns the actual function 
;;if a symbol is the name of a function symbol-function will return is
(symbol-function '+)
(symbol-function 'new-found-paths)

;;sets the symbol-function of some name to a function
(setf (symbol-function 'add2) ;the 
      #'(lambda (x) (+ x 2)))

;;it's the same as if we'd defined a new global function with defun
(add2 1) ; 3
(symbol-function 'add2) ;;returns the lambda expression 
;; returns the function
(funcall (symbol-function '+) 1 1) ;2
(funcall (symbol-function 'add2) 1); 3
(funcall #'add2 1); 3
;;see how sharp-quote # does the same thing
(funcall #'+ 1 1);2
(funcall (function +) 1 2 3) ;6 ;see how they do the same thing
					;well this works too
(funcall '+ 1 1); 2

;;just for fun with apply
(apply (symbol-function '+)
       '(1 2 3));6

;;by making the first arg to defun a list of the form (setf f) you define what happens when the first arg
;;to setf is a call to f*.
(defun primo (lst)
  (car lst))

;;undefined function (SETF PRIMO)
(defun (setf primo) (val lst)
  (setf (car lst) val))

;;if it doesn't work then 
(let ((x (list 'a 'b 'c)))
  (setf (primo x) 480)
  x) ;(480 b c)

;;documentation string
(defun foo (x)
  "implements an enhanced paradigm of diversity."
  x)

(documentation 'foo 'function)

;;6.2 Local Functions
;;labels define local functions
(labels ((add10 (x) (+ x 10))
	 (consa (x) (cons 'a x)))
  (consa (add10 3))) ;(A . 13)

;;not like let in the sense they can refer to any function defined there - including themselves - recursion!!
(labels ((len (lst)
	   (if (null lst)
	       0
	       (+ (len (cdr lst)) 2))))
  (len '(a b c))) ;call the function defined earlier


					;(do ((x a (b x))
					;     (y c (d y)))
					;    ((test x y) (z x y))
					;  (f x y))

;;dos can be thought of as recursive functions 
					;(labels ((rec (x y)
					;           (cond ((test x y)
					;                  (z x y))
					;                 (t
					;                   (f x y)
					;                   (rec (b x) (d y))))))
					;  (rec a c))

 ;;;6.3 Parameter Lists - rest parameters
;;inserting a &rest token BEFORE the var in the param list of a function - the &rest will be set to a list of all remaining args
;; i love you <3 <3 <3 8=============o'''''''' JIZZ

(defun our-funcall (fn &rest args) ;this variable will be set to a list of all the remaining arguments
  (apply fn args))

;;just to see what's going on
(defun our-funcall-see (fn &rest args)
  (format t "fn = ~A ~%args = ~A ~%" fn args)) 

;;just a test - 1 2 3 get passed as a list to the apply function!!! cool huh - apply takes a list &rest makes it a list
(our-funcall '+ 1 2 3) ;fn == + and args = (1 2 3)

;;optional params - params that can be omitted, and would default to certain values
;;if &optional - all things after optional are defaulted to nil if not supplied

(defun philosoph (thing &optional property)
  (list thing 'is property))

(philosoph 'death) ; (DEATH IS NIL)
(philosoph 'death 'bunnies); (DEATH IS BUNNIES)

;;optinal param value - they don't need to be constants, if they aren't constatns then they get evaluated
;;each time a default is needed
(defun philosoph (thing &optional (property 'fun))
  (list thing 'is property))

(philosoph 'death); (DEATH IS FUN)

;;a keyword param is a more flexible kind of optional param
;;like a keylist
(defun keylist (a &key x y z)
  (list a x y z))
;;use
(keylist 1 :y 2) ;(1 nil 2 nil)
(keylist 1 :y 3 :x 2); (1 2 3 nil)

;;keywords and their associated args can be collected in rest parameters and passed on to other functions
;;that are expecting them (like apply - which expects a list)
(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args) ;;remember the last argument to apply must be a list! - the first can be whatever they want
      lst
      (cons obj lst)))

;;use of our-adjoin - notice how the &rest takes care of the keyword params
(our-adjoin 'a '((c b d)) :test #'eql :key #'car)
;;adjoin takes the same keyword args as member so we can just collect them in a 
;;rest arg and pass them along to member

;;use apply - can take as many args as it wants but must end with a list! 
(apply #'max 3 5 '(2 7 3)) ;;see earlier examples - only last arg must be a list

;;complex example
(destructuring-bind ((&key w x) &rest y) '((:w 3) a b)
  (list w x y)) ;;(3 nil (A B))

;; or to get the x key value
(destructuring-bind ((&key w x) &rest y) '((:w 3 :x 10) a b c d e f)
  (list w x y)) ;;(3 10 (A B C))

;;;6.4 UTILITIES - COPY these over for every project at least
;;utility functions - util - util functions
;;T if the list has one element
(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

;;append to the end of a list
(defun append1 (lst obj)
  (append lst (list obj)))

;;apply a function to x numbers and store the result
(defun map-int (fn n)
  (let ((acc nil))               ;create an accumulator, initially nil 
    (dotimes (i n)
      (push (funcall fn i) acc)) ; push successive objects onto it - that is the lisp way
    (nreverse acc))) 

;;takes a function and a list and returns all the non-nil values returned by the fucntion
;;as it is applied to the elements of the list
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))));ovviously why would you push T on to the list g
    (nreverse acc)))

(defun filter2 (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push x acc))))
    (nreverse acc)))

;;gives element of a list with hightest score according to some sorting function
					;(defun most (fn lst)
					;  (if (null lst)
					;      (values nil nil)
					;      (let* ((wins (car lst))
					;             (maxval (funcall fn wins)))
					;        (dolist (obj (cdr lst))
					;          (let ((score (funcall fn obj)))
					;            (when (> score maxval)
					;              (setf wins obj
					;                    maxval score))))
					;        (values wins maxval))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst)) ;first thing in the list
	     (max (funcall fn wins))) ;;the amount if you call a function on it
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max) ;as a modification we could make this sort happen
	      (setf wins obj
		    max score))))
	(values wins max))))

					;use
(single? '(nil nil)) ;nil!
(single? '(nil)) ;T 
(single? 'a) ;nil 
(single? '(a))

(append '(b c d) 'a);  '(B C D . A)
(append1 '(b c d) 'a); '(B C D A)

;;identity returns the same thing
(map-int #'identity 10)
;;or you can trick map-int -to give ;; i love you <3 <3 <3 8=============o'''''''' JIZZyou random numbers
(map-int #'(lambda (x) (random 100))
	 10)

;;show me even numbers and add 10 to each of them
;;filter lets you pick random numbers and then do something to it
(filter #'(lambda (x)
	    (and (evenp x) (+ 10 x))) ;;trick to do something to a number
	'(1 2 3 4 5 6 7 8 9))

(filter #'(lambda (x)
	    (and (oddp x) (+ 10 x))) ;;trick to do something to a number
	'(1 2 3 4 5 6 7 8 9))
;;lets combine tricks
;;doesn't do as expected
(filter #'(lambda (x)
	    (and (evenp x) (random 100))) ;makes sure x is even and prints a ranodom number - totally off from what you'd want to do
	(map-int #'identity 10)) ;gives filter 10 numbers

;;this will print even random numbers
(filter #'(lambda (x)
	    (and (evenp x) x))
	(map-int #'(lambda (x)
		     (random 100))
		 10))

(most #'length '((a b) (a b c) (a b c d 1 23) (a))) ;(a b c d 1 23) 6


 ;;;6.5 closures

;;returns a function
(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

;;test combiner
(funcall (combiner 1) 2 3) ;5
(funcall (combiner '(1 2 3)) '(a) '(1 2 3)) ;(A 1 2 3)
(apply (combiner '(a b)) '((a b) (c d))) ;(A B C D)

;;a general combination function
(defun combine (&rest args)
  (apply (combiner (car args))
	 args))

(combine 2 3); 5
(combine '(a b) '(c d)); '(A B C D)

;;check out lexical scoping - lex vars are only valid within context where they are defined
;; BUT they will continue to valid for as long as something is using the context

;;called a closure
(setf fn (let ((i 3))
	   #'(lambda (x) (+ x i)))) ;;see function defined within the scope of a lexical var

(funcall fn 2) ; returns 5 - but is available outside of the function 

;;closure is a combo of a function and an environmnet - like when a function refers to something
;;from the surroinding lexical environmnet
(defun add-to-list (num lst)
  (mapcar #'(lambda (x)
	      (+ x num)) ;the var num within the lambda expression is free, so in cases like this we're passing a closure to mapcar
	  lst))

 ;;;see how 3 comes from the outer lexical environmnet - it's the num
(add-to-list 3 '(10 20 30 40)) ;(13 23 33 43)


;;how about a function that returns a different closure each time it was called
(defun make-adder (n)
  #'(lambda (x)
      (+ x n)))

;;so the function made here adds 3
(setf add3 (make-adder 3))
(funcall add3 2) ; 5

(setf add27 (make-adder 27))
(funcall add27 6) ;33
(apply  add27 '(6)); 33

;;we can even make several closusers share the same var - like a safe global variable counter
(let ((counter 0))
  (defun reset ()
    (setf counter 0))
  (defun stamp ()
    (setf counter (1+ counter))))

;;see how in this example counter is shared between the 2 closures 
;;like a protected gloabal variable
(list (stamp) (stamp) (reset) (stamp) (stamp) (stamp) (stamp)) ; (1 2 0 1 2 3 4)
(stamp); 5
(reset) ; 0 
(stamp); 1

;;takes a predicate and returns the opposite predicate
(mapcar (complement #'oddp)
	'(1 2 3 4 5 6)); (NIL T NIL T NIL T)

(mapcar #'oddp '(1 2 3 4 5 6)); (T NIL T NIL T NIL)

;;use closures to write our own complement function :)
;;returns a function that takes args - but also takes a function and applys the not to it.  
(defun our-complement (f)
  #'(lambda (&rest args) ;;means it can take multiple args
      (not (apply f args)))) ;f gets made - apply gets called on the rest of the args

(funcall (our-complement #'oddp) 2) ;;means that the lambda function that gets made can take multiple args - nope oddp only takes like 1 arg
;;lets say we had a function that took like three arguments. we could still use 3 becuase of the rest args

;;our-complement takes f as the lexical variable that wouuld otherwise be unknown
;;the reason it kept confusing me was because a funciton f was manipulated before accepting the args to it
;;we had to mapcar everything - since if we just used an apply then it's like one function taking multiple
;;values

					;2 step
;;complent a function
;;call that function on some args
(mapcar (our-complement #'oddp)  '(1 2 3 4 5))
(apply (our-complement #'+) '(1 2 3 4 5)) ;nil - see how all these are args to + and since i'm doing (not (+ 1 2 3 4 5)) - nil
(funcall (our-complement #'+) 1 2 3 4 5); nil - same as above
(funcall (complement #'+) 1 2); nil -same as above - a function that 

(funcall (our-complement #'evenp) 2); should be nil
(funcall (our-complement #'evenp) 1); shouuld be T
(funcall #'oddp 1); T

;;closures can pass some secret var/function to the variable
;;use a closure to sum all vars in a list

;;(mapcar #'car '((1)(2)(3))); (1 2 3)
;;- doesn't do as expected;;(mapcar #'+ (mapcar #'car '((1)(2)(3))));  -- '(1 2 3) obviously
;;does what you thinks
(apply (function +) (mapcar #'car '((1) (2) (3)))); 6
(function +) ;-> the acutal function behind +

;;another function builder
(defun our-mapcarcar (fn)
  #'(lambda (lst-of-lsts)
      (apply fn (mapcar #'car lst-of-lsts))))

(setf my-sums+ (our-mapcarcar #'+)); returns a function
(setf my-minus- (our-mapcarcar #'-)); returns a function
(setf my-times* (our-mapcarcar #'*)); returns a function

(funcall my-sums+ '((1) (2) (3))); 6
(funcall my-minus- '((1) (2) (3))); -4
(funcall my-times* '((3) (2) (3))); 18

 ;;;6.6 function builders

;;takes one or more functions and returns a new function in which all of them are applied in succession
;;http://stackoverflow.com/questions/5928106/compose-example-in-paul-grahams-ansi-common-lisp
;;destructuring-bind takes two lists and the second list is what is matched (ie binded) to the first list
;;the first list is a tree structure and the . means rest like in the examle below.
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns) ;like saying  fn1 = #'sqrt and  rest = '(#'round #'list) ; see how bind takes two parse trees and not just a prase tree and a list. (reverse fns) not just a static list like '(1 2 3 4 5)
    #'(lambda (&rest args) ;args are the values given to the resulting lambda
	(reduce #'(lambda (v f) (funcall f v))        ;values and functions (from the list) - initial value will be (apply #'sqrt 4)
		rest                                  ;this will be the list of other functions 
		:initial-value (apply fn1 args)))))
;;reduce takes a function and a list
;;the function is the lambda one and the function must take 2 args
					;and the list in this case is a list of functions!! - pretty clever

(defun my-compose2 (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (apply f (list v)))
		rest
		:initial-value (apply fn1 args)))))
(mapcar (my-compose2 #'round #'sqrt)
	'(4 9 16 25))
(mapcar (my-compose2 #'list #'round #'sqrt)
	'(4 9 16 25)); the problem with this is that we can still never have multiple arguments for the rest of the functions

(mapcar (compose #'list #'round #'sqrt)
	'(4 9 16 25)) ; ((2) (3) (4) (5))
(mapcar (compose #'round #'sqrt)
	'(4 9 16 25)); (2 3 4 5)
(mapcar (compose #'1+ #'sqrt)
	'(4 9 16 25)); (3.0 4.0 5.0 6.0); sqrt is done first

(funcall (compose #'identity #'sqrt) 4); 2

;;for the example below
;;first value v, will be (apply #'sqrt (4)) - :initial-value2
;;second value will be (funcall round v)
;;third will be (funcall #'list (funcall round v)
;;will v be (apply #'sqrt 4)

;;remember append TAKES A LIST
(reduce #'append '((1) (2) (3) (4)) :initial-value '(i n i t)) ; (I N I T 1 2 3 4)
(reduce #'append '(1) :initial-value '(i n i t)) ; (I N I T . 1)
(reduce #'append '((1 2 3 4 5) (a b c) (z1 z2 z3)) :initial-value '(i n i t)); (I N I T 1 2 3 4 5 A B C Z1 Z2 Z3)
(reduce #'append '((1 2 3 4 5 a b c z1 z2 z3)) :initial-value '(i n i t)); same as above
(reduce #'- '(1 2 3 4)); -8
(- (- (- 1 2) 3) 4); -8
(reduce #'cons '(1 2 3 4));   (((1 . 2) . 3) . 4) - same thing
;;here is what reduce is doing
(cons (cons (cons 1 2) 3) 4); (((1 . 2) . 3) . 4) - same thing!

					;see as long as tehre are at least 2 elemntes this will work
(reduce #'(lambda (x y) (and x y)) '(t t t nil)); NIL
(reduce #'(lambda (x y) (and x y)) '(t t t t t)); T 
;; (and (and (and t t) t) f); something like that is happening

(reduce #'cons '(1 2 3 4) :initial-value 'b); ((((B . 1) . 2) . 3) . 4)  ; 
(reduce #'cons '(1 2 3 4) :initial-value 'b :from-end t) ;(1 2 3 4 . B)
(reduce #'cons '(1 2 3 4) :initial-value nil :from-end t); (1 2 3 4)
;;reduce takes a function and a list - say a function f takes 2 args normally - (reduce #'f '(1 2 3 4 )) --- (f (f (f 1 2) 3) 4)
(reduce #'(lambda (x &rest y) (* x x)) '(4));16 
(reduce #'(lambda (x &rest y) (* x (car y))) '(2 3 4));16
(* (* 2 (car '(3 4))) (car '(4))); i think
(reduce #'(lambda (x &rest y) (* x (car y))) '(2 3 4 5 6 7 8 9 10 11))

;;see how a function that takes a couple of args
;;reduce keeps calling itself and passing the result as the first arg into itself
(reduce #'(lambda (x &rest y) (* x x)) '(2 5000 6000)); 16
(reduce #'(lambda (x &rest y) (* x x)) '(2 50 61)); 16
(reduce #'(lambda (x &rest y) (* x x)) '(2 10 0)); 16
(reduce #'(lambda (x &rest y) (* x x)) '(2 5 10)); 16
(reduce #'(lambda (x &rest y) (+ x 1)) '(1 5 10));3 ;   (f (f 1 5) 10); since it only takes the first arg
(reduce #'(lambda (x &rest y) (+ x (car y))) '(1 5 10)); 16
(reduce #'(lambda (x &rest y) (+ x (car y))) '(1 2 3 4 5 6 7 8 9)); 45


;;function builders below
;;usage
(mapcar (compose #'list #'round #'sqrt)
	'(4 9 16 25 26)) ; ((2) (3) (4) (5) (5))
;;or try with 3 other functions
(mapcar (compose #'sqrt #'round #'(lambda (x) (* x x)))
	'(4 9 16 25 26.1));(4 9 16 25 26.095976)

;;see how the . z works - it's the rest of the items in list form
(destructuring-bind (w (x y) . z) '(a (b c) d e lol blah)
  (list w x y z)); (A B C (D E LOL BLAH))
(destructuring-bind (w (x y) . z) '(a (b c) d e lol blah)
  (list y )); '(C)
(compose #'list #'round #'sqrt) ;;returns a function 

;;disjoin builds a function - use it for getting the union of sets - with mapcar - see example
;;also note the use of a closure here - the created function uses both 
(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns))) ;see how it reduces? cool way to iterate - does the iteration happen here - disjoin always returns a function! it must
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args)))))) ;or does it happen here - no makes sense for it to happen further up

(defun disjoin2 (fn &rest fns)
  (if (null fns)
      fn
      #'(lambda (&rest args)
	  (or (apply fn args) (apply (apply #'disjoin2 fns) args))))) ; see how in the middle (apply #'disjoin2 fns) we didn't have to do (apply #'disjoin2 (cdr fns))

;;uses - see how they both do the same thing - recursion happens 
(mapcar (disjoin #'integerp #'symbolp)
	'(a "a" 2 3 lol 1.1)); (T NIL T T T NIL)
(mapcar (disjoin2 #'integerp #'symbolp)
	'(a "a" 2 3 lol 1.1 2.3)); (T NIL T T T NIL NIL)
(mapcar (disjoin2 #'integerp)
	'(a "a" 2 3 lol 1.1 2.3)); (NIL NIL T T NIL NIL NIL)
(mapcar (disjoin2 #'integerp #'symbolp #'stringp)
	'(a "a" 2 3 lol 1.1 2.3)); (T T T T T NIL NIL)
;;like an intersection -- AND
(defun conjoin2 (fn &rest fns)
  (if (null fns)
      fn
      #'(lambda (&rest args)
	  (and (apply fn args) (apply (apply #'conjoin2 fns) args)))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj  (apply #'conjoin fns))) ;way to iterate
	#'(lambda (&rest args)
	    (and (apply fn args) (apply conj args))))))

;;use - it's like an intersection of sets
(mapcar (conjoin #'integerp #'oddp #'(lambda (x) t) #'(lambda (x) t))
	'(1 2 3 a b d "3")); (T NIL T NIL NIL NIL NIL)
(mapcar (conjoin #'integerp #'oddp #'(lambda (x) t) #'(lambda (x) nil))
	'(1 2 3 a b d "3")); (NIL NIL NIL NIL NIL NIL NIL)
(funcall (conjoin #'integerp #'oddp #'(lambda (x) t) #'(lambda (x) nil))
	 3); nil
(funcall (conjoin #'integerp #'oddp #'(lambda (x) t) #'(lambda (x) t))
	 3); T

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(funcall (curry #'+ 3) 2); 5
(funcall (curry #'- 3) 2); 1
(funcall (curry #'- 3) 2 1 1); -1
(funcall (curry #'- 3 1 2) 2); -2
(append '(3 1 2) '(2)); (3 1 2 2) and then you apply the 

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))));

(funcall (rcurry #'+ 3) 2); 5
(funcall (rcurry #'- 3) 2); -1 ;compare to the (funcall (curry #'- 3) 2); -1
(funcall (rcurry #'- 3 1 2) 2); -4
(append '(2) '(3 1 2)); (2 3 1 2)


;;no matter what the arguments to it, it always returns x
(defun always (x)
  #'(lambda (&rest args)
      x))
;;use
(funcall (always 3) 1 2 32 ); see how it's always 3

;;6.7 lexical and Dynamic Scope
;;lexical scoping 
(let ((x 10)) ;;special variable with dynamic scope
  (defun foo ()
    x));;see how x = 10

(let ((x 20))
  (foo));10 ;see how x still equals 10 here 

;;dynamic scope we look for a var in the environment where the function is called not where it is defined
;;we need to delcare it as special if we do that.
;;dynmaic is defaut if you use setf - confirm with teh stackoverflow question
(let ((x 10))
  (defun foo ()
    (declare (special x)) ;;x deosn't refer to lexical var existing at the time func was defined but to
    x));;whatever special x exists at the time the func is called

(let ((x 15))
  (declare (special x))
  (foo));15 - see how it's speical to where it's called and not where it was defined with this

;;also gobal vars established by calling setf at a toplevel are implicitly special
(setf x 30)
(foo); 30

;;
;;*print-base* - is a global var with default value of 10 - ie base 10

(let ((*print-base* 16))
  (princ 32)); a and back to 32
(let ((*print-base* 16))
  (princ 10))

;;6.8 - Compilation

(defun foo (x)
  (+ x 1))
(compiled-function-p #'foo)
(compiled-function-p #'+) ; T or NIL right
(compile 'foo)

;;6.9 Using Recursion
;;mine
;;find the inductive step
;;find the base case
;;pg
;;1.) show how to solve the problem in the general case (induction) by breaking it down into a finite num
;;of similar, but smaller, problems. ie if it works for one it should work for all.
;;2.) you have to show how to solve the smallest version of the problem - the base case - by some finite
;;num of operations.
;;- a finite problem will get solved eventually, each recusion makes it smaller and the 
;;smallest problem takes a finite number of steps.
;;
;;make sure the way you break up the problem it does in fact lead to a smaller 
;;
;;
;;Exercises chapter 6
;;6-1 version of tokens that takes :test and :start as constituent and starting at 0
(defun tokens2 (str &optional (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str
			       :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens2 str test p2)
		    nil)))
	nil)))

;;constituent is true if it's a graphical character (unlike /n which is new line or a " " space) 

(tokens2 "ab12 3cde.f
	 gh"); see how default is constituent and 0 
(tokens2 "ab12 3cde.f
	 gh"
	 #'constituent
	 5)

;;see how this shit works too! 
(defun tokens3 (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str
			       :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens3 str :test #'constituent :start p2)
		    nil)))
	nil)))
(tokens3 "ab12 3cde.f
	 gh"
	 :test #'constituent
	 :start 5); ("3cde.f" "gh")
(tokens3 "ab12 3cde.f
	 gh"); ("ab12" "3cde.f" "gh")

;;mess with the start value and see how it tokenizes different parts of the string. 


;;6-2 - add start end  key test
(defun bin-search2 (obj vec &key (key #'identity) (start 0) (end nil) (test #'eql))
  (let ((len (length vec)))
    ;; if a real vector, send it to finder
    (if  (and (not (zerop len))
	      (null end))
	 (finder2 obj vec start (1- len) key test)
	 (finder2 obj vec start end key test)))); means if end has a value call this function instead

;;test effects how we're actually comparing things - like if we use eql or equalp - like when we're comparing two different objects 
;;key effects the acutal object we're comparing - before we compare we want to run a function on it so that the test (eql in this case) works on it
;;  for example 
(eql '(a) '(a)); NIL
(eql 'a 'a); T
(eql (car '(a))
     (car '(a))); T (the car is the key in this case and eql is the tester
;;try with mapcar
(mapcar #'(lambda (x &rest y)
	    (print x))
	'((c d) a b))
;;try again
(mapcar #'(lambda (x &rest y)
	    (print x))
	'((c d) (a b) (a a)))
;;try to manipulate
(mapcar #'(lambda (x &rest y)
	    (eql (car x)
		 (cadr x)))
	'((c d) (a b) (a a) (2 1) (2 2)))


					;like in this scenario it would be nice to have a car of x 
(mapcar #'(lambda (x &rest y)
	    (eql (car x)
		 (cadr x)))
	'(((c d)) ((a b)) ((a a))))

;;now it works!
(mapcar #'(lambda (x &rest y)
	    (eql (caar x)
		 (cadar x)))
	'(((c d)) ((a b)) ((a a))))

(defun finder2 (obj vec start end key test)
  (format t "~A~%" (subseq vec start (1+ end)))
  (let ((range (- end start)))
    (format t "~A~%~%" range)
    (if  (or (zerop range) (< range 0)) ;;modified finder to make it work - no more errors
	 (if (funcall test obj  
		      (funcall key (aref vec start))) ;modified - see how we use test in place of eql and key (in this case the identity function) once we've selected the actual object
	     obj
	     nil)
	 (let ((mid (+ start (round (/ range 2)))))
	   (let ((obj2  (funcall key (aref vec mid)))) ;modified object 2 should also be affected by key
	     (if (< obj obj2)
		 (finder2 obj vec start (1- mid) key test)
		 (if (> obj obj2)
		     (finder2 obj vec (1+ mid) end key test)
		     obj))))))); if it's not less than and not > it must be the object!
;;now try to use it
(bin-search2 3 #(0 1 2 3 4 5 6 7 8 9) :start 4 )
(bin-search2 3 #(0 1 2 3 4 5 6 7 8 9) :start 1 :end 2)
(bin-search2 3 #(0 1 2 3 4 5 6 7 8 9) :start 1 :end 2 :key #'(lambda (x) x) )
(bin-search2 3 #(0 1 2 3 4 5 6 7 8 9) :start 1 )

;;6-3 - make something that counts the number of args
(defun my-arg-counter (&rest args)
  (length args))

;;6-4 - make it so that most modifies 2 values - 2 highest scoring elements of  a list 
(defun most2 (fn lst)
  (if (null lst)
      (values nil nil nil nil)
      (let* ((wins (car lst)) ;first thing in the list
	     (max (funcall fn wins)); the first thing in the list
	     (max2 max) ;the old max value to max2
	     (wins2 wins))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf wins2 wins
		    max2 max
		    wins obj
		    max score))))
	(values wins max wins2 max2))))

;;nice try but wrong
(defun most3 (fn lst)
  (values (most2 fn lst)
	  (most2 fn (cadr (remove (most2 fn lst)
				  lst)))))
;;try again from below
;;(defun most4 (fn lst)
;;  (values (most fn lst)
;;          (most fn 
;;                (remove (most fn lst)))))

;;(defun most5 (fn lst)
;;  (progn
;;    (setf w1 m1 ( most fn lst))
;;   (values w1 m1 (most fn
;;                        (remove w1 lst)))))

(defun most6 (fn lst)
  (let (( w1 (most fn lst)))
    (values (most fn lst)
	    (most fn
		  (remove w1 lst)))))
;;(defun most7 (fn lst)
;;  (let (( w1 m1 (most fn lst)))
;;    (values w1 m1
;;           (most fn
;;                (remove w1 lst)))))
;;way to do it using remove-if
;;use
(most2 #'length '((a b) (a b c) (a b c d 1 23) (a))) ;(A B C D 1 23) 6 (A B C) 3
(most2 #'length '((a b) (b)))
(most3 #'length '((a b) (b)))
;;rth try
					;(most4 #'length '((a b) (b)))
;;6th try
					;(most5 #'length '((a b) (b)))
;;7th try
(most6 #'length '((a b) (b))); (a b) (b) ;;this works
(most6 #'length '((a b) (a b c) (a b c d 1 23) (a)))
(most #'length '((a b) (a b c) (a b c d 1 23) (a)));(A B C D 1 23) (A B C)
;;(most7 #'length '((a b) (a b c) (a b c d 1 23) (a)))

(defvar basic-lst '((a b) (b)))
(remove (most2 #'length basic-lst)
	basic-lst)
(most2 #'length (remove (most #'length basic-lst)
			basic-lst))


(values (most #'length basic-lst))

;;mess with filter as well

;;another attempt if we just are able to set the initial values
(defun most8 (fn lst)
  (cond
    ((null lst)
     (values nil nil nil nil))
    ((null (cdr lst))
     (values (car lst) (funcall fn (car lst)) nil nil))
    (t
     (let* ((wins (car lst)) ;first thing in the list
	    (max (funcall fn wins)); the first thing in the list
	    (max2 (funcall fn (cadr lst))) ;the second value of the max in the list
	    (wins2 (cadr lst))) ;;the second thing in the list
       (dolist (obj (cdr lst))
	 (let ((score (funcall fn obj)))
	   (when (> score max)
	     (setf wins2 wins
		   max2 max
		   wins obj
		   max score))))
       (values wins max wins2 max2)))))

(most8 #'length '((a b) (b)));;works

;;most6 was done with filters how about 


;;6.5remove-if (no keywords) in terms of filter
(defun my-remove-if (f lst)
  (filter (complement f) lst))

(defun my-remove-if-2 (f lst)
  (filter #'(lambda (x)
	      (and (funcall (complement f) x) x)) 
	  lst))
(defun my-remove-if-3 (f lst)
  (filter2 (complement f) lst))

(filter #'(lambda (x)
	    (evenp x)) ;;trick to do something to a number
	'(1 2 3 4 5 6 7 8 9))


(filter #'evenp '(1 2 3 4 5 6 7 8 9))
(remove-if #'evenp '(1 2 3 4 5 6 7 8 9))
(my-remove-if #'evenp '(1 2 3 4 5 6 7 8 9))
(my-remove-if-2 #'evenp '(1 2 3 4 5 6 7 8 9))
(my-remove-if-3 #'evenp '(1 2 3 4 5 6 7 8 9))

(filter #'(lambda (x)
	    (and (evenp x) x)) ;;trick to do something to a number
	'(1 2 3 4 5 6 7 8 9))

(my-remove-if-2 #'evenp '(1 2 3 4 5 6 7 8 9))

;;6.6 function that takes one arg, a num, an returns the greatest arg passed to it so far
;;see this returns a function
(defun mx (n)
  #'(lambda (x)
      (if (> x n)
	  (setf n x)
	  n)))

;;version 2
;;this way also works
(let (mx2)
  (defun max-so-far (x)
    (if (or (not mx2) (> x mx2))
	(setf mx2 x)
	mx2)))

;;version 3
(let (mx3)
  (defun max-so-far3 (x)
    (if (or (not mx3) (> x mx3))
	(setf mx3 x)
	mx3)))

;;use
(setf my-mx (mx 0));makes a function
(funcall my-mx 2)
(funcall my-mx 1)
(funcall my-mx 22)
(funcall my-mx 12)

;;use version 2
(max-so-far 2)
(max-so-far 1)
(max-so-far 13)
(funcall #'max-so-far 12)


;;6.7 function that takes one arg a num and returns true if is greater than the arg passed to the func last time it was called
(defun largest-arg?(n)
  #'(lambda (x)
      (if (and (not n) (> x n))
	  (progn 
	    (setf n x)
	    t))))
;;wrong
(defun largest-arg2? (n)
  #'(lambda (x)
      (progn
	(and n (< x n))
	(setf n x))))

(defun largest-arg3? (n)
  #'(lambda (x)
      (cond ((null n)
	     (setf n x)
	     nil)
	    ((> x n)
	     (setf n x)
	     t)
	    (t nil))))
;;use
(setf lar-arg? (largest-arg3? nil)) ;(setf lar-arg? (largest-arg3? 2))
(funcall lar-arg? 1)
(funcall lar-arg? 10)
(funcall lar-arg? 8)

;;6.7 a function -from the net that works
(let (prev)
  (defun greater-p (n)
    (prog1
	(and prev (< prev n))
      (setf prev n))))
(greater-p 1)
(greater-p 2)
(greater-p 2)

;;6.8 - expensive is a fn of one arg between 0 and 100. returns the result of a time-consuming computation
;;define a fn frugal that rets the same answer, but ony calls expensive when given an arg it hasn't seen before
(let (seen-arg)
  (defun frugal (arg)
    (cond
      ((find arg seen-arg)
       (format t "same answer ~A" arg))
      (t
       (print "call expensive function")
       (push arg seen-arg)))))

(frugal 3)
(frugal 4)
(frugal 3)
					;(frugal 19)

;;6.9 define a function like apply where any num printed out before it returns will be printed by default
;;in octal (base 8)

(let ((*print-base* 8))
  (princ 8)) ;; returns 2 things the output generated by princ and 8

(let ((*print-base* 8))
  (defun like-apply (&rest args)
    (apply #'apply args)))
;;use
(like-apply #'identity '(9)) ;98 ;result of function and the the second is the value
(like-apply #'princ '(8)) ; 8 8 ;see how the result of the function and the value

					;works
(defun apply8 (&rest args)
  (let ((*print-base* 8))
    (apply #'apply args)))
;;use
(apply8 #'identity '(8)) ;;doesnt work 
(apply8 #'princ '(8)) ;prints in the correct base
(apply8 #'princ '(9)); prints 11 and 9

					;http://stackoverflow.com/questions/17904739/why-does-this-scoping-example-from-ansi-common-lisp-not-work-as-expected
;;I'm new to lisp and going through ANSI Common Lisp by Paul Graham and one of the exercises is to define a function like apply where any number printed out before it returns will be printed by default in octal.

					;I tried the following:

					;(let ((*print-base* 8))
					;  (defun like-apply (&rest args)
					;    (apply #'apply args)))
					;But it didn't work as expected:
					;
					;(like-apply #'princ '(8)); returns 8 8 (expecting 10 8)
					;The following however works:
					;
					;(defun apply8 (&rest args)
					;  (let ((*print-base* 8))
					;    (apply #'apply args)))
					;Returns correctly:
					;
					;(apply8 #'princ '(8)); returns 10 8 (as expected)
					;So my question is why does the second example work, but not the first one? Both seem to manipulate the *print-base* variable.
					;
					;answered Jul 28 at 5:52
					;
					;Common Lisp uses let to bind both lexical and "special" (dynamic) variables. The behavior you expected was lexical and the behavior you observed was dynamic. The printer variables are all special, and so let creates a dynamic binding for them.
					;
					;The printer variables are sometimes used in examples of why dynamic binding can be useful. For instance the fact that you can control the behavior of princ by binding *print-base* is enabled by dynamic binding, otherwise princ would reference the binding of *print-base* active when princ was defined.
					;
					;This behavior is a major reason many Common Lisp programmers are adamant about the *earmuffs* naming convention for special variables. Note that defvar and defparameter both create special variables.
					;
					;share|edit|flag
					;edited Jul 28 at 6:02
					;
					;
					;
					;Thanks for explaining that so clearly and for the added tip about defparameter and defvar both creating special variables. It completely makes sense what it means when they are special now. – ajivani Jul 28 at 22:02 
					;
					;
					;
					;The behavior you observe is correct.
					;
					;It will be instructive to compare
					;
					;(let ((*print-base* 8))
					;  (defun f1 ()
					;    *print-base*))
					;with
					;
					;(defun f2 ()
					;  (let ((*print-base* 8))
					;    *print-base*))
					;to discover that (f1) returns 10 while (f2) returns 8.
					;
					;This is because *print-base* is bound around the definition of f1, so it is 8 while f1 is being defined but not while it is being executed.

					;I think I get it, since it's a special variable (from m-n's answer above), it has dynamic scope. That means in the first example, it goes back to some global default value that print-base has (which is 10 if nothing is changed). In the second example , it changes the value of print-base every time f2 is called. Thanks to both of you guys. – ajivani Jul 28 at 21:58 


;;some more function builders
(defun mycddr ()
  (compose #'cdr #'cdr))

(defun mycddr2 (args)
  (funcall (compose #'cdr #'cdr)
	   args))
(mycddr2 '((a) 1 2 3)); works 

;;this works
(setf mycddr1 (compose #'cdr #'cdr))

(funcall (compose #'cdr)
	 '((a) 1 2 3))
(funcall (compose #'cdr #'cdr)
	 '((a) 1 2 3))
(funcall mycddr1 '((a) 1 2 3))


(setf my-nth2 (compose #'car #'nthcdr))
(funcall my-nth2 1 '(a b c)); b

(setf my-atom (compose #'not #'consp))
(funcall my-atom 'a)
(funcall my-atom ())
(funcall my-atom '(1))

(setf my-atom2 (rcurry #'typep 'list)) ;takes function and args and appends the the args to the end of list

(typep 1 'number)
(typep '(1 2 3) 'list)

					;so with rcurry it's like saying
(funcall my-atom2 'a)
(funcall my-atom2 '(a))
(apply #'typep (append  '(a)  '(list))); nil ;look at function def of rcurry &rest args means that it will be turned into a list automatically
(apply #'typep (append  '((a))  '(list))); T
(apply #'typep (append  '((a b))  '(list))); T

(setf my-listp (disjoin #'consp #'null))
(funcall my-listp 'a)

(setf my-1+ (curry #'+ 1))
(setf my-1+ (rcurry #'+ 1))
(funcall (rcurry #'- 1) 2)
;;see the minus 1
(setf my-1- (rcurry #'- 1))
(funcall (curry #'- 1) 2); = (- 1 2)
;;mapcan is like mapcar that combines 
(setf my-mapcan (compose (curry #'apply #'nconc) #'mapcar))
;;compase takes 2 functions that take args
;;so the first thing that happens is that each element gets mapcared
;;then the combo of apply and nconc (with curry) make a function
;;so first the thing gets mapcared, then it gets the combo of the function the curry made
(mapcan #'(lambda (x) (and (numberp x) (list x)))
	'(a 1 b c 3 4 d 5))
(funcall my-mapcan #'(lambda (x) (and (numberp x) (list x)))
	 '(a 1 b c 3 4 d 5)); (1 3 4 5)
(mapcar #'(lambda (x) (and (numberp x) (list x)))
	'(a 1 b c 3 4 d 5)); (NIL (1) NIL NIL (3) (4) NIL (5))
(nconc '(a) '(b)); '(A B)
(nconc '(a) '(b) '(c)); '(A B C)

;;see how this gives the same answer
(apply #'nconc (mapcar #'(lambda (x) (and (numberp x) (list x)))
		       '(a 1 b c 3 4 d 5))); (1 3 4 5)
(apply #'apply (append '(nconc) (mapcar #'(lambda (x) (and (numberp x) (list x)))
					'(a 1 b c 3 4 d 5))))
;;what i think is happening - and it is
(funcall #'apply #'nconc (mapcar #'(lambda (x) (and (numberp x) (list x)))
				 '(a 1 b c 3 4 d 5)))
					;(apply #'apply (append #'nconc ) )

(setf my-complement3 (curry #'compose #'not))
(funcall (funcall my-complement3 #'evenp)
	 1); T

 ;;;;chapter 7 - IO streams
 ;;;streams are objs sources or destinations of characters
 ;;;*standard-input* - to read from the stream
 ;;;*standard-output* - default place for output` 
 ;;;7.1 - streams
					;(setf path (make-pathname :name "/Users/aliya/lisp/lisp/myfile.txt")) ; pathname
(setf path (make-pathname :name "myfile100"))
(setf str (open path :direction :output
		:if-exists :supersede)) ; stream gets overwritten
(format str "Something~%") ; if we give this stream to format it will print it to that file
(format str "and this as well~%") ; now write to this stream (which isn't the defaut terminal anymore)
(close str)
(setf str (open path :direction :input))
(read-line str)
(read-line str) ; done the second time to show that we can get the second line as well.
(close str)
(with-open-file (str path :direction :output
		     :if-exists :append) ;found this by hyperspec it said it uses the same args as open
  (format str "blah~A~%" (random 100)))

 ;;;7.2 input
(defun try-input ()
  (format t "Please enter your name: ")
  (read-line))
;;to display contents of file on the toplevel
;;explain function do #'do as well
(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof); initial val for line - the variable
	       (read-line str nil 'eof))); the incremental step for line
	((eql line 'eof)); the stopping condition for 
      (format t "~A~%" line)))) ; the with-open-file 
;;test it out
(pseudo-cat "/Users/aliya/lisp/lisp/myfile.txt")
(pseudo-cat "/Users/aliya/lisp/lisp/myfile100")

					;(defun ask-read ()
					;  (read))

					;(defun ask-number2 ()
					;  (ask-number))

;;try read and read-from-string in the REPL
(prin1 "hello")
(princ "hello")
;;format directive
(format nil "Dear ~A, ~% Our records indicate" "Mr Malatesta")
(format t "~S ~A" "z" "z") ;~S directive uses prin1 and ~A directive uses princ 
(format nil "~10,2,0,'*,' F" 26.21875)
(format nil "~20,2,0,'*,' F" 26.21875)
(format nil "~20,3,0,'*,' F" 26.21875)
(format nil "~20,3,1,'*,' F" 26.21875) ;number of digits to shif the decimal point
(format nil "~20,3,-1,'*,' F" 26.21875)
(format nil "~20,100,1,'/,' F" 26.2187102398019830915) ; character to print when you don't have room
(format nil "~,2,F" 26.21875) ;same as below
(format nil "~,2F" 26.49539) ;same as thing below
(format nil "~,1F" 1.25)
;;adding more 
(format nil "~,10F  ~,10,F" 2.7813 3.14159)



 ;;;output example with ring buffers and so on
					;(defstruct buf
					;  vec (start -1) (used -1) (new -1) (end -1))
;;getter - for when you use bref as a getter
					;(defun bref (buf n)
					;  (svref (buf-vec buf)
					;         (mod n (length (buf-vec buf)))))
					;setter for what happens when you call setf with bref - seems pretty cool
					;(defun (setf bref) (val buf n)
					;  (setf (svref (buf-vec buf)
					;               (mod n (length (buf-vec buf))))
					;        val))
;;a new buffer is a vector - a 1 dimensional array
					;(defun new-buf (len)
					;  (make-buf :vec (make-array len)))
 ;;;buf insert
					;(defun buf-insert (x b)
					;  (setf (bref b (incf (buf-end b)))
					;        x))
					;
					;(defun buf-pop (b)
					;  (prog1
					;    (bref b (incf (buf-start b)))
					;    (setf (buf-used b) (buf-start b)
					;          (buf-new b) (buf-end b))))
					;
					;(defun buf-next (b)
					;  (when (< (buf-used b) (buf-new b))
					;    (bref b (incf (buf-used b)))))
					;
					;(defun buf-reset (b)
					;  (setf (buf-used b) (buf-start b)
					;        (buf-new b)  (buf-end b)))
					;
					;(defun buf-clear (b)
					;  (setf (buf-start b) -1 (buf-used b) -1
					;        (buf-new   b) -1 (buf-end b) -1))
					;
					;(defun buf-flush (b str)
					;  (do ((i (1+ (buf-used b)) (1+ i)))
					;      ((> i (buf-end b)))
					;    (princ (bref b i) str)))




(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
	 (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
	       (mod n (length (buf-vec buf))))
	val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
	  (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
	(buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used  b) -1
	(buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(setf my-buf (new-buf 3))
(buf-insert 101 my-buf)
(buf-insert 99 my-buf)
(buf-insert 22 my-buf)
(buf-insert 1 my-buf)
(buf-pop my-buf)
(buf-next my-buf)
(buf-reset my-buf)
(buf-clear my-buf)
my-buf
(buf-pop my-buf)

(buf-insert '3 my-buf)
(buf-insert '3 my-buf)
(buf-insert '3 my-buf)
(buf-pop my-buf)
(buf-next my-buf)
(buf-flush my-buf nil);error here!

;;now try writing this out and figureing it out

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)       ;(var initial-value update)
	    (or (setf from-buf (buf-next buf)) 
		(read-char in nil :eof)))) 
	((eql c :eof))                    ;stopping cond and return value
      (cond ((char= c (char old pos))     ;the body and expr in cron order
	     (incf pos)
	     (cond ((= pos len)            ; 3 we have complete match and we write the replacement to output stream and 
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)         ; 2 if match begins queue into the buffer b
		    (buf-insert c buf))))
	    ((zerop pos)                   ; 1 until it mataches the first character write back exactly the same thing to the output file
	     (princ c out)
	     (when from-buf
	       (buf-pop buf)
	       (buf-reset buf)))
	    (t                             ; 4 if it fails before this point we can pop the first char in the buffer and write it to the output stream after which we reset the the buffer and start over with pos equal to zero
	     (unless from-buf
	       (buf-insert c buf))
	     (princ (buf-pop buf) out)
	     (buf-reset buf)
	     (setf pos 0))))
    (buf-flush buf out)))

;;;;;;;;;;;;;;;;;;;;test and change paths;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;these won't work on every operating system so test them out
					;(file-subst "baro" "baric" "~/lisp/pg/myfile.txt" "~/lisp/pg/outputfile.txt")
					;(file-subst "bar" "XxxxX" "~/lisp/pg/myfile.txt" "~/lisp/pg/outputfile.txt")
					;(step (file-subst "baro" "baric123456" "~/lisp/pg/myfile.txt" "~/lisp/pg/outputfile.txt"))
					;(file-subst " th" " z" "test1" "test2")


;;file with word barbarous
;;sub "baro" baric


 ;;;;needed to do this to see what the hell was happening in the above code
 ;;;;algorithm is pretty simple after going over it
(setf my-buf (new-buf 4))
(buf-insert 'b my-buf) ;2
(buf-insert 'a my-buf);2
(buf-insert 'r my-buf);2
(buf-insert 'b my-buf);4 - unless from-buf (which is nil in this case so it'll insert 'b into the buffer
(princ (buf-pop my-buf));4 but pop out the first b - and setf to zero since this wasn't a match
(buf-reset my-buf);4 ;resets the buffer - means the next thing must be chosen from the buffer
my-buf
(buf-next my-buf); from the loop - 'a is the value that gets passed to c (our character incrementer)
(princ 'a) ;1 - to the output stream
(buf-pop my-buf); and removes a from the buffer
(buf-reset my-buf); so that it tries to get the rest of the stuff from the buffer
my-buf
(buf-next my-buf); from the loop - 'a is the value that gets passed to c (our character incrementer)
(princ 'r) ;1 - to the output stream
(buf-pop my-buf); and removes r from the buffer
(buf-reset my-buf)
my-buf
(buf-next my-buf)
;;here's where it goes into 1 and pos = 1
(buf-next my-buf); nil
;;read in the next character -- barb - a
(char "baro" 1); a == a
(buf-insert 'a my-buf)
my-buf
					;read in next character - barba - r
(char "baro" 2); r == r
(buf-insert 'r my-buf)
my-buf
;;read in next char - barbar - o
my-buf
(buf-next my-buf) ;nil - so the next char read is o

(char "baro" 3) ; 0==o 
;;position 3
(length "baro"); 4 == pos == lenght of string so we can replace and reset
(princ "baric")
(buf-clear my-buf)
my-buf
(buf-next my-buf); nil so barbaro - u
(princ 'u); 1
my-buf
(buf-next my-buf); nil so barbarou - s
(princ 's); 1

;;read macro
;;remember to start emacs do the following:
;;C-x 3 2 -- starts the emacs buffer
;;C-x 5 2 -- opens a new window
;;go to other window and click - M-x slime - this will start the slime buffer
;;if a buffer moves away or whatever, you can always do a quick - M-x switch-to-buffer command and get there
;;C-c C-c - will compile the whatever you want into the repl
;;C-x C-p will do a quick eval of the
(car (read-from-string "'a")) ; notice how it prints quote the macro character
(car (read-from-string "(quote a)"))
(cadr (read-from-string "'a")) ; see how this is a now
(car (read-from-string "#'+")) ; function (like quote but you see how this is a function dispatch macro)
;;other dispatching read macros
					;(car (read-from-string "#(1 2 3)"))
(car (read-from-string "(vectora 1 2 3)"))
(let ((*print-pretty* t))
  (vectorp (read-from-string (format nil "~S"
				     (vector 1 2))))) ;evals to T 
(vectorp (car (read-from-string "(vector 1 2 3)"))) ;returns nil
(vectorp (read-from-string (format nil "~S"
				   (vector 1 2)))) ; evals to T
(let ((*print-pretty* t))
  (read-from-string (format nil "~S"
			    (vector 1 2)))) ;notice that it actually uses the read macro to get this stuff
;;this makes it an actual vector! so you're reading in a vector not the string when you use the read macro
					;(let ((*print-pretty* t))
					;  (prin1 (read-from-string (vector 1 2))))
					;so when read encounters a macro character like ' it calls the associated function

;;7-1 takes a filename and returns a list of strings representing each line in the file.
;;attempt 1 - doesn't work
(defun file-to-list (filename)
  (let ((path (make-pathname :name filename)))
    (with-open-file (s path :direction :input)
      (do ((line (read-line s nil 'eof)
		 (read-line s nil 'eof)))
	  (format t "~A~%" line)))))
;;attempt 2 - wrong
(defun file-to-list-helper (stream path)
  (with-open-file (stream path :direction :input)
    (do ((line (read-line stream nil 'eof)
	       (read-line stream nil 'eof)))
	(format t "~A~%" line))))
(defun file-to-list (filename)
  (let ((strlist nil))
    (with-open-file (str filename :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  (push 1ine strlist)))))

					; (file-to-list "myfile.txt") - myfile has to be a file of expressions


(let ((list-strings nil))
  (defun file-to-list (filename)
    (with-open-file (stream filename :direction :input)
      (do ((line (read-line stream nil 'eof)
		 (read-line stream nil 'eof)))
	  ((eql line 'eof)
	   list-strings)
	(push line list-strings)))))

(setf *teststrings* nil)

					;(setf *teststrings* (file-to-list "myfile.txt")) ;this duplicates it, we need something else




;;7-1 the internet way using an accumulator
(defun lines->list (path)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil nil) (read-line str nil nil))
	 (acc nil (cons line acc))) ;why not backwards
	((not line) (nreverse acc)))))
					;just for fun - and the wrong way
(defun lines->list2 (path)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil nil) (read-line str nil nil))
	 (acc nil (cons acc line)))
	((not line) acc))))


(setf *teststrings3* nil)
					;(setf *teststrings3* (lines->list "myfile.txt"))
*teststrings3*
					;(setf *teststrings3* (lines->list2 "myfile.txt"))
*teststrings3*

;;the let-over-lamda ends up saving the previous state! like a logger! seeems like cool
;;here is what i want to do instead
					;7-2
(defun file-to-list (path)
  (let ((list-strings nil))
    (with-open-file (str path :direction :input)
      (do ((line (read-line str 'nil 'eof) ;initial value for line
		 (read-line str 'nil 'eof)))
	  ((eql line 'eof) list-strings)
	(push line list-strings)))))

					;(setf *teststrings* (file-to-list "myfile.txt"))
(first *teststrings*) ;see how this works no matter how many times you call the above command in a row (unlike usinng the let-over-lambda since that saves the var)

(defun file-to-strings (path)
  (let ((list-strings nil))
    (with-open-file (str path :direction :input)
      (do ((line (read-line str 'nil 'eof) (read-line str 'nil 'eof)))
	  ((eql line 'eof) list-strings)
	(push line list-strings)))))

;;7-2
(defun file-to-s-expressions (path)
  (let ((list-exprs nil))
    (with-open-file (str path :direction :input)
      (do ((expr (read str nil nil) (read str nil nil)))
	  ((eql expr 'eof) list-exprs) ;maybe since the last expression isn't ever 'eof we never find someting?
	(push expr list-exprs)))))


(defun s->lists (path)
  (with-open-file (str path :direction :input)
    (do ((s (read str nil nil) (read str nil nil))
	 (acc nil (cons s acc)))
	((not s) (nreverse acc)))))

(setf *teststrings4* nil)
;;;;(setf *teststrings4* (file-to-s-expressions "exprfile.txt")) ; but this doesn't work!! causes errors still
*teststrings4*
					;(setf *teststrings4* (s->lists "exprfile.txt")) ; see this works 



					;7-3 function that takes two filenames and writes the second file a copy of the first, minus comments
(setf *lexample* "some code here % and some comments here")
(subseq *lexample* 0 (position #\% *lexample*))

(defun remove-comments (file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	  ((eql line 'eof))
	(princ (subseq line 0 (position #\% line)) out)))))

					;(remove-comments "/Volumes/MEDIA/lisp/pg/commentsfile.txt" "/Volumes/MEDIA/lisp/pg/myfile1.txt")

(defun remove-comments2 (file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	  ((eql line 'eof))
	(format out "~A~%" (subseq line 0 (position #\% line))))))); this one adds teh new line

;;notice how this works now - prints the new line
					;(remove-comments2 "/Volumes/MEDIA/lisp/pg/commentsfile.txt" "/Volumes/MEDIA/lisp/pg/myfile1.txt")


					;some stuff after adding it to the online git repo
					;touch README.md
					;git init
					;git add README.md ; or use "git add ." to add everything in this folder
					;git commit -m "first commit"
					;git remote add origin https://github.com/ajivani/lisp.git
					;git push -u origin master

					;Push an existing repository from the command line
					;git remote add origin https://github.com/ajivani/lisp.git
					;git push -u origin master

					;lets test this now
					;basically to clone we need git clone git://github.com/ajivani/lisp.git nameofgitfolder
					;then it automatically downloads stuff here!
;;git add .
;;git commit -m "some comment on the commit"
;;git push -u origin master

;;7-4 - function that takes a 2d array of floats and displays it in neat columns

					;quick array review
(setf 2darry (make-array '(10 4 2) :initial-element 2.718)) ;means 3d array
(setf (aref 2darry 2 1 1) 'blue) ;see what element 
(setf 2darry (make-array '(12 4) :initial-element 2.718))
(setf 2darry2 (make-array '(3 9) :initial-element pi))
(setf (aref 2darry 3 1) 'changed-stuff)
(array-dimensions 2darry) ;(12 4)
;;mess with these as well to get a feel for it
(format nil "~5,50,F" 123.1234567909876543210)
(format nil "~10,10,0,'*,F" 123.45678901234567890)
(format nil "~10,5,0,'*,F" 123.45678901234567890)
(format nil "~10,10,0,'*,F   ~10,2,0,,F" 123.45678901234567890 94123.7885)
(format nil "~10,2,0,'*,F   ~10,2,0,,F" 123.45678901234567890 94123.7885)
(format nil "~10,5,0,'*,F   ~10,2,0,,F ~%rest of it" 123.45678901234567890 94123.7885)
(format nil "~10,2,0,'*,F ~10,2,0,,F" 123.45678901234567890 94123.7885)
(format nil "~10,2,0,'*,F~10,2,0,,F" 123.45678901234567890 94123.7885)
(format nil "~10,8,0,'*,F~10,10,0,,F" 123.45678901234567890 94123.788580938409324809)

					;use dotimes to do this -- really easy :)
(defun print-2d-array (arry)
  (let* ((dims (array-dimensions arry))
	 (a (car dims))
	 (b (cadr dims)))
    (dotimes (x a)
      (format t "~%")
      (dotimes (y b)
	(format t "~10,2,0,'*,F" (aref arry x y))))))

(print-2d-array 2darry)
(print-2d-array 2darry2)

					;7-5 modify stream-subst to allow wildcards in the pattern if the char + occurs in old, it should match any input char

(defun stream-subst (old new in out)
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)       ;(var initial-value update)
	    (or (setf from-buf (buf-next buf)) 
		(read-char in nil :eof)))) 
	((eql c :eof))                    ;stopping cond and return value
      (cond ((or (char= c (char old pos))     ;the body and expr in cron order
		 (char= #\+ (char old pos))) ;so if it equals 
	     (incf pos)
	     (cond ((= pos len)            ; 3 we have complete match and we write the replacement to output stream and 
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)         ; 2 if match begins queue into the buffer b
		    (buf-insert c buf))))
	    ((zerop pos)                   ; 1 until it mataches the first character write back exactly the same thing to the output file
	     (princ c out)
	     (when from-buf
	       (buf-pop buf)
	       (buf-reset buf)))
	    (t                             ; 4 if it fails before this point we can pop the first char in the buffer and write it to the output stream after which we reset the the buffer and start over with pos equal to zero
	     (unless from-buf
	       (buf-insert c buf))
	     (princ (buf-pop buf) out)
	     (buf-reset buf)
	     (setf pos 0))))
    (buf-flush buf out)))

					;7-6 modify stream-subst so the pattern can include an element that matches any digit character, an element that matches
					;any alphanumeric char, or an element that matches any char. the pattern must also be able to match any specific
					;input char (hint: old can no longer be a stringa)
;;see the solution that uses labels - pg 102
(labels ((add10 (x) (+ x 10))
	 (consa (x) (cons 'a x)))
  (consa (add10 13)))




;;;;chapter 8  ch 8 chap 8 - symbols
(symbol-name 'abc); can use symbols as names as well as data objects
					;lisp is case insensitvie
(eql 'aBc 'Abc)
(CaR '(a b c)) 

(list '|Lisp 1.5| '|| '|a b c|  '|abc| '|ABC|) ;when symbol name is read no case conversion and macro chars are treated like other chars

(symbol-name '|a b c|) ; reutnrs "a b c" because that's the name

;;;8-2 property lists

;;get takes a symbol and a key of any type (using eql) and returns the value associated with that key
;;in the symbol's property list 
(get 'alizarian 'color) ;nil - this is returning the value associated with the key in the symobl's property list

(setf (get 'alizarin 'color) 'red) ;how to assoc a value with a key use setf like so
(get 'alizarin 'color)

(setf (get 'alizarin 'transparancy) 'high)

;;remember diagram from figure 8.1 has symbol ((name "FOO") (package somepackage) (value 27) (function #<function>) (plist (TRANSPARANCY HIGH COLOR RED)))
(symbol-plist 'alizarin) ;property lists aren't represented as assoc lists though used int he same way (but nowdays we use hash tables instead) 

;;;8-3 creating symbols
;;packages are symbol-tables mappings (mapping names to symbols)
;;a symbol that belongs to a package is said to be interned in that package - functions and vars have symbols as their names
(intern "random-symbol") ;a way to intern a symbol - string and an optional package (defualts to current package)


					;(defpackage "MY-APPLICATION"
					;  (:use "COMMON-LISP" "MY-UTILITIES")
					;  (:nicknames "APP")
					;  (:export "WIN" "LOSE" "DRAW"))
;;Keywords
;;symbols in the keyword package (known as keywords) - 2 unique properties
;;always evaluate to themselves and you can refer to them anywhere as :x instead of keyword:x

(defun noise (animal)
  (case animal
    (:dog :woof)
    (:cat :meow)
    (keyword:pig :oink))) ;see how 

;;symbols related to vars 2 ways - symb name of a special var, the value of the variable is stored in a field within the sybmol itself liek the (figure 8.1 drawing)

;;Random Text exmample 8-8
;;two parts - part one will read a sample text accumulating info on likelihood of one word following another
;;part one make a hashtable with key as a word and values as words that follow it
;; key |discover| and value ((|sin| . 1) (|wide| . 2) (|sights| . 1))
;;part 2 - make a random walk and choose the next word based on previous words

;;see how intern below works
(intern "random-symbol") ;|random-symbol| ;makes this string into a symbol
(assoc '|.| '( (silly . 1) (|.| . 3)  (|!| . 1) (nice . 1))) ; (|.| . 3)
(assoc 'boy '( (silly . 1) (boy . 3)  (said . 1) (nice . 1)))
;;--part 1
(defparameter *words* (make-hash-table :size 10000)) ;Will Contain our hashtable of keys and values 

(defconstant maxword 100) ; biggest word

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
	  (pos 0))
      (do ((c (read-char s nil :eof)
	      (read-char s nil :eof)))
	  ((eql c :eof))
	(if (or (alpha-char-p c) (char= c #\'))
	    (progn
	      (setf (aref buffer pos) c)
	      (incf pos))
	    (progn
	      (unless (zerop pos)
		(see (intern (string-downcase
			      (subseq buffer 0 pos)))) ;check symbol and add it in
		(setf pos 0))
	      (let ((p (punc c)))
		(if p (see p))))))))) ;treat it as a separate 

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|)
    (#\! '|!|) (#\? '|?|)))

;;eg the gethash would return something like the following:
;;((|sin| . 1) (|wide| . 2) (|sights| . 1))
;;we pick the current symbol and 
(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*)))) ; (boy . 3)
      (if (null pair)
	  (push (cons symb 1) (gethash prev *words*)) ;add word to the values
	  (incf (cdr pair))))
    (setf prev symb)))

(dolist (x '("the" "quick" "and" "the" "dead" "." "the" "bold" "and" "the" "beautiful"))
  (prin1 (intern x))
  (see (intern x)))


(defun quick-hash-print (ht)
  (maphash #'(lambda (k v)
	       (format t "~A -- ~A  " k v))
	   ht))
					;(quick-hash-print *words*)
					;quick review on cycling through a hash table
(maphash (compose #'(lambda (k v) (format t " ~A:~A " k v)))
	 *words*)

(defun quick-hash (ht)
  (maphash #'(lambda (k v)
	       (progn
		 (princ k)
		 (princ v)
		 (print " ----- ")))
	   ht))
(quick-hash *words*)
(see (intern "the"))

;;now for part 2 - uses a random-next to geerate a new word using 
(defun generate-text (n &optional (prev '|.|)) ;optional value with '|.| as the first value
  (if (zerop n)
      (terpri) ;simple output function like princ - probably prints just a new line
      (let ((next (random-next prev)))
	(format t "~A " next)
	(generate-text (1- n) next))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
	 (i (random (reduce #'+ choices
			    :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair))) ;like saying if i - frequency of pair  < 0 then pick this one - since i is random sometimes the less likely thing will also get picked
	  (return (car pair))))))


;;used this to understand what was going on
(reduce #'+ (gethash '|.| *words*) :key #'cdr)
(gethash '|.| *words*) ;((|the| . 1))
(reduce #'princ (gethash '|.| *words*)) ;(|the| . 1)
(reduce #'princ (gethash '|.| *words*) :key #'cdr) ; 1
(reduce #'+ (gethash '|.| *words*) :key #'cdr) ;1 but if we had multiple numbers then the sum adds up and we pick a random number between 0 and the sum
					;(read-text "path-to-textfile")
					;(generate-text 10)

;;exercise 8-1
;;yes it's possible if they belong to 
(eql 'a 'a) ;T unless they belong to different packages


;;random side problem 1 - random 1-1
					;Use the number 5 and print all possible sums
					;( 1 1 1 1 1)
					;( (1 1) 1 1 1)
					;( (1 1) (1 1) 1)
					;( (1 1) (1 1) (1 1)); fails
					;( (1 1 1) 1 1)
					;( (1 1 1) (1 1))
					;( (1 1 1) (1 1 nil));fails
					;( (1 1 1 1) 1)
					;( (1 1 1 1) (1 1 1 1); fails

(defun expand-nums (n)
  (let ((l '()))
    (expand-nums-helper n l)))

(defun expand-nums-helper (n li)
  (cond
    ((eql 0 n) li)
    (t
     (expand-nums-helper (1- n) (push '1 li)))))


;;;chapter 9 - numbers (alomst at macros chapter)

(mapcar #'float '(1 2/3 .5)) 
(truncate 1.3)
(floor 1.4) ;ret 1 and the diff
(ceiling 1.4) ;ret 2 and the diff

					;using floor and celing we can simplify the def of mirror? we saw earlier
(defun palindrome? (x)
  (let ((mid (/ (length x) 2)))
    (equal (subseq x 0 (floor mid))
	   (reverse (subseq x (ceiling mid))))))

(palindrome? "racecar")

(defun our-truncate (n)
  (if (> n 0)
      (floor n)
      (ceiling n)))
(our-truncate -1.34)
(truncate -1.34)

					;round returns the nearest even digit
(mapcar #'round '(-2.5 -1.5 1.5 2.5 2.9 3.1))
(mapcar #'signum '(-2 -0.0 0.0 0 .5 3))
(* (abs -.3) (signum -.3)) ; always returns x 
(list (minusp -0.0) (zerop -0.0))
(/ 365 12) ;gives a ratio
(float 365/12) ;gives the float of the ratio
(expt 2 5) ; 2^5
(log 32 2) 
(exp 1) ;is e ^ 1
(exp 2)
(expt 27 1/3) ;use this to find roots
(expt 49 1/2) ;see
(sqrt 49) ;it is faster
;;trig functions
(let ((x (/ pi 4)))
  (list (sin x) (cos x) (tan x)))
(let ((x (/ pi 2)))
  (list (sin x) (cos x) (tan x)))

(values most-positive-fixnum)
(values most-negative-fixnum)

(typep 1 'fixnum)
(typep most-positive-fixnum 'fixnum)
(typep (1+ most-positive-fixnum) 'bignum)

;;ray tracing - define an eye, light source(s), surfaces, image plane
;;projection of the world into the image plane

;;figure 9-2 9.2 some math utilities


(defun sq (x) (* x x))
;;magnitude - lenght of a vector given it's coordinates - think size of arrow
(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))
;;just do a quick video on khan academy on unit vectors - 1/|V| * |V| = 1 - scalar times the vector equals 1 that is what the unit vector is
(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

					;the magnitude of any unit vector is 1.0 by definition 
(multiple-value-call #'mag (unit-vector 23 12 478)) ;1.0 
(mag (/ 23 (mag 23 12 478)) (/ 12 (mag 23 12 478)) (/ 478 (mag 23 12 478))) ;1.0

;;fig 9.3 when we use :conc-name it means we can call just use the name given
(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc)))
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))


;;;now make the surface
(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output :if-exists :supersede)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
	  ((< (- 50 y) inc))
	(do ((x -50 (+ x inc)))
	    ((< (- 50 x) inc))
	  (print (color-at x y) p))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
		   (- y (y eye))
		   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
	(* (lambert s int xr yr zr) (surface-color s)) ;intensity * color
	0))) ;means a miss and should be black (ie we get the background)

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

;;Figure 9.5
(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
	    :radius r
	    :center (make-point :x x :y y :z z)
	    :color c)))
    (push s *world*)
    s))
;;typecase (cube #'cube-intersect) if we needed to add to this - depending on the type of s we can make it a sphere if we want
(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
	   s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-point :x (+ (x pt) (* n xr))
		    :y (+ (y pt) (* n yr))
		    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))

;;mess with the spheres 
(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -500 200 .8)
  (defsphere -80 -150 -500 200 .7)
  (defsphere 70 -100 -500 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "/Users/aliya/lisp/lisp/spheres-test.pgm") res))

;;works on mac laptop not on desktop
(defun ray-test2 (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .3)
  (defsphere -80 -150 -1200 200 .3)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 4))
    (do ((z 2 (1+ z)))
	((> z 10))
      (defsphere (* x 200) 300 (* z -400) 10 1)))
  (tracer (make-pathname :name "spheres-test.pgm") res))

					;(ray-test 4)

;;ex 9-1 takes a list of reals and returns true iif they are in non-decreasing order
(defun decreasing-realsp (l)
  (cond
    ((null l) nil)
    (t
     (let ((c (car l))
	   (ret t))
       (dolist (r (cdr l))
	 (if (>= c r) 
	     (setf c r)
	     (return (setf ret nil))))
       ret))))

(decreasing-realsp '())
(decreasing-realsp '(2.0 1.9 1.1 1.2 0.9))
(decreasing-realsp '(2.0 1.9 1.112))

;;an even easier one!
(defun decreasing-realsp2 (l)
  (apply #'>= l)) ;;sees if current element is > or equal to the previous one

(decreasing-realsp2 '(2.0 1.9 1.1 1.01 1.012))

;;ex 9-2 takes interger number of cents and returns 4 vals on how to make that numer using .25 .10 .5 .1 cents
(defun make-change (value)
  (let ((money '(25 10 5 1))
	(new-val (* 100 value)))
    (dolist (x money)
      (multiple-value-bind (c val) (change-helper new-val x)
	(progn
	  (setf new-val val)
	  (format t "Unit ~A: ~A~%" x c))))))

(defun change-helper (value den)
  (let* ((c (truncate (/ value den)))
	 (new-val (- value (* c den))))
    (values c new-val)))

(make-change 1.89)

;;ex 9-3 simulate contest
(defun sim-contest (&rest args)
  (let* ((res (map-int #'(lambda (x) (random 2)) 10))
	 (wi (apply #'+ res))
	 (wag (- 10 wi)))
    (values wi wag)))

;;ex 9-4 - screw it macro time!

;;random side problem 2 - random 2
;;find power of n using the divide and conquer
;;using mit open courseware lesson 3 - Introduction to algorithms (2005)
(eql (* (exp 2.4) (exp 2)) (exp (+ 2.4 2))) ;T
(eql (exp 5) (* (exp (/ (- 5 1) 2)) (exp (/ (- 5 1) 2)) (exp 1))) ;T
;;note that (n-1)/2 would have to be even if n were odd
;;and that we can use n/2 if n is even and (n-1)/2 if n is odd
;;x^n = x^(n/2) * x^(n/2) if n is even
;;x^n = x^((n-1)/2) *  x^((n-1)/2) * x if x is odd

;;well no point dividing and conconquering if you're doing the recursive call twice!
(defun find-powers-d&c (x n)
  (cond
    ((eql 0 n) 1)
    ((eql 1 n) x)
    (t
     (if (evenp n)
	 (* (find-powers-d&c x (/ n 2)) (find-powers-d&c x (/ n 2)))
	 (* (find-powers-d&c x (/ (- n 1) 2)) (find-powers-d&c x (/ (- n 1) 2)) x)))))

;;make it faster - why use two recursive calls when you only need to use 1 and 
;;in case n is odd you use 1 recursive call and two multiplicaiton calls
(defun find-powers (x n)
  (cond
    ((eql 0 n) 1)
    ((eql 1 n) x)
    (t
     (cond 
       ((evenp n)
	(let ((recurse-val (find-powers x (/ n 2))))
	  (* recurse-val recurse-val)))
       (t
	(let ((recurse-val (find-powers x (/ (- n 1) 2))))
	  (* recurse-val recurse-val x)))))))



;;random question 3 - random 3 - find max subarray
;;in book "Introduction to Algorithms" - Thomas C Corman
;;using divide and conquer - find the max crossing array
;;find the max left
;;find the max right
;;one of those 3 will be the overall max
(setf *pricesperday* (vector 13 -3 -25 20 -3 -16 -23 18 20 -7 12 -5 -22 15 -4 7))

(defun find-max-crossing-subarray (A low mid high)
  (let ((left-sum -10000)
	(sum 0)) 
    (do ((i mid (1- i)))
	((eql i low)))))

;;;;;;;;;;Chapter 10 macros ch 10;;;;;;;
;;lisp code is expressed as lists - from expressions to code
;;10.1 eval - takes an expression, evaluates it, and returns it's value
(eval '(+ 1 2 3))
(eval '(format t "Hello"))
;;eval is what we talk to at the toplevel
(defun our-toplevel()
  (do ()
      (nil)
    (format t "%> ")
    (print (eval (read))))) ;;hmmm repl - Read Eval Print L(oop) - just a guess on the last one


(symbol-function '+) ;the function #'+ (the plus function)
(apply (symbol-function '+)
       (mapcar #'(lambda (x)
		   (eval x ))
	       '(1 2 3))) ; 6

;;get function in car of the expression, eval all the args in the cdr and then
;;return the result of applying the former to the latter 
;;(ie the function in car to the result of the eval of the args - which will most likely be a list of args)
(defun our-fake-eval (expr env)
  (cond
    ((eql (car expr) 'quote) (cadr expr)) ;quotes are special functions - quote's job is to preserve it's args from evaluation
    (t
     (apply (symbol-function (car expr))
	    (mapcar #'(lambda (x)
			(eval x)) ;(eval x env)) ;env there to represent the lexical closures
		    (cdr expr))))))

(coerce #'(lambda (x) x) 'function) ;makes it a function

(compile nil '(lambda (x) (+ x 2)))

;;now macros! - how a macro should be translated to (setf x nil) before being compiled or evaluated
(defmacro nil! (x)
  (list 'setf x nil))

(setf *x* 1)
(nil! *x*)

(macroexpand-1 '(nil! x)) ; takes a macro call and generates it's expansion

;;here is what macroexpand-1 is really doing
(funcall #'(lambda (expr)
	     (apply #'(lambda (x) (list 'setf x nil))
		    (cdr expr)))
	 '(nil! *x*)); (SETF *X* NIL)

;;could be something like 
(apply #'+ '(3)) ;but instead
(apply #'(lambda (x) (+ x 1))
       '(1))
;;causes an error without the quote but with the quote it produces the function below
(apply #'(lambda (x) (list 'setf x nil))
       '(*x*)); (setf *x* nil)
;;ooh confused check out how ' makes it into code and not value
(funcall #'(lambda (x) (list '+ x 'x x x 'x 1)) 200); (+ 20 X 20 20 X 1)
(list (+ 1 1) '(+ 1 1)) ;(2 (+ 1 1)) ;see how one is the value of 1+1 and one is the list (+ 1 1)

(funcall #'(lambda (a b c) (list a b c))
	 'setf
	 'x
	 'nil) ; (SETF X NIL) pretty cool huh
;;backquote
`(a b c) ; by itself same as regular quote
;;but backquotes can be used with commas to evaluate the function
;;can use , and ,@ (comma-at) to turn on evaluation
(setf a 1 b 2)
`(a is ,a and b is ,b) ;(A IS 1 AND B IS 2)

;;lets us define nil! witht he backquote and x - exaclty as we wanted it
(defmacro nil! (x)
  `(setf ,x nil))

(macroexpand-1 '(nil! x))

(nil! a)

(setf *testlst* '(a b c))
;; , with a backquote
`(*testlst* is ,*testlst*); (*TESTLST* IS (A B C))
;;comma-at ,@ with a backquote will show the 
`(*testlst* is ,@*testlst*) ;(*TESTLST* IS A B C)
`(lst is ,lst) ;(LST IS (2))
`(comma-at lst is ,@lst)

;;select open paran and press (ctrl + c + return) (control + c + enter) to expand macro quickly
`(a (+ 1 2) 3) ; (A (+ 1 2) 3)
`(a ,(+ 1 2) 3); (A 3 3)
`(a (list a b) 3); (A (list A B) 3)
`(a ,(list 1 2) 3); (A (1 2) 3)
`(a ,(list (+ 1 1 8 9) 2) 3); (A (19 2) 3)
`(a ,@(list (+ 1 1 8 9) 2) 3); (A 19 2 3)
`(a ,@(list (+ 1 7 3 2) 2) 3); (A 13 2 3)
`(a ,(list 'a 'b) 3); (A (A B) 3)

;;see it fails since it's trying to evaluate 
					;`(a ,(list a b) 3); FAILS because (list a b) would fail since neither a or b are functions
					;looks like it evalutates everythin in the list
`(a ,(list 'a (list 1 2) 'b) d); (A (A (1 2) B) d)
`(a ,(list 'a (+ 1 2) 'b) d); (A (A 3 B) D)
`(a ,@(list 'a (+ 1 2) 'b) d); (A A 3 B D) ; see how everything gets selected



;;this is like while (test) (body1) (body2) (body3) -- basically we make a list
;;of lists that 
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;;now use the macro - see how the first set of () is a the test
;;and the sets of () that come after will be the 
(let ((x 0))
  (while (< x 10) ; test is (< x 10) in this case
    (print x)     ;body notice anything after the first () will be 
    (incf x)))    ;body 

(macroexpand-1 '(let ((x 0))
		 (while (< x 10)
		   (princ x)
		   (incf x)))); didn't work
					;this works
(let ((x 0))
  (macroexpand-1 '(while (< x 10)
		   (princ x)
		   (incf x)))) ;(DO () ((NOT (< X 10))) (PRINC X) (INCF X))

(let ((x 0))
  (pprint (macroexpand-1 '(while (< x 10)
			   (princ x)
			   (incf x))))) ;lol not printed prettier

;;quick git review -- setup already exists
;;git add .                            --will add all the files
;;git commit -m "some commit comment"  --commits it to the master branch
;;git push                             --will push it ot github

(defun quicksort (vec l r)
  (let ((i l)
	(j r)
	(p (svref vec (round (+ 1 r) 2)))) ;1 - find the pivot
    (while (<= i j)
      (while (< (svref vec i) p) (incf i)) ;2 - stops when we find the element that is > pivot or continues until i is the pivot
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
	(rotatef (svref vec i) (svref vec j))
	(incf i)
	(decf j))
      (if (> (- j l) 1) (quicksort vec l j));3 - if each partition has more than 1 element then break it down further
      (if (> (- r i) 1) (quicksort vec i r))); - if right - index > 1 means we have more than one element - so we can further break down this partition as well
    vec))

;;test it out
(setf *v1* (vector 10 8 7 -17 19 2 10 15))
(quicksort *v1* 0 (1- (length *v1*))) ;;also note it's not destructive

(defun quicksort1 (vec l r)
  (let ((a l)
	(b r)
	(p (svref vec (round (1+ r) 2))))
    (while (<= a b)
      (while (< (svref vec a) p) (incf a))
      (while (> (svref vec b) p) (decf b))
      (when (<= a b)
	(rotatef (svref vec a) (svref vec b))
	(incf a)   ;otherwise the next bit messes up
	(decf b))  ;otherwise the next part messes up
      (if (> (- b l) 1) (quicksort1 vec l b)) 
      (if (> (- r a) 1) (quicksort1 vec a r)))
    vec))

;;one more time for practice - just to see if i understand quicksort
(defun quicksort2 (vec l r)
  (let ((a l)
	(z r)
	(p (svref vec (round (+ 1 r) 2))))
    (while (<= a z)
      (while (< (svref vec a) p) (incf a))
      (while (< p (svref vec z)) (decf z))
      (when (<= a z) ;;means we found something that needs to switch
	(rotatef (svref vec a) (svref vec z))
	(incf a)
	(decf z))
      (if (> (- z l) 1) (quicksort2 vec l z))
      (if (> (- r a) 1) (quicksort2 vec a r)))
    vec))

(defun quicksort-memory1 (vec l r)
  (let ((a l)
	(z r)
	(p (svref vec (round r 2))))
    (while (<= a z)
      (while (> p (svref vec a)) (incf a))
      (while (< p (svref vec z)) (decf z))
      (when (<= a z)
	(rotatef (svref vec a) (svref vec z))
	(incf a)
	(decf z)
	(if (> (- r a) 1) (quicksort-memory1 vec a r))
	(if (> (- z l) 1) (quicksort-memory1 vec l z))))
    vec))


;;see if it works again
(quicksort2 *v1* 0 (1- (length *v1*)))

;;;MACRO DESIGN
;;ntimes macro - works like this -  (ntimes 10 (princ "."))

;;incorrect
(defmacro ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))
;;seems to work
(let ((y 10))
  (ntimes y
	  (princ ".")))
;;but when we change the let var to x it conflicts with the internal x in the defmacro!
(let ((x 10))
  (ntimes x
	  (princ "."))) ;doesn't work!!
;;here's a better example of it not working
(let ((x 100))
  (ntimes 7
	  (print x)
	  (setf x (+ x 1))
	  (princ x))
  x) ;  100 is returned! instead of 107; it increments the wrong x - increments
					;the x in the ntimes macro instead of the x in the let, the one we wanted
;;see this is an example of inadvertent variable capture

(let ((x 100))
  (pprint (macroexpand-1 '(ntimes 7
			   (setf x (+ x 1))))))

;;using gensyms
(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
	 ((>= ,g ,n)); see how this comma n messes things up ,n == (setf v (- v 1))
       ,@body)))

;;notice there are only 2 (half as many)
;;multiple evaluation is what it falls for
(let ((v 5))
  (ntimes (setf v (- v 1))
	  (princ ".")))

(defmacro  ntimes (n &rest body)
  (let ((g (gensym))
	(h (gensym))) ;create 2 gensyms to avoid variable capture
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
	   (>= ,g ,h)
	 ,@body))))

(pprint (macroexpand-1 '(cond (a b)
			 (c d e)
			 (t f))))


;;10.6 generalized reference
(defmacro cah (lst)
  `(car ,lst))
;;macros get expanded right into the code - any macro whose expansion can be the first arg to setf can itself be the first arg to setf

(let ((x (list 'a 'b 'c)))
  (setf (cah x) 44)
  x)

(let ((x (list 'a 'b 'c)))
  (pprint (macroexpand-1 '(setf (cah x) 44))))

(defmacro my-incf (x &optional (y 1))
  `(setf ,x (+ ,x ,y)))

(setf lst nil)
(setf (car (push 1 lst)) (1+ (car (push 1 lst))))
lst ;(1 2)
(setf lst nil)
(my-incf (car (push 1 lst)))
lst ;(1 2)
(setf lst nil)
(incf (car (push 1 lst)))
lst ;(2) ;;notice how the above doesn't match this one
;;using define-modify-macro would works - takes 3 args
;;name of macro - additional params - name of function that yields the new value of the place
(define-modify-macro our-incf (&optional (y 1)) +)
(define-modify-macro append1f (val)
  (lambda (lst val) (append lst (list val))))

(let ((lst '(a b c)))
  (append1f lst 'd)
  lst)

;;;10.7 Example: Macro Utilities
;; C-c RET will return the macro

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop)) ;done to avoid multiple calls since ,stop can be a call to something that evaluates to a number
	 ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

					;(defmacro random-choice (&rest exprs)
					;  `(case (random ,(length exprs))
					;     ,@(let ((key 


;;Uses
(for x 1 8
     (princ x))

(in '- '* '+ '- '/)

;;see how this function version creates the code that we need
;;a macro would expand the code to something similar and then run it
(defun my-in (obj &rest choices)
  (let ((insym (gensym))) ;a guaranteed unique symbol
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (x) `(eql ,insym ,x))
		     choices)))))

					;See It expands it out - it's also just a  
(my-in '- '* '+ '- '/)
;;as an experiment i changed the ,x to x and you can see that the equality
;;is a constant in the second scenario, a constant that 


					;in this case the or function actually gets called and works like the identity function!
					;see how when it's created the identity function gets called
(defun my-in2 (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       ,(identity (mapcar #'(lambda (x) `(eql ,x ,insym))
			  choices)))))

(my-in2 '- '+ '* '- '^)

					;C-c RET macro expansion
(macroexpand-1 '(in '- '* '+ '- '/)); see how it draws it out

;;mapcar messing and seeing what macar does
(mapcar #'(lambda (x) (+ x 100))
	'(1 2 3 4 5));(101 102 103 104 105)

					;now look what happens when we put it into code, it's almost like
					;we can make the code if we could figure out a way to change the variables
(mapcar #'(lambda (x) `(+ x 100))
	'(1 2 3 4 5)); ((+ X 100) (+ X 100) (+ X 100) (+ X 100) (+ X 100))

;;more examples of in to see what actually happens
;;base case for roughly what happens - we know it creates in code a massive or statement 
(defmacro in-1 (obj &rest choices)
  `(or ,@(mapcar #'(lambda (x) `(eql ,obj ,x))
		 choices)))
(macroexpand-1 '(in-1 '- '+ '- '*))
					;(OR (EQL '- '+) (EQL '- '-) (EQL '- '*))

;;comma missing in the `(eql ,obj ,x) - so this doesn't works
					;(defmacro in-1a (obj &rest choices)
					;  `(or ,@(mapcar #'(lambda (x) '(eql ,obj ,x))
					;		 choices)))

(defmacro in-2 (obj &rest choices)
  `(or (mapcar #'(lambda (x) `(eql ,x ,obj))
	       ,@choices)))
(macroexpand-1 '(in-2 '- '+ '- '*)) ;;says + is not of type list
					;(OR (MAPCAR #'(LAMBDA (X) `(EQL ,X ,OBJ)) '+ '- '*))

;;play with this a bit before noting important plays
(defmacro in-3 (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,(mapcar #'(lambda (x) `(eql ,insym ,x))
		    choices)))))
					;extra set of paranthesis but otherwise fine
(pprint (macroexpand-1 '(in-3 '- '+ '* '-)))
					;(LET ((#:G962 '-))
					;  (OR ((EQL #:G962 '+) (EQL #:G962 '*) (EQL #:G962 '-))))

(defmacro in-4 (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       `(or ,(mapcar #'(lambda (c) `(eql ,c ,insym)); this one is needed since we want to create this code
		     ,choices)))))
(pprint (macroexpand-1 '(in-4 '- '+ '* '-)))
					;(LET ((#:G1011 '-))
					;  `(OR ,(MAPCAR #'(LAMBDA (C) `(EQL ,C ,INSYM)) ('+ '* '-))))

;;just move the comma around and adjust 
(defmacro in-5 (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       ,(or (mapcar #'(lambda (x) `(eql ,x ,insym))
		    choices)))))
(pprint (macroexpand-1 '(in-5 '- '* '+ '-)))
					;(LET ((#:G1020 '-))
					;  ((EQL '* #:G1020) (EQL '+ #:G1020) (EQL '- #:G1020)))

(in-3 '- '& '+ '= '-)


;;from onlips - macro chapter
;;want something called nil! like from earlier
(defmacro nil! (var)
  (list 'setq var nil)) ;note nil evaluates to nilcc

;;the macro expansion is
(macroexpand-1 `(nil! x))
					;(setq x nil) is what it expands to during macroexpansion
					;say the expansion led to another macro, then that macro would eventually be
					;expanded again until the last ting is not a function
					;like russian doll with another doll inside of it
					;parser reads source code and setds output to the compiler, but the 
					;output of the parser consists of lists of lisp objects, with macros
					;you manipulate the program while it's in this intermediate form between
					;parser and compiler. it changes what hte compiler can see! 

;;backquote is special since it lets it look close to the actual expansion
(equalp `(a b c) '(a b c)); T
(equalp `(a b c) (list 'a 'b 'c)); T
(setq a 1 b 2 c 3 d 4)
;;backquote tells us to turn off the quoting
`(a ,b c ,d); (A 2 C 4)
(list 'a b 'c d); (A 2 C 4)

(nil! **x**); sets **x** to nil

					;comma can be deep in parans
`(a (,b c)); (A (2 C))
(list 'a (list b 'c)); (A (2 C))

					;can work with and within the quotes as well
`(a b ,c (' ,(+ a b c)) (+ a b) 'c '((,a ,b))); (A B 3 ('6) (+ A B) C '(( 1 2)))
`( a b '( ,(- a c)) (' ,(+ 100 c)) '(- a c) '(- ,a ,c) (- ,a ,c)); (A B '(-2) ('103) '(- A C) '(- 1 3) (- 1 3))


`(,a ,(+ b c)); (1 5)
(setf plus '+)
`(,a ,(funcall plus b c)); (1 5)
(list a (funcall plus b c)); (1 5)

(defmacro nil! (var)
  `(setq ,var nil))
(macroexpand-1 '(nil! a))

(nil! a); slime macroexpand-1 shortcut C-c C-m ---- or C-c M-m

;;make the use of it first
					;(mapcar #'(lambda (x)
					;	    (nif x 'p 'z 'n))
					;	'(0 2.5 -8))
;;with backquote
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0, zero)
     (-1, neg)))

					;now use it again
(nif -1 'a 'b 'c)

(mapcar #'(lambda (x)
	    (nif x 'positive 'z 'negative))
	'(0 2.5 -8 -0)); cool huh

;;without backquote - same thing but more confusing to see what it expands to
(defmacro nif (expr pos zero neg)
  (list 'case
	(list 'truncate (list 'signum expr))
	(list 1 pos)
	(list 0 zero)
	(list -1 neg)))

;; ,@ instead of inserting just the value of the expression, it splices it
;; define splice - means inserting while removing the outermost level of parantheses

(setq b '(1 2 3))
`(a ,b c); (A (1 2 3) C)
`(a ,@b c); (A 1 2 3 C)
;; ,@ must be within a sequence - error - `,@b since there is nowhere to splice the value of b to
;; object being spliced must be a list unless it occurs last
					;`(a ,@1) 
`(a ,@'1); (A . 1) but the `(a ,@'1 b) will cause an error

(when (> 2 1)
  (print `(do this))
  (print `(do that))
  1)

(defmacro our-when (test &body body)
  `(if ,test
       (progn
	 ,@body)))

(our-when2 (> 2 1)
	   (print 'a)
	   (print 'b)
	   1)

(defmacro our-when2 (test &body body)
  `(cond (,test
	  ,@body)))

(our-when2 (> 2 1)
	   (print 'a)
	   (print 'b)
	   1)

;;comma-at is splices a list like so where b = '(1 2 3)
`(a ,@b c); (A 1 2 3 C)
(cons 'a (append b (list 'c))); (A 1 2 3 C)
;;comma-@ ,@ exists only to make it easier to read then the second thing

;;remember member - we have to explicitly define the test for eq
(member 'a b :test #'eq)

;;how to write a simple macro
					; call: (memq x choices)
					;expansion: (member x choices :test #'eq) 

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(memq '2 b)

;;with while
					;(while hungry
					;  (stare intently)
					;  (meow)
					;  (rub-against-leg))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(mac (while (able) (laugh)))
;;trick to setting a variable with the list returned by macroexpand-1 
(setq *expr1* (macroexpand-1 '(memq 'a '(a b c))))
					;(setq *expr2* (mac '(memq 'a '(a b c)))) ; doesn't work since pprint doesn't return what we'd e

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;now back to ansii commmon lisp with a bacground in what ` does and what  , does in a macro `(some-symbol ,@(the-value-returned-by-this-expression-spliced))

(defmacro my-in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj)) ;remember the ` means just create code
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

;;Queston and Answer for the above macro
					;q: but why do we need the , in the second `(eql ,insym ,c) when we already have a backwards quote `(let....) near the start of the definition?
					;a: we need a , per ` so the second one has a `(eql .....) and we need the value of ,insym and the value of ,c not just the symbol 'C)

					;q: well why don't we need a ,choices? like why can we just leave it to have choices (without comma)
					;a: the word choices is withing a set of backquote `(let....) and a comma ,@(mapcar....choices)

					;q: why do we have a ,@ before the mapcar the ,@(mapcar ...)
					;a: remember our goal above is to have an expression like below
(or (eql '+ '-)
    (eql '+ '*)
    (eql '+ '+)
    (eql '+ '+))
					;if we just left the mapcar we'd have the above expression but surrounded by one more pair of parantheses
((or (eql '+ '-)
     (eql '+ '*)
     (eql '+ '+)
     (eql '+ '+)))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
	    (mapcar #'(lambda (expr) `(,(incf key) ,expr))
		    exprs))))

(random-choice 'a 'b 88 19)
;;expands to:
(case (random 4)
  (0 'a)
  (1 'b)
  (2 88)
  (3 19))

;;use
					;(with-gensym (x y z) 
					; (rest-of-code) )
;;equivalent to:
(let ((x (gensym))
      (y (gensym))
      (z (gensym)))
  (format t "rest of body using generated symbold ~a ~a ~a" x y z))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym)))
		 syms)
     ,@body))

;;lets do in with let 
(defmacro in-test1 (obj &rest choices)
  (with-gensyms (insym) ;using the with symbol here
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,c ,insym))
		     choices)))))

(in-test1 '+ '- '& '^); look the let created by the with-gensyms
					;(LET ((#:G1002 '+))
					;  (LET ((#:G1004 (EQL '- #:G1002)))



(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(avg 8 9 10); (/ (+ 8 9 10) 3) see how the 3 is already ready during compile time

					;normally we'd just make apply an apply function
(defun avg-func (&rest args)
  (/ (apply #'+ args) (length args)))

(avg-func 8 9 10); (when using the function the (length args) is calculated at runtime)

;;intentional variable capture

;;ie instead of this:
					;(let ((val (calculate-something)))
					;  (if val
					;      (1+ val) ;;ie do something with the value returned from the test condition
					;      0))

;;we can use this
					;(aif (calculate-something)
					;  (1+ it)
					;  0)

;;use variable caputure to see if a conditional returns something
;;if it returns someting we use that value and 
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;;;;Chapter 10 excercises chap 10 exercises
;;exercises 10-1
(let ((x 'a)
      (y 'b)
      (z '(c d)))
  (progn
    (print `(,z ,x z))
    (print `(x ,y ,@z))
    `((,@z ,x) z))) 
;;10-2
(defmacro my-if (test then else)
  `(cond ((,@test)
	  ,then)
	 (t
	  ,else)))
;;both would work the above has too many
(defmacro cif (test then else)
  `(cond
     (,test ,then)
     (t ,else)))


(my-if (> 1 1) (print "true") (print false))

;;exercise 10-3 
(let ((n 2))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 1)))

(defmacro nth-expr0 (n &rest exprs)
  `(case ,n 
     ,@(with-gensyms (i)
		     (let* ((i 0))
		       (mapcar #'(lambda (expr) `(,(incf i) ,expr))
			       exprs)))))

(defmacro nth-expr1 (n &rest exprs)
  `(case ,n 
     ,@(with-gensyms (i)
		     (progn
		       (setf i 0)  
		       (mapcar #'(lambda (expr) `(,(incf i) ,expr))
			       exprs)))))

(defmacro nth-expr(n &rest exprs)
  `(case ,n
     ,@(let ((c 0))
	    (mapcar #'(lambda (expr) `(,(incf c) ,expr))
		    exprs))))

					;doesn't work
(defmacro nth-expr2 (n &rest exprs)
  (with-gensyms (x)
    `(let ((,x 0))
       (case ,n
	 ,@(mapcar #'(lambda (expr) `(,(incf x) ,expr))
		   exprs)))))
					;also doesn't work - we get a x isn't a number thing
(defmacro nth-expr3 (n &rest exprs)
  (let ((x (gensym)))
    `(let ((,x 0))
       (case ,n
	 ,@(mapcar #'(lambda (expr) `(,(incf x) ,expr))
		   exprs)))))
;;also doesn't work       
(defmacro nth-expr4 (n &rest exprs)
  (let ((x (gensym)))
    `(case ,n
       (let ((,x 0))
	 ,@(mapcar #'(lambda (expr) `(,(incf x) ,expr))
		   exprs)))))      
;;also doesn't work but can see why now
(defmacro nth-expr5 (n &rest exprs)
  (let ((i (gensym)))
    `(let ((,i 0))
       (case ,n
	 ,@(mapcar #'(lambda (expr) `((incf ,i) ,expr))
		   exprs)))))
					;still weird
(defmacro nth-expr6 (n &rest exprs)
  (let* ((i (gensym))
	 (i 0))
    `(case ,n
       ,@(mapcar #'(lambda (expr) `(,(incf i) ,expr))
		 exprs))))


					;(setf c "test"); and replace below 'c with c and it will still work
(nth-expr6 2 '(a b c) 2 'c 'asdf #'+ (+ 2 2 1 (/ 2 9)))

;;make a recursive version of ntimes
;;does not work wtih the labels one because i used labels wrong
(defmacro ntimes-r2 (n &rest body)
  (with-gensyms(gn grec)
    `(let ((,gn ,n))
       (labels (,grec (c)
		      (when (<= c ,gn)
			,@body
			(,grec (+ 1 c))))
	 (,grec 0)))))

(let ((v 5))
  (ntimes-r2 v (princ ".")))

(defmacro ntimes-rec (n &rest body)
  (with-gensyms (gn grec)
    `(let ((,gn ,n))
       (labels ((,grec (i)
		  (when (< i ,gn)
		    ,@body
		    (,grec (1+ i)))))
	 (,grec 0)))))

(defmacro ntimes-r2 (n &body body)
  (with-gensyms (gn grec)
    `(let ((,gn ,n))
       (labels ((,grec (i)
		  (when (< i ,gn)
		    ,@body
		    (,grec (1+ i)))))
	 (,grec 0)))))

;;exercise 10-5


(defmacro n-of (n expr)
  (with-gensyms (gn grec)
    `(let ((,gn ,n))
       (labels ((,grec (start stop)
		  (if (>= start stop)
		      nil
		      (cons ,expr (,grec (1+ start) stop)))))
	 (,grec 0 ,gn)))))

;;alternate way to do it using the accumulator and then just returning that
(defmacro n-of-alternate1 (n expr)
  (with-gensyms (gn grec gacc)
    `(let ((,gn ,n))
       (labels ((,grec (i j acc)
		  (if (>= i j)
		      (nreverse acc)
		      (,grec (1+ i) j (cons ,expr acc)))))
	 (,grec 0 ,gn nil)))))
;;would generate a bunch of (cons (incf i) acc) where acc is (cons (incf i) (cons (incf i) (cons (incf i) nil)))
;;another cool way to do it using do
(defmacro n-of-alternate2 (n expr)
  (with-gensyms (gn gi gacc)
    `()))



;;use
(let ((n 4)
      (i 0))
  (n-of n (incf i)))

;;want something like this as the expansion
(let ((n 4)
      (i 0))
  (cons (incf i) (cons (incf i) (cons (incf i) (cons (incf i) nil)))))



;;Ex 10-6; exercise 10-6 - macro that takes a body of code and protects vars
;;want to make a 
					;(let ((v1 v1)
					;      (v2 v2)
					;      (v3 v3))
					; ,@body        ;in this scenario the manipulaiton that occurs in 
(defmacro protect-vars (vars &rest body)
  `(let ,(mapcar #'(lambda (v) `(,v ,v))
		 vars)
     ,@body))
;;with the progn and lets see it without it
(let ((a 10)
      (b 20)
      (c 30))
  (progn 
    (protect-vars2 (a c)
		   (progn
		     (setf a -1
			   b -1
			   c -1)
		     (print a)
		     (print b)
		     (print c)))
    (print a)
    (print b)
    (print c)))

;;another way to do this which is really cool
(defmacro protect-vars2 (params &rest body)
  `((lambda ,params ,@body) ,@params))

;;10-7 why is this a bad def of push
(defmacro push-wrong (obj lst)
  `(setf ,lst (cons ,obj ,lst)))
;;see how this push will be ,lst and ,lst twice so suffers from 
;;multiple evaluation problem

;; check the real push
(defun test-push ()
  (let ((a (make-array 3))
        (i 0))
    (setf (aref a 0) (list 0)
          (aref a 1) (list 1)
          (aref a 2) (list 2))
    (push 4 (aref a (incf i)))
    (format t "~A ~A ~A~%" (aref a 0) (aref a 1) (aref a 2))))

;; check the wrong push
(defun test-push-wrong ()
  (let ((a (make-array 3))
        (i 0))
    (setf (aref a 0) (list 0)
          (aref a 1) (list 1)
          (aref a 2) (list 2))
    (push-wrong 4 (aref a (incf i)))
    (format t "~A ~A ~A~%" (aref a 0) (aref a 1) (aref a 2))))

(test-push)
(test-push-wrong)

;;;;CLOS 11

;;with structures
(defstruct rect-st
  height width)

(defstruct circle-st 
  radius)

(defun area-st (x)
  (cond ((rect-st-p x)
	 (* (rect-st-height x) (rect-st-width x)))
	((circle-st-p x)
	 (* pi (expt (circle-st-radius x) 2)))))
					;use
(let ((r (make-rect-st))
      (c (make-circle-st)))
  (setf (rect-st-height r) 2
	(rect-st-width r) 3
	(circle-st-radius c) 10)
  (area-st r)
  (area-st c))

;;doing it with classes
(defclass rectangle () ;this space is for the superclass
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle)) ;the first arg is a x of type rectangle? yes it looks at the type of arg and invokes the corresponding method
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(let ((r (make-instance 'rectangle))
      (c (make-instance 'circle)))
  (setf (slot-value r 'height) 2
	(slot-value r 'width) 3
	(slot-value c 'radius) 10)
  (print (area r))
  (print (area c)))

;;adding a colored and colered-circle

(defclass colored ()
  (color)) ;has a color

(defclass colored-circle (circle colored) ;inherits for circle and colored
  ())

;;lets make an instance of colored-circle and see what happens
(let ((c (make-instance 'circle))
      (myred (make-instance 'colored))
      (cc (make-instance 'colored-circle)))
  (setf (slot-value cc 'color) 'red
	(slot-value cc 'radius) 2)
  (print (area cc))
  (print (slot-value cc 'color)))

;;;11.2 classes and instances

(defclass circle ()
  (radius center))

;;try it here
(setf c (make-instance 'circle)) ;#<CIRCLE {1006150143}>
					;and to set it's values
(setf (slot-value c 'radius) 1);

;;the third arg is a list of slot definitions
;;slot-def is a list of a name followed by properties
;;properties are the :accessor (look like keyword arguments)
(defclass circle () 
  ((radius :accessor circle-radius) ;could have used a :writer or :reader
   (center :accessor circle-center))) ;instead of just accessor which is both of the above

(setf c (make-instance 'circle)); so this makes the instance of it

;;setter
(setf (circle-radius c) 1)
;;getter
(circle-radius c)

;;specifying a :writer or :reader in
(defclass circle ()
  ((radius :accessor circle-radius
	   :initarg :radius             ;to give a default val for a slot
	   :initform 1)
   (center :accessor circle-center
	   :initarg :center             ;init name
	   :initform (cons 0 0))))      ;init value

(setf c (make-instance 'circle :radius 3))
(circle-radius c); 3
(circle-center c); (0 . 0) ;since we didn't specify anyting note how it's still got a value

					;note how :initargs takes precedence over :initforms
;;tabloid simulator
;;make all the instances share a slot by using :allocation :class
(defclass tabloid ()
  ((top-story :accessor tabloid-story
	      :allocation :class)));one per class
					;make a couple of tabloids
(setf daily-blab       (make-instance 'tabloid)
      unsolicited-mail (make-instance 'tabloid))
					;make a couple of stories
(setf (tabloid-story daily-blab) 'adultery-of-senator)
(tabloid-story unsolicited-mail) ; also the same

;;11.4 superclasses
(defclass graphic ()
  ((color   :accessor graphic-color   :initarg :color)
   (visible :accessor graphic-visible :initarg :visible
	    :initform t)))

;;inherits from circle and from graphics
(defclass screen-circle (circle graphic)
  ())

;;will have 4 slots
(graphic-color (make-instance 'screen-circle
			      :color 'red :radius 3)) 

(setf gc (make-instance 'screen-circle
			:color 'red :radius 3))
;;try out the circle-radius of gc as well as the circle-center

;;make a screen circle have a default color
(defclass screen-circle (circle graphic)
  ((color :initform 'purple))) ;didn't need to put graphic-color there - if i put graphic color it messes up, so what if two slots have the same name? how does it distinguis it? lets try!

;;when we do the above
(graphic-color (make-instance 'screen-circle)) ; purple is the color we get
(setf sc (make-instance 'screen-circle))
(graphic-visible sc); T just like we assumed

;;As a test see what would happen if you made another slot called color
;;and then only initialized one part of it?
(defclass fake-graphic ()
  ((color :accessor fake-graphic-color :initarg :color)))
(defclass fake-screen-circle (fake-graphic circle graphic)
  ((color :initform 'red)))

;;see how both of the slots got the same property
(fake-graphic-color (make-instance 'fake-screen-circle)); red!
(graphic-color (make-instance 'fake-screen-circle)); ALSO red!

;;say i want to specify which one of the colors i want to have
(defclass fake-graphic2 ()
  ((color :accessor fake-graphic-color2 :initarg :color)))
(defclass fake-screen-circle2 (fake-graphic2 circle graphic)
  ((fake-graphic-color2 :initform 'blue)))
					;this just causes an error;(fake-graphic-color2 (make-instance 'fake-screen-circle2))

;;;11.5 precedence
;;with multiple superclasses here is how we find the direction - order matters

;;the basclass is t -> standard-object (that class is the one where the second arg to defclass is () i.e where it looks like it's inheriting from nothing)
;;left to right as they appear in the calls
(defclass sculpture () (height width depth))

(defclass statue (sculpture) (subject))

(defclass metalwork () (metal-type))

(defclass casting (metalwork) ())

(defclass cast-statue (statue casting) ())


;;precedence rules
;;1 start at the bottom

;;2 walk upwards taking the leftmost unexplored branch

;;3 if you enter a node and see anohter path entering same node from right
;;then instead of entering the node, retrace your steps until get to a node
;;with an unexpored path leading upwards. go back to step 2

;;4 when you get to t you're done the order you entered a node is the order
;;each node is determined in 

;;;think of it like a recursive function left then right 

(defmethod combine (x y)
  (list x y))

(combine 'a 'b)

(defclass stuff () 
  ((name :accessor name :initarg :name)))

(defclass ice-cream (stuff) ())

(defclass topping (stuff) ())


;;look at the signature needs 2 objs ice-cream and topping
(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
	  (name ic)
	  (name top)))

;;so with an ice-cream and a topping it picks the right thing
(combine (make-instance 'ice-cream :name 'chocolate)
	 (make-instance 'topping :name 'treacle))

;;so with an ice-cream and anything else the below question will be called
(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A."
	  (name ic)
	  x))

;;with one arg as ice-cream and one anyting helse the above gets called
(combine (make-instance 'ice-cream :name 'calm)
	 'reluctance); "CALM ice-cream with RELUCTANCE."

;;methods can also have specialized on types
(defmethod combine ((x number) (y number))
  (+ x y))

(combine 4 3)

;;or methods can be specializedon individual objs as determined by eql:
(defmethod combine ((x (eql 'powder))
		    (y (eql 'spark)))
  'boom)

(combine 'powder 'spark); 'boom

;;if we use another method with the qualifiers and specializations
;;it overwrites the original one
(defmethod combine ((x (eql 'powder)) (y (eql 'spark)))
  'kaboom)

(combine 'powder 'spark); see it's kaboom this time 

;;11.7 Auxiliary methods
					;before - but first do this 
					;after - let you wrap up a method ps do this

					;putting quantifying keyword after the method name
(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

					;then call speak with with instakce of speaker just prints the second arg
(speak (make-instance 'speaker)
       "I'm hungry")

;;make sublcass intellectual, which wraps before- and after- methods
;;around the speak method
(defclass intellectual (speaker) ())

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string)
  (princ " in some sense"))

;;now create a subclass of speakers that always have the last
;;and the first word (lol)
(speak (make-instance 'intellectual)
       "I'm hungry"); Perhaps I'm hungry in some sense

;;note all the before- and after- methods get called
;;the following shoould make the before show up before the intellectual?
(defmethod speak :before ((s speaker) string)
  (princ "I think "))

					;no they actually end up in the middle of the call, wrapped around like a sandwich
(speak (make-instance 'intellectual)
       "I'm hungry"); Perhaps I think I'm hungry in some cases

;;can use call-next-method and next-method-p to see if we want to let the 
;;around method return the last method
(defclass courtier (speaker) ())

(defmethod speak :around ((c courtier) string)
  (format t "Does the King believe that ~A" string)
  (if (eql (read) 'yes)
      (if (next-method-p) 
	  (call-next-method))
      (format t "Indeed, it is a preposterous idea. ~%"))
  'bow)

					;when the first arg to speak is an instance of the courtier class, the
;;courtier's tongue is now guarded by the around method

					;try the follwoing when you say anything other than yes, it'll print "Indeed this is a preposterous idea"
					;but change it to yes and it calls the before for speak (because of the
					;call-next-method in there
					;(speak '(make-instance 'courtier) "kings will last")

;;11.8 Method Combination

					;(defun price (&res args)
					;  (+ (apply <most-specific-primary-method> args)
					;     .
					;     .
					;     .
					;     (apply <least-specific-primary-method args)))

					;the :method-combination to defgeneric

(defgeneric price (x)
  (:method-combination +))

(defclass jacket () ())

(defclass trousers () ())

(defclass suit (jacket trousers) ())

(defmethod price + ((jk jacket)) 350)

(defmethod price + ((tr trousers)) 200)

(price (make-instance 'suit)); 550

;;11.9 Encapsulation
(defpackage "CTR"
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))

(in-package ctr)

(defclass counter () ((state :initform 0)))

(defmethod increment ((c counter))
  (incf (slot-value c 'state)))

(defmethod clear ((c counter))
  (setf (slot-value c 'state) 0))

(defmethod val ((c counter))
  (let (( v (slot-value c 'state)))
    v))

(unintern 'state)


;;;Chapter 11 exercises - exercise 11 - exer 11 - questions chapter 11 - chap 11 questions
;;11-1
;;make a def for rect and circle that doesn't use slot value
;;make it have an area

(defclass rectangle ()
  ((height :accessor height
	   :initarg :height
	   :initform 1)
   (width  :accessor width
	   :initarg :width
	   :initform 1)))

(defclass circle ()
  ((radius :accessor radius
	   :initarg :radius 
	   :initform 1)
   (center :accessor center
	   :initarg :center
	   :initform (cons 0 0))))

(defmethod area ((x circle))
  (* pi (expt (radius x) 2)))

(defmethod area ((x rectangle))
  (* (width x) (height x)))

(let ((r (make-instance 'rectangle :height 3 :width 10))
      (c (make-instance 'circle :radius pi :center (cons 1 1))))
  (progn
    (format t "Area of circle ~A~%" (area c))
    (format t "Area of rectangle ~A~%" (area r))
    (setf (cdr (center c)) 3)
    (setf (car (center c)) 12)
    (format t "Center of circle is ~A" (center c))))

;;11.2 rewrite sphere and point as classes and 

(defclass point1 ()
  ((x :accessor x
      :initarg :x
      :initform 0)
   (y :accessor y
      :initarg :y
      :initform 0)
   (z :accessor z
      :initarg :z
      :initform 0)))

(defclass surface ()
  ((color :accessor surface-color
	  :initarg :color
	  :initform 'blue)))

(defclass sphere (surface)
  ((radius :accessor sphere-radius
	   :initarg :radius
	   :initform 1)
   (center :accessor sphere-center
	   :initarg :center
	   :initform (make-instance 'point1 :x 0 :y 0 :z 0))))

(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere 
			  :radius r
			  :center (make-instance 'point1 :x x :y y :z z)
			  :color c)))
    (push s *world*)
    s))

(defmethod intersect ( (s sphere) (pt point1) xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-instance 'point1 :x (+ (x pt) (* n xr))
		       :y (+ (y pt) (* n yr))
		       :z (+ (z pt) (* n zr))))))

(defmethod normal ((s sphere) (pt point1))
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))

(defmethod ray-test3 (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "sphere-test2.png") res))


;;11-3 - see notebook - use formula 
					;a : c d e f h g
;;11-3-a - a c d e f g h
;;11-3-b - b d e f g h c

;;11-4 
					;precedence:      takes object and ret precedence list, classes ordered
					;from most specific to least specific
					;methods:         takes a generic function and rets list of all it's methods
					;specializations: takes a method and ret a list of specializations of
					;the params. Each elem of the ret list will be either
					;a class or a list of the form (eql x) or t (indicating param is unspecialized)

(defun most-spec-app-method (fn args)
  (let ((classlst (mapcar #'precedence args)))
    (dolist (meth (methods fn))
      (if (do ((i 0 (1+ i))
	       (spec (specializations meth) (cdr spec)))
	      ((not spec) t)
	    (or (member (car spec) (nth i classlist) :test #'equal)
		(return)))
	  (return-from most-spec-app-method meth)))))



;;;;Chapter 12

;;shared structure - a list being part of another
(setf *part* (list 'b 'c)); 

(setf *whole* (cons 'a *part*));

(tailp *part* *whole*); T
;;the first cons is part of (in fact, is the cdr of) the second. 

(defun our-tailp (x y)
  (or (eql x y)
      (and (consp y)
	   (our-tailp x (cdr y)))))

					;say we used 
(eql '(b c) '(b c)); NIL

(eql *part* (cdr *whole*)); T ;because of def of eql

(setf *part* (list 'b 'c)
      *whole1* (cons 1 *part*)
      *whole2* (cons 2 *part*))
;;these two lists are now part of the other without either being a tail
;; of the other
(tailp *part* *whole1*); T
(tailp *part* *whole2*); T
(tailp *whole1* *whole2*); nil
(tailp *whole2* *whole1*); nil

;;top-level list structure; 
(list 'a '(b c) 'd); so top level would be 'A '(B C) and 'D - where `(B C) is like one element - ie it refers to the conses that make up a list not any conses that make up it's elements

;;two lists contain the same list as an element
(setf element (list 'a 'b)
      holds1 (list 1 element 2)
      holds2 (list element 3))

(eql (cadr holds1) element); T
(tailp element holds1); nil
(tailp element holds2); nil

;;second element in holds1 shares structre with holds2, 
;;BUT holds1 and holds2 do not share structure as lists.
;;they only share structure if they share top-level list structure

;;two kinds of copying

;;this one will avoid sharing top-level list structure with the orig list
;;but see if (car lst) is another list then it just gets consed
(defun our-copy-list (lst)
  (if (null lst)
      nil
      (cons (car lst) (our-copy-list (cdr lst)))))

;;to ret a list that doesn't share even a tree structure with the orig lst
;;look this won't have anything in commonon since each element of the list
;;gets copied 
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
	    (our-copy-tree (cdr tr)))))
;;call both and see the difference see notbook for drawings 
(defparameter *x* '(x a d))
(defparameter *x1* `(x a ,*part* d))

(defparameter *x2* (our-copy-tree *x*))
(defparameter *x3* (our-copy-tree *x*))

(print *part*); (B C)
					;*x4* will now only not share top-level structure
(defparameter *x4* (our-copy-list *x1*)); (X A (B C) D)
(setf (car *part*) 1); (1 C)
;;see when we change part how it changes 
(print *x4*); (X A (1 C) D)
;;the same thing with our-copy-tree thing will change

(setf whole (list 'a 'b 'c)
      tail (cdr whole))

(setf (second tail) 'e)
					;now whole is (A B E)
					;and tail is (B E)
;;both tail and whole modify the same cons

;;12.3 Queue - shared structure to rep a queue
;;fifo queue; insert and retrieve from a diferent end

(defun make-queue() (cons nil nil))

(defun enqueue(obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
	    (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(setf q1 (make-queue)); (nil nil)

(progn (enqueue 'a q1); ((A) A)
       (enqueue 'b q1); ((A B) B)
       (enqueue 'c q1)); ((A B C) C)

					;(setf (car q1) (list 'a)); so if (nil . nil) then goes  (A . nil)
					;(setf (cdr q1) `(A . nil)

(setf q1 (make-queue))

					;see what happens when manually going through steps of enqueue to understand
(setf q1 (make-queue)) ; (cons nil nil) ; (nil . nil)
(setf (car q1) (list 'a)); (A . nil); retruns (A)
;;;SEE HERE the first time i did this i didn't understand that car q (think of it as a cons box combo [][] representing [car][cdr])
					; i had this
					; (setf (cdr q1) (list 'a)); see how this would mean the cdr points to a new 
;;cons instead of both the car and cons pointing to the same element...which was the point of the whole chapter
					;q1 ([]  . [])
					;     |     |
					;     v     v
					;  (a.nil) (a.nil)
					;
					;instead we want this
					;q1 ([]  . [])
					;     |     |
					;     v     v
					;  (a.nil) (a.nil)


					;instead if we do this it's like both the "front" and "back" (the car and the cdr of q) will point to the same block
(setf (cdr q1) (setf (car q1) (list 'a))); (A)


(print q1); ((A) A); where does the second bracket go? 
(car q1); (A)
(cdr q1); (A)
(eql (car q1) (cdr q1)); T

(cons '(a) '(a)); ((A) A) ;that is just how it's displayed

(setf q (make-queue))
(enqueue 'a q)
(print q); ((A) A)
(car q); (A)
(cdr q); (A) ; cdr returns a list


;;step 2a adding something to the queue, where we make a cons and change "back"
(cdr (cdr q1)); nil 
(cddr q1); nil 
(setf (cdr (cdr q1)) (list 'b)); (B) ; makes the "back"'s (the cdr of q) point to the last object (list 'b) in this case
(cdr (cdr q1)); (B); make the 
(car q1); (A)
(cdr q1); (A B)
(print q1); ((A) A B) 
;;;step 2b 
(setf (cdr q1) (cdr (cdr q1)));ret (B) ;now it makes the "back" point to the last element that we just added  (so now "front" now points to A and cdr now points to B...also a's cdr points to b and b's cdr now points to nil)
(print q1); ((A) B) ; NO LONGER THE CASE SEE THE ABOVE CONFUSION AN RE-RUN THESE; KEPT HERE BECAUSE THEY WERE A GOOD REMINDER TO ALWAYS REMEMBER THE 
(car q1); (A) ;NOTE................
(cdr q1); (B)

(setf q (make-queue))
(enqueue 'a q); (A)
(enqueue 'b q); (A B)
(print q); ((A B) B)
(car q); (A B) ;NOTE...............
(cdr q); (B)

;;hmm A queue is a pair of a list, and the last cons in the same list;
;;call these "front" (pair of list) and "back" (last cons in that same list)
;;to retrieve an element from the queue just pop "front". 
;;to add an element, we create a new cons, make it the cdr of the back (the last cons of the same list)
;;and then set "back" to it. 

;;so if did this
(setf q (make-queue))
(enqueue 'a q)
(enqueue 'b q); now we're at the above step 2 completion
;;say we do this
(enqueue 'c q); first get the last element - go through "back" (the cdr of q) and get it's cdr - this shold be nil (see drawing in notebook) - and make that the new element that points to nil
					;now make the cdr of q, ie "back" point to the last element

;;;12.4 destructive functions
;;there are functions that are allowed to modify list but might not do anything
(setf lst '(a r a b i a))
(delete 'a lst); returns (R B I)
(print lst); (A R B I)

;;if you want to keep sideeffects you have to setf them

(setf lst (delete 'a lst)); ps C-x o (swtich between open split screen buffer views)

;;desstructive version of append nconc
(defun nconc2 (x y)
  (cond 
    ((consp x)
     (setf (cdr (last x)) y) ;go to the last cons cell and make it point to y ;; [][]->[]....
     x);return x ie the first list which is both lists
    (t
     y))); just return the second list if x isn't a list
;;or like this to make it more clear
(defun nconc2 (x y)
  (if (consp x)
      (progn
	(setf (cdr (last x)) y); set the last element of the first list's cdr to point to the second list
	x); return x ie the first list which now combines (destructively) both lists
      y));;but why return y if x isn't a list

;;mapcan is like mapcar, but splices together the values returned by the 
;;functions (which must be lists) using nconc:
(mapcan #'list 
	'(a b c)
	'(1 2 3 4))
					;(A 1 B 2 C 3)

;;here's how the function might be defined
(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

(our-mapcan #'list
	    '(a b c)
	    '(1 2 3 4))

(mapcar #'list '(a b c) '(1 2 3 4)); ((A 1) (B 2) (C 3))
;;see how mapcan will be 
(mapcan #'list '(a b c) '(1 2 3 4)); (A 1 B 2 C 3)


;;with apply
(apply #'mapcar #'list '((a b c) (1 2 3 4))); ((A 1) (B 2) (C 3))
;;mapcan splices the results, also it is destructive to orig lists
(apply #'mapcan #'list '((a b c) (1 2 3 4))); (A 1 B 2 C 3)

(nconc '(A 1) '(B 2) '(C 3)); (A 1 B 2 C 3)
;;but if we do it with the result of the mapcar
(nconc '((A 1) (B 2) (C 3))); get the same result; ((A 1) (B 2) (C 3))
;;so that's why look what happens when we use apply
(apply #'nconc '((A 1) (B 2) (C 3))); (A 1 B 2 C 3)

;;so why doesn't our-mapcan work? wait it does; had somethng stupid in the param list
(our-mapcan #'list
	    '(a b c)
	    '(1 2 3 4)); (A 1 B 2 C 3)


;;if children retus a list of someone's children 
(defun children (c)
  (cond ((equalp c 'haiderali)
	 '(anwer shakut anis nasreen))
	((equalp c 'anwer)
	 '(ameen rabia))
	((equalp c 'anis)
	 '(nazia))
	((equalp c 'ameen)
	 '(georgie moosa anwerbear francis charlie))
	(t
	 nil)))

(defun grandchildren (x)
  (mapcan #'(lambda (c)
	      (copy-list (children c)))
	  (children x)))

(grandchildren 'haiderali)
(grandchildren 'anwer)
(grandchildren 'nazia)

;;nondestructive variant of mapcan might be like so:
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts))) 

(mappend #'list 
	 '(a b c)
	 '(1 2 3 4));(A 1 B 2 C 3) but non destructive since the 

(our-mapcan #'list 
	    '(a b c)
	    '(1 2 3 4)); (A 1 B 2 C 3)

;;or using mappend we could leave out the copy-list in grandchildren
(defun grandchildren (x)
  (mappend #'children (children x)))

;;binary search tree destrcutive
(defun bst-insert! (obj bst <)
  (if (null bst)
      (make-node :elem obj)
      (progn (bsti obj bst <)
	     bst)))

;;see how this one inserts an element right in the tree itself
;;unlike the other def this one modifies the orig tree
(defun bsti (obj bst <)
  (let ((elem (node-elem bst)))
    (if (eql obj elem)
	bst
	(if (funcall < obj elem)
	    (let (( l (node-l bst)))
	      (if l
		  (bsti obj l <)
		  (setf (node-l bst)
			(make-node :elem obj))))
	    (let ((r (node-r bst)))
	      (if r
		  (bsti obj r <)
		  (setf (node-r bst)
			(make-node :elem obj))))))))

(defun bst-insert1! (obj bst <)
  (cond 
    ((null bst)
     (make-node :elem obj))
    (t
     (bsti1 obj bst <)
     bst)))

(defun bsti1 (obj bst <)
  (let ((elem (node-elem bst)))
    (cond
      ((eql obj elem)
       bst)
      ((funcall #'< obj elem)
       (let ((l (node-l bst)))
	 (if l
	     (bsti1 obj l <)
	     (setf (node-l bst) (make-node :elem obj)))))
      (t
       (let ((r (node-r bst)))
	 (if r
	     (bsti1 obj r <)
	     (setf (node-r bst) (make-node :elem obj))))))))

;;bst definitionin in book is broken use the one from the link
(defun bst-delete (obj bst <)
  (cond
    ((null bst) nil)
    ((eql obj (node-elem bst))
     (del-root bst))
    (t
     (if (funcall < obj (node-elem bst))
	 (setf (node-l bst) (bst-delete obj (node-l bst) <))
	 (setf (node-r bst) (bst-delete obj (node-r bst) <)))
     bst)))

					;corrected bst-delete
(defun bst-delete (obj bst <)
  (if (null bst)
      nil ;returns nothing
      (if (eql (node-elem bst) obj)
	  (del-root bst); returns a node
	  (progn 
	    (if (funcall < obj (node-elem bst))
		(setf (node-l bst) (bst-delete obj (node-l bst) <)) ;set the left side to be the result of del-root or nil
		(setf (node-r bst) (bst-delete obj (node-r bst) <))); otherwsie look for obj on the right side and then set the rght side to be the result of delete or nil
	    bst))))

(defun del-root (bst)
  (let ((l (node-l bst))
	(r (node-r bst)))
    (cond 
      ((null l) r)
      ((null r) l)
      (t        (if (zerop (random 2))
		    (cutnext r bst nil)
		    (cutprev l bst nil))))))

					;A deleted internal node needs to be replaced either by the maximal node in the left subtree
					;or by the minimal node in the right subtree, and your function does not do this."
					;draw out different scenarios
(defun cutnext (bst root prev)
  (if (node-l bst)
      (cutnext (node-l bst) root bst)                 ;traverse tree until you get to left most (and we're starting from the right side so we get leftmost on right side)
      (if prev
	  (progn
	    (setf (node-elem root) (node-elem bst)    ;root still has pointers that point to the right thing, we just need to change the element that root points to
		  (node-l prev)    (node-r bst))      ;think of scenario where a node deepr than bst but on the bst's right - for example if i added (7 100 9 20 15 30) - we would want the 9 to replace the 7 being deleted as teh root of the tree ;think about if the leftmost element on the right side has a right leaf. that leaf will need to be the left side of the "prev" (ie the current bst node's parent) since the leaf (which could be a bst itself) will be less than the parent; see drawings
	    root)                                     ;without this, we have no 
	  (progn
	    (setf (node-l bst) (node-l root))         ;case where 3 node bst (parent and 2 children) and parent gets deleted. child on right needs to move up and have it's node-l point to the child on the left (just like the parent)
	    bst))))                                    ;no more references to root after this

(defun cutprev (bst root prev)
  (if (node-r bst)
      (cutprev (node-r bst) root bst)
      (if prev
	  (progn
	    (setf (node-elem root) (node-elem bst)
		  (node-r prev)    (node-l bst))      ;for example if i added (5 2 4 3 3.2 3.5) - in this case say we're deleteting the 5 we want it replaced 4 and we want node-r of 2 to point to the 3.2
	    root)
	  (progn
	    (setf (node-r bst) (node-r root))
	    bst))))


(defparameter *bst* nil)

(dolist (x '(7 2 9 8 4 1 5 12))
  (setf *bst* (bst-insert1! x *bst* #'<)))

					;leftmost on right side is 8 and rightmost on left side is 5
(bst-delete 7 *bst* #'<); can return #<5> or #<8> depending on what random says

					;go through the tree and see (it works :)) (node-l (node-r *bst*))

(bst-delete 9 *bst* #'<); can be either #<8> or #<12>

(node-r *bst*); this time it's 12 that is the node-r
(node-l (node-r *bst*)); and 8 that is on it's left


;;doubly linked list

(defstruct (dl (:print-function print-dl))
  prev next data)

(defun print-dl (dl stream depth)
  (declare (ignore depth))
  (format stream "#<DL ~A>" (dl->list dl)))

;;this version would print from both sides 
(defun dl->list2 (lst)
  (if (dl-p lst) ;cool right all structures seem to have this, must be a way to define this - how does it just know if something is eql to an obj type
      (if (dl-prev lst)
	  (progn
	    (cons (dl->list2 (dl-prev lst))
		  (cons (dl-data lst) 
			(dl->list2 (dl-next lst)))))
	  (cons (dl-data lst)
		(dl->list2 (dl-next lst)))))); see as soon as this happens you can see why 

(defun dl->listbackwards (lst)
  (reverse (dl->listbackwardsh lst)))
    
(defun dl->listbackwardsh (lst)
  (if (dl-p lst)
      (cons (dl-data lst)
	    (dl->listbackwardsh (dl-prev lst)))))

(defun dl->list (lst)
  (if (dl-p lst)
      (cons (dl-data lst)
	    (dl->list (dl-next lst)))))

(defun dl->list-all (lst)
  (if (dl-prev lst)
      (dl->listall (dl-prev lst))
      (dl->list lst)))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc)))))) ;when starting
    (rec x nil)))
;;inserts to the start of a ist
(defun dl-insert (x lst)
  (let ((elem (make-dl :data x :next lst)))
    (when (dl-p lst)
      (if (dl-prev lst)
	  (setf (dl-prev elem) (dl-prev lst)
		(dl-next (dl-prev lst)) elem))
      (setf (dl-prev lst) elem))
    elem))

;the initial value being nil means the first call will be to dl-insert with x and nil (which returns a newly created dl with x being the data value of the dl struct)
;the from-end means use it's created in the order you type it ie (dl-list '(a b c)) makes a dl like so (dl-insert a (dl-insert b (dl-insert (c nil))))
(defun dl-list (&rest args)
  (reduce #'dl-insert args ;reduce takes a function and an arg ; also the function must take 2 args exaclty
	  :from-end t :initial-value nil))

(defun dl-remove (lst)
  (if (dl-prev lst)
      (setf (dl-next (dl-prev lst)) (dl-next lst)))
  (if (dl-next lst)
      (setf (dl-prev (dl-next lst)) (dl-prev lst)))
  (dl-next lst))

(dl-list 'a 'b 'c)

(dl-insert 'a (dl-insert 'b (dl-insert 'c nil)))

;(reduce #'(lambda (a b c) (+ a b c)) '(1 1 1 1 1 1 1 1)) ; won't work function must take 2 args

(defvar *dl* (make-dl :data 3))
;;add some more elements to the doubly linked list
(setf (dl-next *dl*) (make-dl :data 5))
(setf (dl-next (dl-next *dl*)) (make-dl :data 15))
(setf (dl-prev *dl*) (make-dl :data 1))
(setf (dl-prev (dl-prev *dl*)) (make-dl :data -1))
;;or
(defvar *d1* nil)
(mapcar #'dl-insert 
	'(23 5 2 0 9)
	'(*d1* *d1* *d1* *d1* *d1*))


(flatten (dl->list2 *dl*));see how this would only work if data was an int and not a list of int

;;review of reduce
;;needs a function that takes 2 args
;;needs a list as the second thing to reduce
(reduce #'(lambda (a b)
		     (cons b a))
		 '(a b c d e f)
		 :initial-value nil); (F E D C B A)
;end 
(reduce #'(lambda (a b)
		     (cons b a))
		 '(a b c)
		 :initial-value 'lol
		 :from-end t); (((LOL . C) . B) . A)

(reduce #'(lambda (a b)
		     (progn
		       (print a)
		       (print b)
		       (print 'end)
		       (cons b a)))
		 '(a b c d e f)
		 :initial-value 'initval); shows that the first args are (initval a)

;see how with
(setf *dl* (dl-list 23 5 2 0 9))

(dl->list *dl*)
(dl->listbackwards (dl-next *dl*)); (23 5)
(dl->list-all (dl-next *dl*))
(dl-remove (dl-next *dl*)); has side effects
(setf dl (dl-list 'a 'b))

;;;12.7 cicular structure
(setf *print-circle* t)
(setf x (list 'a))
(progn (setf (cdr x) x) nil); a list that points to itself
(setf *print-circle* t)


(setf *print-circle* t)
(let ((a (make-array 1)))
  (setf (aref a 0) a));see how this points to itself an array that 
