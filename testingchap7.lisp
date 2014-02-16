
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








































