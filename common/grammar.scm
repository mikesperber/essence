; Start-separated grammars

; Guarantees:

; - the grammar symbols are numbers.
; - nonterminals and terminals are consecutive, respectively
; - the nonterminals come before the terminals
; - the start production is first

(define-record-type grammar
  (nonterminals
   terminals
   number-of-nonterminals
   number-of-symbols
   error start
   productions
   terminal-attribution)
  ())

(define (grammar-start-production grammar)
  (car (grammar-productions grammar)))

(define (terminal? symbol grammar)
  (>= symbol (grammar-number-of-nonterminals grammar)))

(define (nonterminal? symbol grammar)
  (< symbol (grammar-number-of-nonterminals grammar)))

; Productions are specialized and show up in the specialized output.
; Hence, they need equality and an external representation.

(define (make-production lhs rhs attribution)
  (vector lhs rhs attribution))
	  
(define (production-lhs p) (vector-ref p 0))
(define (production-rhs p) (vector-ref p 1))
(define (production-attribution p) (vector-ref p 2))

(define (attribution-arglist args)
  (let loop ((i (length args)) (l '()))
    (if (zero? i)
	l
	(loop (- i 1) (cons (concatenate-symbol "$" (number->string i)) l)))))

(define-syntax define-grammar
  (syntax-rules ()
    ((define-grammar grammar-name symbol-enum nts ts s rules)
     (define-grammar grammar-name symbol-enum nts ts s rules #f))
    ((define-grammar grammar-name symbol-enum
       (nonterminals ...)
       (terminals ...)
       start-symbol
       (((lhs rhs ...) expression) ...)
       terminal-attribution)
     (begin
       (define-enumeration symbol-enum
	 ($start nonterminals ... $error terminals ...))
       (define grammar-name
	 (grammar-maker (list (enum symbol-enum $start)
			      (enum symbol-enum nonterminals) ...)
			(list (enum symbol-enum $error)
			      (enum symbol-enum terminals) ...)
			(length '($start nonterminals ...))
			(length '($start nonterminals ... $error terminals ...))
			(enum symbol-enum $error)
			(enum symbol-enum $start)
			(list (make-production
			       (enum symbol-enum $start)
			       (list (enum symbol-enum start-symbol))
			       '(lambda (x) x))
			      (make-production
			       (enum symbol-enum lhs)
			       (list (enum symbol-enum rhs) ...)
			       `(lambda ,(attribution-arglist '(rhs ...))
				  expression))
			      ...)
			'terminal-attribution))))))

(define (productions-with-lhs lhs grammar)
  (filter (lambda (production)
	    (equal? lhs (production-lhs production)))
	  (grammar-productions grammar)))

; nullable computation

(define (compute-nullable? grammar)
  
  (let ((visited-vector
	 (make-vector (grammar-number-of-nonterminals grammar) #f))
	(nullable-vector
	 (make-vector (grammar-number-of-nonterminals grammar) #f)))
    
    (define (nullable? nonterminal)
      (if (vector-ref visited-vector nonterminal)
	  (vector-ref nullable-vector nonterminal)
	  (begin
	    (vector-set! visited-vector nonterminal #t)
	    (let loop ((productions (productions-with-lhs nonterminal grammar)))
	      (if (null? productions)
		  #f
		  (let ((production (car productions)))
		    (if (every? (lambda (symbol)
				  (and (nonterminal? symbol grammar)
				       (nullable? symbol)))
				(production-rhs production))
			(begin
			  (vector-set! nullable-vector nonterminal #t)
			  #t)
			(loop (cdr productions)))))))))

    (for-each nullable? (grammar-nonterminals grammar))

    (lambda (nonterminal)
      (vector-ref nullable-vector nonterminal))))

(define (sequence-nullable? sequence grammar nullable?)
  (not (any? (lambda (symbol)
	       (or (terminal? symbol grammar)
		   (not (nullable? symbol))))
	     sequence)))

; First set computation

(define (sf-first rhs k grammar first-map)
  (let loop ((rhs-rest rhs))
    
    (if (null? rhs-rest)
	'(())
	(let ((cdr-first (loop (cdr rhs-rest)))
	      (s (car rhs-rest)))
	  (if (terminal? s grammar)
	      (uniq
	       (map (lambda (f)
		      (restricted-append k (list s) f))
		    cdr-first))
	      (list-union
	       (map
		(lambda (f-cdr)
		  (map
		   (lambda (f-car)
		     (restricted-append k f-car f-cdr))
		   (vector-ref first-map s)))
		cdr-first)))))))

(define (lhs-next-first lhs k grammar old-first)
  (let loop ((ps (productions-with-lhs lhs grammar))
	     (first '()))
    (if (null? ps)
	first
	(let ((rhs-first (sf-first (production-rhs (car ps)) k grammar old-first)))
	  (loop (cdr ps)
		(union first rhs-first))))))

(define (initial-first-map grammar)
  ;; each nonterminal is associated with the empty set
  (make-vector (grammar-number-of-nonterminals grammar) '()))

(define (next-first-map grammar k last-first-map)
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (let ((new-first-map (make-vector (grammar-number-of-nonterminals grammar))))
    (for-each
     (lambda (nonterminal)
       (vector-set! new-first-map nonterminal
		    (lhs-next-first nonterminal k grammar last-first-map)))
     (grammar-nonterminals grammar))
    new-first-map))
 
(define (first-equal? f-1 f-2)
  (and (= (length f-1) (length f-2))
       (let loop ((f-1 f-1))
	 (or (null? f-1)
	     (and (member (car f-1) f-2)
		  (loop (cdr f-1)))))))
	     
(define (first-map-equal? fm-1 fm-2)
  (let ((size (vector-length fm-1)))
    (let loop ((i 0))
      (or (= i size)
	  (and (first-equal? (vector-ref fm-1 i)
			     (vector-ref fm-2 i))
	       (loop (+ 1 i)))))))

(define (compute-first grammar k)
  ;; fixpoint iteration
  (if (= 1 k)
      (compute-first-1 grammar)
      (let loop ((first-map (initial-first-map grammar)))
	(let ((new-first-map (next-first-map grammar k first-map)))
	  (if (first-map-equal? first-map new-first-map)
	      first-map
	      (loop new-first-map))))))

; first_1 computation

(define (compute-first-1 grammar)
  (let ((nullable? (compute-nullable? grammar))
	(first-map (make-vector (grammar-number-of-nonterminals grammar) '()))
	(depths (make-vector (grammar-number-of-nonterminals grammar) 0)))
    
    (define (for-each-nonterminal f)
      (for-each f (grammar-nonterminals grammar)))

    ;; each Y with X -> \alpha Y \beta with \alpha nullable
    (define (initial-symbols lhs)
      (let production-loop ((productions (productions-with-lhs lhs grammar))
			    (symbols '()))
	(if (not (null? productions))
	    (let loop ((rhs-rest (production-rhs (car productions)))
		       (symbols symbols))
	      (if (not (null? rhs-rest))
		  (let* ((symbol (car rhs-rest))
			 (visited? (or (= lhs symbol)
				       (memv symbol symbols)))
			 (symbols (if visited?
				      symbols
				      (cons symbol symbols))))
		    (if (and (nonterminal? symbol grammar)
			     (nullable? symbol))
			(loop (cdr rhs-rest) symbols)
			(production-loop (cdr productions) symbols)))
		  (production-loop (cdr productions) symbols)))
	    symbols)))

    (define (for-each-induction f lhs)
      (for-each f
		(filter (lambda (symbol)
			  (nonterminal? symbol grammar))
			(initial-symbols lhs))))

    (define (associate-depth! nonterminal depth)
      (vector-set! depths nonterminal depth))
    
    (define (depth-association nonterminal)
      (vector-ref depths nonterminal))

    (define (merge-firsts! lhs nonterminal)
      (vector-set! first-map lhs
		   (union (vector-ref first-map lhs)
			  (vector-ref first-map nonterminal))))

    (for-each
     (lambda (nonterminal)
       (let ((initial (filter-map
		       (lambda (symbol)
			 (and (terminal? symbol grammar)
			      (cons symbol '())))
		       (initial-symbols nonterminal))))
	 (vector-set! first-map nonterminal
		      (if (nullable? nonterminal)
			  (cons '() initial)
			  initial))))
     (grammar-nonterminals grammar))

    (complete-subsets! for-each-nonterminal
		       =
		       for-each-induction
		       associate-depth! depth-association
		       merge-firsts!)
    first-map))

; Follow set computation

(define (initial-follow-map grammar)
  ;; start symbol must be followed by the empty string to get off the
  ;; ground
  (map (lambda (nt)
	 (cons nt
	       (if (equal? nt (grammar-start grammar))
		   '(())
		   '())))
       (grammar-nonterminals grammar)))

;;; perform
;;; follow (k, A) = U { first (k, beta follow (k, B)) | B -> alpha A beta }
;;; by iterating over the right sides of all productions, updating the
;;; follow-set as appropriate

(define (next-follow-map grammar k first-map last-follow-map)
  (let loop ((productions (grammar-productions grammar))
	     (last-follow-map last-follow-map))
    (if (null? productions)
	last-follow-map
	(let ((lhs (production-lhs (car productions))))
	  (let rhs-loop ((rhs-rest (production-rhs (car productions)))
			 (last-follow-map last-follow-map))
	    (if (null? rhs-rest)
		(loop (cdr productions) last-follow-map)
		(let ((sym (car rhs-rest)))
		  (if (terminal? sym grammar)
		      (rhs-loop (cdr rhs-rest) last-follow-map)
		      (let* ((fi-rest (sf-first (cdr rhs-rest) k grammar first-map))
			     (fo-lhs (cdr (assoc lhs last-follow-map)))
			     (fo-sym (uniq
				      (pair-map (lambda (xs ys) (restricted-append k xs ys))
					       fi-rest fo-lhs))))
		      (rhs-loop (cdr rhs-rest)
				(update-follow-map last-follow-map
						   sym fo-sym)))))))))))

(define (update-follow-map follow-map sym fo-sym)
  (let loop ((follow-map follow-map))
    (if (equal? (caar follow-map) sym)
	(cons (cons sym (union fo-sym (cdar follow-map)))
	      (cdr follow-map))
	(cons (car follow-map)
	      (loop (cdr follow-map))))))

(define (pair-map f xs ys)
  (let xs-loop ((xs xs))
    (if (null? xs)
	'()
	(let ((x (car xs)))
	  (let ys-loop ((ys ys))
	    (if (null? ys)
		(xs-loop (cdr xs))
		(let ((y (car ys)))
		  (cons (f x y) (ys-loop (cdr ys))))))))))

(define (compute-follow grammar k first-map)
  ;; fixpoint iteration
  (let loop ((follow-map (initial-follow-map grammar)))
    (let ((new-follow-map (next-follow-map grammar k first-map follow-map)))
      (if (first-map-equal? follow-map new-follow-map)
	  follow-map
	  (loop new-follow-map)))))

; List utilities

(define (uniq l)
  (let loop ((l l) (r '()))
    (if (null? l)
	(reverse r)
	(loop (cdr l)
	      (if (member (car l) r)
		  r
		  (cons (car l) r))))))

(define (union l1 l2)
  (append (filter (lambda (x)
		    (not (member x l2)))
		  l1)
	  l2))

(define (list-union l)
  (let loop ((l l) (r '()))
    (if (null? l)
	r
	(loop (cdr l)
	      (union (car l) r)))))

(define (take n l)
    (cond
     ((or (zero? n) (null? l))
      '())
     (else
      (cons (car l) (take (- n 1) (cdr l))))))

(define (restricted-append k l1 l2)
  (if (null? l1)
      (take k l2)
      (if (zero? k)
	  '()
	  (cons (car l1)
		(restricted-append (- k 1)
				   (cdr l1)
				   l2)))))
