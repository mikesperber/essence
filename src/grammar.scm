; Copyright (c) 2000-2008 by Michael Sperber and Peter Thiemann. See file COPYING.

; Start-separated grammars

; Guarantees:

; - the grammar symbols are numbers.
; - nonterminals and terminals are consecutive, respectively
; - the nonterminals come before the terminals
; - the start production is first

(define-record-type grammar :grammar
  (really-make-grammar name enum-name
		       nonterminals
		       terminals
		       number-of-terminals
		       number-of-symbols
		       error start
		       productions-by-lhs
		       symbol->name-procedure
		       terminal-attribution
		       properties)
  grammar?
  (name grammar-name)
  (enum-name grammar-enum-name)
  (nonterminals grammar-nonterminals)
  (terminals grammar-terminals)
  (number-of-terminals grammar-number-of-terminals)
  (number-of-symbols grammar-number-of-symbols)
  (error grammar-error)
  (start grammar-start)
  (productions-by-lhs grammar-productions-by-lhs)
  (symbol->name-procedure grammar-symbol->name-procedure)
  (terminal-attribution grammar-terminal-attribution)
  (properties grammar-properties set-grammar-properties!))

(define (grammar-number-of-nonterminals grammar)
  (- (grammar-number-of-symbols grammar)
     (grammar-number-of-terminals grammar)))

(define (grammar-nonterminal-offset grammar)
  (car (grammar-nonterminals grammar)))

;; at least nonterminals must be sorted
(define (make-grammar name enum-name
		      nonterminals terminals
		      error start
		      productions-by-lhs ; vector, indexed by normalized non-terminal
		      symbol->name-procedure
		      terminal-attribution)
  (let* ((number-of-nonterminals (length nonterminals))
	 (number-of-terminals (length terminals))
	 (number-of-symbols (+ number-of-terminals number-of-nonterminals)))
    
    (really-make-grammar name enum-name
			 nonterminals terminals
			 number-of-terminals
			 number-of-symbols
			 error start
			 productions-by-lhs
			 symbol->name-procedure
			 terminal-attribution
			 '())))

(define (grammar-fetch-property grammar name proc)
  (cond
   ((assoc name (grammar-properties grammar)) => cdr)
   (else
    (let ((value (proc grammar)))
      (set-grammar-properties! grammar
			       (cons (cons name value)
				     (grammar-properties grammar)))
      value))))

(define (grammar-symbol->name symbol grammar)
  ((grammar-symbol->name-procedure grammar) symbol))

(define (grammar-start-production grammar)
  (car (grammar-productions-with-lhs (grammar-start grammar) grammar)))

(define (terminal? symbol grammar)
  (< symbol (grammar-number-of-terminals grammar)))

(define (nonterminal? symbol grammar)
  (>= symbol (grammar-number-of-terminals grammar)))

(define (grammar-productions-with-lhs lhs grammar)
  (vector-ref (grammar-productions-by-lhs grammar)
	      (- lhs (grammar-nonterminal-offset grammar))))

(define (grammar-for-each-production proc grammar)
  (do ((offset (grammar-nonterminal-offset grammar))
       (by-lhs (grammar-productions-by-lhs grammar))
       (count (grammar-number-of-nonterminals grammar))
       (i 0 (+ 1 i)))
      ((= i count))
    (for-each proc (vector-ref by-lhs i))))

; Productions are specialized and show up in the specialized output.
; Hence, they need equality and an external representation.

; We break this abstraction further below in `define-grammar-2'
(define (make-production lhs rhs attribution)
  (vector lhs rhs attribution))
	  
(define (production-lhs p) (vector-ref p 0))
(define (production-rhs p) (vector-ref p 1))
(define (production-attribution p) (vector-ref p 2))

(define-syntax define-grammar
  (syntax-rules ()
    ((define-grammar grammar-name symbol-enum ts s rules)
     (define-grammar grammar-name symbol-enum ts s rules #f))
    ((define-grammar grammar-name symbol-enum ts s rules attribution-arglist)
     (define-grammar-1 grammar-name symbol-enum ts s rules () attribution-arglist))))

; check syntax
(define-syntax define-grammar-1
  (syntax-rules ()
    ((define-grammar-1 grammar-name symbol-enum
       (terminals ...)
       start-symbol
       ((lhs ((rhs ...) body ...) ...) rest ...)
       (rule ...)
       terminal-attribution)
     (define-grammar-1 grammar-name symbol-enum
       (terminals ...)
       start-symbol
       (rest ...)
       (rule ... (lhs ((rhs ...) body ...) ...))
       terminal-attribution))
    ((define-grammar-1 grammar-name symbol-enum
       (terminals ...)
       start-symbol
       ()
       (rule ...)
       terminal-attribution)
     (define-grammar-2 grammar-name symbol-enum
       (terminals ...)
       start-symbol
       (rule ...)
       terminal-attribution))))

(define-syntax define-grammar-2
  (lambda (e r c)

    (define (attribution-arglist args)
      (let loop ((i (length args)) (l '()))
	(if (zero? i)
	    l
	    (loop (- i 1) (cons (string->symbol (string-append "$" (number->string i))) l)))))

    (define (delete-duplicates l)
      (let loop ((l l) (rev '()))
	(cond
	 ((null? l) (reverse rev))
	 ((memq (car l) rev) (loop (cdr l) rev))
	 (else (loop (cdr l) (cons (car l) rev))))))

    (let ((%begin (r 'begin))
	  (%define-enumeration (r 'define-enumeration))
	  (%make-grammar (r 'make-grammar))
	  (%enum (r 'enum))
	  (%make-production (r 'make-production))
	  (%list (r 'list))
	  (%enumerand->name (r 'enumerand->name))
	  (%define (r 'define))
	  (%lambda (r 'lambda))
	  (%attribution-arglist (r 'attribution-arglist)))
		   
      (apply
       (lambda (_ grammar-name symbol-enum
		  terminals start-symbol productions terminal-attribution)
	 (let* ((nonterminals
		 (delete-duplicates (map car productions)))
		(symbols `($error ,@terminals $start ,@nonterminals))
		(nonterminal-offset (+ 1 (length terminals)))
		(productions-by-lhs (make-vector (+ 1 (length nonterminals)) '()))
		(symbol-table
		 (do ((symbol-table (make-symbol-table))
		      (i 0 (+ 1 i))
		      (ss symbols (cdr ss)))
		     ((null? ss) symbol-table)
		   (table-set! symbol-table (car ss) i))))

	   (define (symbol-index s)
	     (or (table-ref symbol-table s)
		 (car s)))

	   (define (nonterminal-index nt)
	     (- (symbol-index nt) nonterminal-offset))

	   (define (add-production! prod)
	     (let ((i (- (vector-ref prod 0) nonterminal-offset)))
	       (vector-set! productions-by-lhs i
			    (cons prod (vector-ref productions-by-lhs i)))))
	   
	   (add-production! (vector nonterminal-offset (list (symbol-index start-symbol)) '(lambda (x) x)))

	   (for-each (lambda (rule)
		       (let ((lhs (symbol-index (car rule))))
			 (for-each (lambda (prod)
				     (let ((rhs (car prod))
					   (body (cdr prod)))
				       (add-production! (vector lhs
								(map symbol-index rhs)
								`(lambda ,(attribution-arglist rhs)
								   ,@body)))))
				   (cdr rule))))
		     productions)

	   `(,%begin
	     (,%define-enumeration ,symbol-enum
	        ,symbols)
	     (,%define ,grammar-name
	        (,%make-grammar ',grammar-name ',symbol-enum
				',(map symbol-index (cons '$start nonterminals))
				',(map symbol-index (cons '$error terminals))
				,(symbol-index '$error)
				,(symbol-index '$start)
				',productions-by-lhs
				(,%lambda (symbol)
					  (,%enumerand->name symbol ,symbol-enum))
				',terminal-attribution)))))
       e))))

(define (grammar-define-enumeration-form grammar)
  `(define-enumeration ,(grammar-enum-name grammar)
     (,@(map (lambda (s)
	       (grammar-symbol->name s grammar))
	     (grammar-terminals grammar))
      ,@(map (lambda (s)
	       (grammar-symbol->name s grammar))
	     (grammar-nonterminals grammar)))))

; nullable computation

(define (compute-nonterminal-nullable? grammar)
  
  (let ((offset (grammar-nonterminal-offset grammar))
	(visited-vector
	 (make-vector (grammar-number-of-nonterminals grammar) #f))
	(nullable-vector
	 (make-vector (grammar-number-of-nonterminals grammar) #f)))
    
    (define (nullable? nonterminal)
      (if (vector-ref visited-vector (- nonterminal offset))
	  (vector-ref nullable-vector (- nonterminal offset))
	  (begin
	    (vector-set! visited-vector (- nonterminal offset) #t)
	    (let loop ((productions (grammar-productions-with-lhs nonterminal grammar)))
	      (if (null? productions)
		  #f
		  (let ((production (car productions)))
		    (if (every? (lambda (symbol)
				  (and (nonterminal? symbol grammar)
				       (nullable? symbol)))
				(production-rhs production))
			(begin
			  (vector-set! nullable-vector (- nonterminal offset) #t)
			  #t)
			(loop (cdr productions)))))))))

    (for-each nullable? (grammar-nonterminals grammar))

    (lambda (nonterminal)
      (vector-ref nullable-vector (- nonterminal offset)))))

(define (nonterminal-nullable? nonterminal grammar)
  ((grammar-fetch-property grammar 'nonterminal-nullable?
			   compute-nonterminal-nullable?)
   nonterminal))

(define (sequence-nullable? sequence grammar)
  (not (any? (lambda (symbol)
	       (or (terminal? symbol grammar)
		   (not (nonterminal-nullable? symbol grammar))))
	     sequence)))

; First set computation

(define (really-nonterminal-first grammar nonterminal first-map)
  (vector-ref first-map (- nonterminal (grammar-nonterminal-offset grammar))))

(define (really-sequence-first sequence k grammar nonterminal-first)
  (let loop ((sequence-rest sequence))
    
    (if (null? sequence-rest)
	'(())
	(let ((cdr-first (loop (cdr sequence-rest)))
	      (s (car sequence-rest)))
	  (if (terminal? s grammar)
	      (uniq
	       (map (lambda (f)
		      (restricted-append k (list s) f))
		    cdr-first))
	      (list-union
	       (map
		(lambda (f-cdr)
		  (uniq
		   (map
		    (lambda (f-car)
		      (restricted-append k f-car f-cdr))
		    (nonterminal-first s k grammar))))
		cdr-first)))))))

(define (lhs-next-first lhs k grammar old-first)
  (let loop ((ps (grammar-productions-with-lhs lhs grammar))
	     (first '()))
    (if (null? ps)
	first
	(let ((rhs-first
	       (really-sequence-first
		(production-rhs (car ps)) k grammar
		(lambda (nonterminal k grammar)
		  (really-nonterminal-first grammar nonterminal old-first)))))
	  (loop (cdr ps)
		(union first rhs-first))))))

(define (initial-first-map grammar)
  ;; each nonterminal is associated with the empty set
  (make-vector (grammar-number-of-nonterminals grammar) '()))

(define (next-first-map grammar k last-first-map)
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (let ((new-first-map (make-vector (grammar-number-of-nonterminals grammar)))
	(offset (grammar-nonterminal-offset grammar)))
    (for-each
     (lambda (nonterminal)
       (vector-set! new-first-map (- nonterminal offset)
		    (lhs-next-first nonterminal k grammar last-first-map)))
     (grammar-nonterminals grammar))
    new-first-map))
 
(define (map-equal? fm-1 fm-2)
  (let ((size (vector-length fm-1)))
    (let loop ((i 0))
      (or (= i size)
	  (and (list-set-equal? (vector-ref fm-1 i)
				(vector-ref fm-2 i))
	       (loop (+ 1 i)))))))

(define (compute-first grammar k)
  (if (= 1 k)
      (compute-first-1 grammar)
      ;; fixpoint iteration
      (let loop ((first-map (initial-first-map grammar)))
	(let ((new-first-map (next-first-map grammar k first-map)))
	  (if (not (map-equal? first-map new-first-map))
	      (loop new-first-map)
	      (lambda (nonterminal)
		(really-nonterminal-first grammar nonterminal first-map)))))))

(define (nonterminal-first nonterminal k grammar)
  ((grammar-fetch-property grammar (cons 'nonterminal-first k)
			   (lambda (grammar)
			     (compute-first grammar k)))
   nonterminal))

(define (sequence-first sequence k grammar)
  (really-sequence-first sequence k grammar nonterminal-first))

; first_1 computation

(define (compute-first-1 grammar)
  (let ((first-map (make-vector (grammar-number-of-nonterminals grammar) '()))
	(depths (make-vector (grammar-number-of-nonterminals grammar) 0))
	(offset (grammar-nonterminal-offset grammar)))
    
    (define (for-each-nonterminal f)
      (for-each f (grammar-nonterminals grammar)))

    ;; each Y with X -> \alpha Y \beta with \alpha nullable
    (define (initial-symbols lhs)
      (let production-loop ((productions (grammar-productions-with-lhs lhs grammar))
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
			     (nonterminal-nullable? symbol grammar))
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
      (vector-set! depths (- nonterminal offset) depth))
    
    (define (depth-association nonterminal)
      (vector-ref depths (- nonterminal offset)))

    (define (overwrite-first! nonterminal-1 nonterminal-2)
      (vector-set! first-map (- nonterminal-1 offset)
		   (vector-ref first-map (- nonterminal-2 offset))))

    (define (merge-firsts! lhs nonterminal)
      (vector-set! first-map (- lhs offset)
		   (union (vector-ref first-map (- lhs offset))
			  (vector-ref first-map (- nonterminal offset)))))

    (for-each
     (lambda (nonterminal)
       (let ((initial (filter-map
		       (lambda (symbol)
			 (and (terminal? symbol grammar)
			      (cons symbol '())))
		       (initial-symbols nonterminal))))
	 (vector-set! first-map (- nonterminal offset)
		      (if (nonterminal-nullable? nonterminal grammar)
			  (cons '() initial)
			  initial))))
     (grammar-nonterminals grammar))

    (complete-subsets! for-each-nonterminal
		       =
		       for-each-induction
		       associate-depth! depth-association
		       overwrite-first! merge-firsts!)

    (lambda (nonterminal)
      (vector-ref first-map (- nonterminal offset)))))

; Follow set computation

(define (initial-follow-map grammar)
  (let ((follow-map (make-vector (grammar-number-of-nonterminals grammar) '())))
    ;; start symbol must be followed by the empty string to get off the
    ;; ground
    (vector-set! follow-map (- (grammar-start grammar) (grammar-nonterminal-offset grammar)) '(()))
    follow-map))

;;; perform
;;; follow (k, A) = follow (k, A) u U { first (k, beta follow (k, B)) | B -> alpha A beta }
;;; by iterating over the right sides of all productions, updating the
;;; follow-set as appropriate

(define (next-follow-map grammar k last-follow-map)
  (let ((new-follow-map (copy-vector last-follow-map))
	(offset (grammar-nonterminal-offset grammar)))
    (grammar-for-each-production
     (lambda (production)
       (let ((lhs (production-lhs production)))
	 (let rhs-loop ((rhs-rest (production-rhs production)))
	   (if (pair? rhs-rest)
	       (let ((sym (car rhs-rest)))
		 (if (terminal? sym grammar)
		     (rhs-loop (cdr rhs-rest))
		     (let* ((fi-rest (sequence-first (cdr rhs-rest) k grammar))
			    (fo-lhs (vector-ref new-follow-map (- lhs offset)))
			    (fo-sym (uniq
				     (pair-map (lambda (xs ys)
						 (restricted-append k xs ys))
					       fi-rest fo-lhs))))
		       (vector-set! new-follow-map (- sym offset)
				    (union fo-sym 
					   (vector-ref new-follow-map (- sym offset))))
		       (rhs-loop (cdr rhs-rest)))))))))
     grammar)
    new-follow-map))

(define (compute-follow grammar k)
  (if (= 1 k)
      (compute-follow-1 grammar)
      ;; fixpoint iteration
      (let ((offset (grammar-nonterminal-offset grammar)))
	(let loop ((follow-map (initial-follow-map grammar)))
	  (let ((new-follow-map (next-follow-map grammar k follow-map)))
	    (if (not (map-equal? follow-map new-follow-map))
		(loop new-follow-map)
		(lambda (nonterminal)
		  (vector-ref follow-map (- nonterminal offset)))))))))

; follow_1 computation

(define (compute-follow-1 grammar)

  (let ((follow-map (make-vector (grammar-number-of-nonterminals grammar) '()))
	(depths (make-vector (grammar-number-of-nonterminals grammar) 0))
	(offset (grammar-nonterminal-offset grammar)))

    (define (for-each-nonterminal f)
      (for-each f (grammar-nonterminals grammar)))

    (define (for-each-induction f nonterminal)
      (for-each
       (lambda (lhs)
	 (if (any? (lambda (production)
		     (cond
		      ((last-memv nonterminal (production-rhs production))
		       => (lambda (rhs-rest)
			    (sequence-nullable? (cdr rhs-rest) grammar)))
		      (else #f)))
		   (grammar-productions-with-lhs lhs grammar))
	     (f lhs)))
       (grammar-nonterminals grammar)))

    (define (associate-depth! nonterminal depth)
      (vector-set! depths (- nonterminal offset) depth))
    
    (define (depth-association nonterminal)
      (vector-ref depths (- nonterminal offset)))

    (define (overwrite-follow! nonterminal-1 nonterminal-2)
      (vector-set! follow-map (- nonterminal-1 offset)
		   (vector-ref follow-map (- nonterminal-2 offset))))


    (define (merge-follows! lhs nonterminal)
      (vector-set! follow-map (- lhs offset)
		   (union (vector-ref follow-map (- lhs offset))
			  (vector-ref follow-map (- nonterminal offset)))))

    (for-each
     (lambda (nonterminal)
       (let ((follow '()))
	 (grammar-for-each-production
	  (lambda (production)
	    (cond 
	     ((memv nonterminal (production-rhs production))
	      => (lambda (rhs-rest)
		   (let rhs-loop ((rhs-rest (cdr rhs-rest)))
		     (if (pair? rhs-rest)
			 (let ((first (delq '()
					    (sequence-first rhs-rest 1 grammar))))
			   (set! follow (union first follow))
			   (cond 
			    ((and (nonterminal? (car rhs-rest) grammar)
				  (nonterminal-nullable? (car rhs-rest) grammar))
			     (rhs-loop (cdr rhs-rest)))
			    ((memv nonterminal rhs-rest)
			     => (lambda (rhs-rest)
				  (rhs-loop (cdr rhs-rest))))))))))))
	  grammar)
	 (vector-set! follow-map (- nonterminal offset) follow)))
     (grammar-nonterminals grammar))

    (vector-set! follow-map (- (grammar-start grammar) offset)
		 (cons '() (vector-ref follow-map (- (grammar-start grammar) offset))))

    (complete-subsets! for-each-nonterminal
		       =
		       for-each-induction
		       associate-depth! depth-association
		       overwrite-follow! merge-follows!)

    (lambda (nonterminal)
      (vector-ref follow-map (- nonterminal offset)))))

(define (nonterminal-follow nonterminal k grammar)
  ((grammar-fetch-property grammar (cons 'nonterminal-follow k)
			   (lambda (grammar)
			     (compute-follow grammar k)))
   nonterminal))

; List utilities

(define (last-memv x l)
  (let loop ((rest l) (tail #f))
    (cond
     ((memv x rest)
      => (lambda (rest)
	   (loop (cdr rest) rest)))
     (else tail))))
      
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

(define (list-set-equal? f-1 f-2)
  (and (= (length f-1) (length f-2))
       (let loop ((f-1 f-1))
	 (or (null? f-1)
	     (and (member (car f-1) f-2)
		  (loop (cdr f-1)))))))

(define (copy-vector vector)
  (let* ((length (vector-length vector))
	 (copy (make-vector length)))
    (do ((i 0 (+ 1 i)))
	((= i length))
      (vector-set! copy i (vector-ref vector i)))
    copy))
