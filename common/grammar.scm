; Start-separated grammars

(define-record-type grammar
  (nonterminals terminals
   error start
   productions
   terminal-attribution)
  ())

(define (grammar-start-production grammar)
  (car (grammar-productions grammar)))

(define (terminal? sym grammar)
  (or (member sym (grammar-terminals grammar))))

(define (nonterminal? sym grammar)
  (not (terminal? sym grammar)))

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
	 ($error terminals ... $start nonterminals ...))
       (define grammar-name
	 (grammar-maker (list (enum symbol-enum nonterminals) ...)
			(list (enum symbol-enum $error)
			      (enum symbol-enum terminals) ...)
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
		   (cdr (assoc s first-map))))
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
  (map (lambda (nt)
	 (cons nt '()))
       (grammar-nonterminals grammar)))

(define (next-first-map grammar k last-first-map)
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (map
   (lambda (first-map-entry)
     (let ((nonterm (car first-map-entry)))
       (cons nonterm
	     (lhs-next-first nonterm k grammar last-first-map))))
   last-first-map))
 
(define (first-equal? f-1 f-2)
  (and (= (length f-1) (length f-2))
       (let loop ((f-1 f-1))
	 (or (null? f-1)
	     (and (member (car f-1) f-2)
		  (loop (cdr f-1)))))))
	     
(define (first-map-equal? fm-1 fm-2)
  (let loop ((fm-1 fm-1))
    (or (null? fm-1)
	(let* ((nonterm-1 (caar fm-1))
	       (first-1 (cdar fm-1))
	       (first-2 (cdr (assoc nonterm-1 fm-2))))
	  (and (first-equal? first-1 first-2)
	       (loop (cdr fm-1)))))))


(define (compute-first grammar k)
  ;; fixpoint iteration
  (let loop ((first-map (initial-first-map grammar)))
    (let ((new-first-map (next-first-map grammar k first-map)))
      (if (first-map-equal? first-map new-first-map)
	  first-map
	  (loop new-first-map)))))

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
