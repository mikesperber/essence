; Start-separated grammars

(define-record-type grammar :grammar
  (make-grammar nonterminals terminals eoi error
		start
		productions
		terminal-attribution)
  (nonterminals grammar-nonterminals)
  (terminals grammar-terminals)
  (eoi grammar-eoi)
  (error grammar-error)
  (start grammar-start)
  (productions grammar-productions)
  (terminal-attribution grammar-terminal-attribution))
  
(define (grammar-start-production grammar)
  (car (grammar-productions grammar)))

(define (terminal? sym grammar)
  (or (member sym (grammar-terminals grammar))))

(define (nonterminal? sym grammar)
  (not (terminal? sym grammar)))

; Productions are specialized and show up in the specialized output.
; Hence, they need an external representation.

(define (make-production lhs rhs attribution)
  (vector lhs rhs attribution))
	  
(define (production-lhs p) (vector-ref p 0))
(define (production-rhs p) (vector-ref p 1))
(define (production-attribution p) (vector-ref p 2))

(define-syntax define-grammar
  (lambda (exp rename compare)

    (define (conc . things)
      (string->symbol (apply string-append
			     (map (lambda (thing)
				    (if (symbol? thing)
					(symbol->string thing)
					thing))
				  things))))

    (define rule-production car)
    (define (rule-contains-attribution? rule)
      (not (null? (cdr rule))))
    (define rule-attribution cadr)

    (define (make-attribution-arglist i)
      (let loop ((i i) (l '()))
	(if (zero? i)
	    l
	    (loop (- i 1)
		  (cons (conc "$" (number->string i)) l)))))

    (define (make-trivial-attribution rhs-length)
      `(lambda ,(make-attribution-arglist rhs-length)
	 '$1))

    (define (rule-make-attribution rule)
      (let ((rhs-length (length (cdr (rule-production rule)))))
	`(lambda ,(make-attribution-arglist rhs-length)
	   ,(if (rule-contains-attribution? rule)
		(rule-attribution rule)
		'$1))))

    (let ((name (cadr exp))
	  (nonterminals (list-ref exp 2))
	  (terminals (list-ref exp 3))
	  (start (list-ref exp 4))
	  (rules (list-ref exp 5))
	  (terminal-attribution (if (null? (list-tail exp 6))
				    #f
				    (list-ref exp 6)))

	  (%begin (rename 'begin))
	  (%define (rename 'define))
	  (%let (rename 'let))
	  (%lambda (rename 'lambda))

	  (%list (rename 'list))
	  (%map (rename 'map))

	  (%define-enumeration (rename 'define-enumeration))
	  (%name->enumerand (rename 'name->enumerand))
	  (%enum (rename 'enum))

	  (%make-grammar (rename 'make-grammar))
	  (%make-production (rename 'make-production)))

      (let ((s-name (conc name '- "symbol"))
	    (new-start (conc start "^"))
	    (extra-terminals '(eoi error)))

	`(,%begin
	  (,%define-enumeration ,s-name ,(append extra-terminals 
						 terminals
						 (list new-start)
						 nonterminals))
	  (,%define ,name
	    (,%make-grammar (,%list
			     ,@(map (lambda (n) `(,%enum ,s-name ,n))
				    (cons new-start nonterminals)))
			    (,%list
			     ,@(map (lambda (t) `(,%enum ,s-name ,t))
				    (append extra-terminals terminals)))
			    (,%enum ,s-name eoi)
			    (,%enum ,s-name error)
			    (,%enum ,s-name ,new-start)
			    (,%list
			     (,%make-production
			      (,%enum ,s-name ,new-start)
			      (,%list (,%enum ,s-name ,start))
			      ,(make-trivial-attribution 1))
			     ,@(map
				(lambda (rule)
				  `(,%make-production
				    (,%enum ,s-name ,(car (rule-production rule)))
				    (,%list
				     ,@(map (lambda (symbol)
					    `(,%enum ,s-name ,symbol))
					   (cdr (rule-production rule))))
				    ',(rule-make-attribution rule)))
				rules))

			    ',terminal-attribution)))))))

