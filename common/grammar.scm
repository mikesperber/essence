; Start-separated grammars

(define-record-type grammar :grammar
  (make-grammar nonterminals terminals error
		start
		productions
		terminal-attribution)
  (nonterminals grammar-nonterminals)
  (terminals grammar-terminals)
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

(define (attribution-arglist args)
  (define (conc . things)
    (string->symbol (apply string-append
			   (map (lambda (thing)
				  (if (symbol? thing)
				      (symbol->string thing)
				      thing))
				things))))
  
  (let loop ((i (length args)) (l '()))
    (if (zero? i)
	l
	(loop (- i 1) (cons (conc "$" (number->string i)) l)))))

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
	 (make-grammar (list (enum symbol-enum nonterminals) ...)
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
