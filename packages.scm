; Interfaces

(define-interface cogen-directives-interface
  (export ((define-without-memoization define-memo define-primitive)
	   :syntax)
	  (define-data :syntax)))

(define-interface stream-interface
  (export make-stream
	  stream-car stream-cdr stream-empty?
	  list->stream stream->list))

(define-interface grammar-interface
  (export grammar-productions grammar-nonterminals
	  grammar-start grammar-error
	  grammar-number-of-nonterminals
	  grammar-productions-with-lhs
	  grammar-fetch-property
	  grammar-symbol->name
	  production-lhs production-rhs production-attribution

	  grammar-start-production
	  terminal? nonterminal?
	  (define-grammar :syntax)

	  nonterminal-nullable? sequence-nullable?
	  nonterminal-first sequence-first
	  nonterminal-follow))

(define-interface scc-union-interface
  (export complete-subsets!))

(define-interface lr-spectime-interface
  (export compute-lr-closure
	  compute-slr-closure
	  goto accept find-eoi-lookahead-item items->lookahead-sets+items
	  initial? handles-error?
	  active next-terminals next-nonterminals
	  make-item
	  item-lhs item-rhs item-production item-lookahead
	  display-item
	  check-for-reduce-reduce-conflict check-for-shift-reduce-conflict))

(define-interface parser-interface
  (export parse))

(define-interface parser-generate-interface
  (export generate-parser))

; Structures

(define-structure cogen-directives cogen-directives-interface
  (open scheme define-record-types)
  (files (common cogen-directives)
	 (common defdata)))

(define-structure stream stream-interface
  (open scheme)
  (files (common stream)))

(define-structure grammar grammar-interface
  (open scheme big-util defrecord enumerated scc-union)
  (files (common grammar)))

(define-structure scc-union scc-union-interface
  (open scheme)
  (files (common scc-union)))

(define-structure lr-spectime lr-spectime-interface
  (open scheme big-util sort grammar)
  (files (common lr-spectime)))

(define-structure ds-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives) ; DEFINE-WITHOUT-MEMOIZATION
  (files (common lookahead)
	 (direct direct-lr-naive)))

(define-structure ds-lr-vanilla parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr-vanilla)))

(define-structure ds-lr-imperative parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr-imperative)))

(define-structure ds-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (direct direct-lr)))

(define-structure cps-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common lookahead)
	 (cps cps-lr-naive)))

(define-structure cps-lr-vanilla parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-vanilla)))

(define-structure cps-lr-attrib parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-attrib)))

(define-structure cps-lr-attrib-error parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-attrib-error)))

(define-structure cps-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead-trie)
	 (common memo)
	 (cps cps-lr)))

(define-structure cps-lr-generate parser-generate-interface
  (open scheme
	cps-lr-genext pgg-specialize
	cogen-gensym cogen-globals
	big-util)
  (begin
    (define (generate-parser grammar lookahead method goal-name)
      (gensym-ignore-name-stubs!)
      (set-generate-flat-program! #t)
      (specialize compute-parser
		  '(compute-parser 0 0 0 1)
		  (list grammar lookahead method 'input)
		  goal-name)
      (append (filter (lambda (form)
			(not (eq? 'define-data (car form))))
		      *support-code*)
	      *residual-program*))))
