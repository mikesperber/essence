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
	  goto accept initial? handles-error?
	  active next-terminals next-nonterminals
	  make-item
	  item-lhs item-rhs item-production item-lookahead))

(define-interface parser-interface
  (export parse))

; Structures

(define-structure cogen-directives cogen-directives-interface
  (open scheme define-record-types)
  (files (common cogen-directives)
	 (common defdata)))

(define-structure stream stream-interface
  (open scheme)
  (files (common stream)))

(define-structure grammar grammar-interface
  (open scheme big-scheme scc-union)
  (files (common grammar)))

(define-structure scc-union scc-union-interface
  (open scheme)
  (files (common scc-union)))

(define-structure lr-spectime lr-spectime-interface
  (open scheme big-scheme grammar)
  (files (common lr-spectime)))

(define-structure ds-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives) ; DEFINE-WITHOUT-MEMOIZATION
  (files (common lookahead)
	 (direct direct-lr-naive)))

(define-structure ds-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (direct direct-lr)))

(define-structure ds-lr-imperative parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (direct direct-lr-imperative)))

(define-structure cps-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common lookahead)
	 (cps cps-lr-naive)))

(define-structure cps-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead-trie)
	 (cps cps-lr)))

(define-structure cps-lr-attrib parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (cps cps-lr-attrib)))

(define-structure cps-lr-attrib-error parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (cps cps-lr-attrib-error)))
