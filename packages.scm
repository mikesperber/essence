; Interfaces

(define-interface cogen-directives-interface
  (export ((define-without-memoization define-memo define-primitive)
	   :syntax)
	  (define-data :syntax)))

(define-interface stream-interface
  (export make-stream
	  stream-car stream-cdr stream-empty?
	  list->stream))

(define-interface grammar-interface
  (export make-production
	  grammar-productions grammar-nonterminals
	  grammar-start grammar-error
	  production-lhs production-rhs production-attribution
	  productions-with-lhs

	  grammar-start-production
	  terminal? nonterminal?
	  (define-grammar :syntax)

	  compute-first sf-first compute-follow))
	 
(define-interface lr-spectime-interface
  (export compute-lr-closure
	  compute-slr-closure add-slr-lookahead
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
  (open scheme big-scheme)
  (files (common grammar)))

(define-structure lr-spectime lr-spectime-interface
  (open scheme big-scheme grammar)
  (files (common lr-spectime)))

(define-structure ds-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream)
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
