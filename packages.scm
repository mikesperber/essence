; Interfaces

(define-interface cogen-directives-interface
  (export ((define-without-memoization define-memo define-primitive)
	   :syntax)
	  (define-data :syntax)))

(define-interface make-stream-interface
  (export make-stream))

(define-interface access-stream-interface
  (export stream-car stream-cdr))

(define-interface grammar-interface
  (export make-production
	  grammar-productions grammar-nonterminals
	  grammar-start grammar-eoi grammar-error
	  production-lhs production-rhs production-attribution

	  grammar-start-production
	  terminal? nonterminal?
	  (define-grammar :syntax)))
	 
(define-interface lr-spectime-interface
  (export compute-closure compute-lr-closure
	  compute-slr-closure add-slr-lookahead
	  goto accept final?
	  active next-terminals next-nonterminals
	  make-item
	  item-lhs item-rhs item-production item-lookahead
	  items->trie
	  compute-first compute-follow

	  make-list))

(define-interface lr-runtime-interface
  (export scan-list->stream))

(define-interface parser-interface
  (export parse))

; Structures

(define-structure cogen-directives cogen-directives-interface
  (open scheme define-record-types)
  (files (common cogen-directives)
	 (common defdata)))

(define-structures ((access-stream access-stream-interface)
		    (make-stream make-stream-interface))
  (open scheme)
  (files (common stream)))

(define-structure grammar grammar-interface
  (open scheme enumerated define-record-types)
  (files (common grammar)))

(define-structure lr-spectime lr-spectime-interface
  (open scheme sort grammar)
  (files (common lr-spectime)))

(define-structure lr-runtime lr-runtime-interface
  (open scheme make-stream grammar)
  (files (common lr-runtime)))

(define-structure ds-lr parser-interface
  (open scheme signals cogen-directives lr-spectime)
  (begin
    (define _error error))
  (files (common the-trick)
	 (common lookahead)
	 (direct parse-result)
	 (direct direct-lr)))

(define-structure cps-lr parser-interface
  (open scheme signals grammar lr-spectime access-stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (cps cps-lr)))
