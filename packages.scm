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
	  goto accept initial?
	  active next-terminals next-nonterminals
	  make-item
	  item-lhs item-rhs item-production item-lookahead
	  items->trie))

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

(define-structure ds-lr parser-interface
  (open scheme signals cogen-directives lr-spectime)
  (begin
    (define _error error))
  (files (common the-trick)
	 (common lookahead)
	 (direct parse-result)
	 (direct direct-lr)))

(define-structure cps-lr-naive parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (cps cps-lr-naive)))

(define-structure cps-lr parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (cps cps-lr)))

(define-structure cps-lr-attrib parser-interface
  (open scheme signals grammar lr-spectime stream
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (cps cps-lr-attrib)))
