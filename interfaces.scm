;; This depends on interfaces.scm

(define-interface cogen-directives-interface
  (export ((define-without-memoization define-memo define-primitive)
	   :syntax)
	  (define-data :syntax)))

(define-interface stream-interface
  (export make-stream
	  stream-car stream-cdr stream-empty?
	  list->stream stream->list))

(define-interface options-interface (export get-options))

(define-interface main-interface (export main))

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

