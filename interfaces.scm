;; This depends on interfaces.scm

(define-interface cogen-directives-interface
  (export ((define-without-memoization define-memo define-primitive)
	   :syntax)
	  (define-data :syntax)))

(define-interface essence-options-interface (export get-options))

(define-interface essence-main-interface (export main))

(define-interface essence-grammars-interface
  (export grammar-nonterminals
	  grammar-start grammar-error
	  grammar-number-of-terminals grammar-number-of-nonterminals
	  grammar-number-of-symbols
	  grammar-nonterminal-offset
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

(define-interface essence-scc-unions-interface
  (export complete-subsets!))

(define-interface essence-lr-spectime-interface
  (export compute-lr-closure
	  compute-slr-closure
	  goto accept find-eoi-lookahead-item items->lookahead-sets+items
	  initial? handles-error?
	  active next-terminals next-nonterminals
	  make-item
	  item-lhs item-rhs item-production item-lookahead
	  display-item display-closure
	  check-for-reduce-reduce-conflict check-for-shift-reduce-conflict
	  trace-state trace-shift trace-reduce))

(define-interface essence-parser-interface
  (export parse))

(define-interface essence-parser-generate-interface
  (export generate-parser))

