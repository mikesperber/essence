;; Interfaces
;; ~~~~~~~~~~

(define-interface cogen-directives-interface
  (export (define-without-memoization :syntax)
	  (define-memo :syntax)
	  (define-primitive :syntax)))

(define-interface source-grammar-interface
  (export (source-grammar-terminals (proc (:values) :values))
	  (source-grammar-nonterminals (proc (:values) :values))
	  (source-grammar-rules (proc (:values) :values))
	  (source-grammar-start  (proc (:values) :values))
	  (source-grammar-has-terminal-attribution? (proc (:values) :boolean))
	  (source-grammar-terminal-attribution (proc (:values) :values))
	  (first-nonterminal-index :number)
	  (list-position (proc (:values :values) :number))
	  (make-list (proc (:number :values) :values))
	  (terminal->index (proc (:values :values) :number))
	  (eoi-terminal :number)
	  (error-terminal :number)))
  
(define-interface lr-runtime-interface
  (export eoi-terminal
	  source-grammar-terminals
	  translate-input-list
	  define-terminals
	  terminate-input-list))

(define-interface lr-spectime-interface
  (export compute-closure compute-lr-closure
	  compute-slr-closure add-slr-lookahead
	  goto accept final?
	  next-terminals next-nonterminals
	  item-lhs item-rhs make-item
	  items->trie
	  production-rhs
	  source-grammar->grammar
	  grammar-start-production
	  compute-first compute-follow))

(define-interface parser-interface
  (export do-parse))

;; Structures
;; ~~~~~~~~~~

(define-structure cogen-directives cogen-directives-interface
  (open scheme)
  (files (common cogen-directives)))

(define-structure source-grammar source-grammar-interface
  (open scheme)
  (files (common source-grammar)))

(define-structure lr-runtime lr-runtime-interface
  (open scheme source-grammar)
  (files (common lr-runtime)))

(define-structure lr-spectime lr-spectime-interface
  (open scheme sort source-grammar)
  (files (common lr-spectime)))

(define-structure ds-lr parser-interface
  (open scheme signals cogen-directives lr-spectime lr-runtime)
  (begin
    (define _error error))
  (files (common memo)
	 (common the-trick)
	 (common lookahead)
	 (direct parse-result)
	 (direct direct-lr)))

(define-structure cps-lr parser-interface
  (open scheme signals lr-spectime lr-runtime
	cogen-directives cogen-define-data)
  (begin
    (define _error error))
  (files (common memo)
	 (common the-trick)
	 (common lookahead)
	 (cps cps-lr)))
