;; Interfaces
;; ~~~~~~~~~~

(define-interface source-grammar-interface
  (export (source-grammar-terminals (proc (:value) :value))
	  (source-grammar-nonterminals (proc (:value) :value))
	  (source-grammar-rules (proc (:value) :value))
	  (source-grammar-start  (proc (:value) :value))
	  (source-grammar-has-terminal-attribution? (proc (:value) :boolean))
	  (source-grammar-terminal-attribution (proc (:value) :value))
	  (first-nonterminal-index :number)
	  (list-position (proc (:value :value) :number))
	  (make-list (proc (:number :value) :value))
	  (terminal->index (proc (:value :value) :number))
	  (eoi-terminal :number)
	  (error-terminal :number)))
  
(define-interface lr-runtime-interface
  (export (eoi-terminal (proc () :number))
	  (error-terminal (proc () :number))
	  (source-grammar-terminals (proc (:value) :value))
	  (translate-input-list (proc (:value :value) :value))
	  (define-terminals (proc (:value) :value))
	  (terminate-input-list (proc (:value :number) :value))))

(define-interface lr-spectime-interface
  (export compute-closure compute-lr-closure
	  compute-slr-closure add-slr-lookahead
	  goto accept final?
	  active next-terminals next-nonterminals
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
	cogen-directives define-data)
  (files (common memo)
	 (common the-trick)
	 (common lookahead)
	 (cps cps-lr)))
