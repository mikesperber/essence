(define-structure cogen-directives cogen-directives-interface
  (open scheme define-record-types)
  (files (common cogen-directives)
	 (common defdata)))

(define-structure essence-options essence-options-interface
  (open scheme big-util)
  (files (common command-line)))

(define-structure essence-grammars essence-grammars-interface
  (open scheme big-util define-record-types enumerated essence-scc-unions)
  (files (common grammar)))

(define-structure essence-scc-unions essence-scc-unions-interface
  (open scheme)
  (files (common scc-union)))

(define-structure essence-lr-spectime essence-lr-spectime-interface
  (open scheme big-util sort essence-grammars)
  (files (common lr-spectime)))

(define-structure essence-ds-lr-naive essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives) ; DEFINE-WITHOUT-MEMOIZATION
  (files (common lookahead)
	 (direct direct-lr-naive)))

(define-structure essence-ds-lr-vanilla essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr-vanilla)))

(define-structure essence-ds-lr-imperative essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr-imperative)))

(define-structure essence-ds-lr essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (direct direct-lr)))

(define-structure essence-cps-lr-naive essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common lookahead)
	 (cps cps-lr-naive)))

(define-structure essence-cps-lr-vanilla essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-vanilla)))

(define-structure essence-cps-lr-attrib essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-attrib)))

(define-structure essence-cps-lr-attrib-error essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead)
	 (common memo)
	 (cps cps-lr-attrib-error)))

(define-structure essence-cps-lr essence-parser-interface
  (open scheme signals essence-grammars essence-lr-spectime
	cogen-directives)
  (files (common the-trick)
	 (common lookahead-trie)
	 (common memo)
	 (cps cps-lr)))

(define-module (make-parser-tests parser)
  (define-structure tests (export toy-grammars-tests)
    (open scheme
	  enumerated
	  test-suites
	  essence-grammars
	  parser)
    (files (examples toy-grammars-test)
	   (examples toy-grammars)
	   (examples toy-inputs)))
  tests)

(def essence-cps-lr-tests (make-parser-tests essence-cps-lr))
