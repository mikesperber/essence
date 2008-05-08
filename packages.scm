(define-structure cogen-directives cogen-directives-interface
  (open scheme define-record-types)
  (files (src cogen-directives)
	 (src defdata)))

(define-structure essence-options essence-options-interface
  (open scheme big-util)
  (files (src command-line)))

(define-structure essence-grammars essence-grammars-interface
  (open scheme big-util define-record-types enumerated essence-scc-unions)
  (for-syntax (open scheme tables))
  (files (src grammar)))

(define-structure essence-scc-unions essence-scc-unions-interface
  (open scheme)
  (files (src scc-union)))

(define-structure essence-lr-spectime essence-lr-spectime-interface
  (open scheme big-util sort essence-grammars)
  (files (src lr-spectime)))

(define-structure essence-cps-lr essence-parser-interface
  (open scheme
	srfi-23 ; ERROR
	essence-grammars essence-lr-spectime
	cogen-directives)
  (files (src the-trick)
	 (src lookahead-trie)
	 (src memo)
	 (src cps-lr))
  (begin
    (define (parse-error message closure error-status recovering? symbol input)
      (if recovering?
	  (begin
	    (display message)
	    (if error-status
		(begin
		  (display "; last error was ")
		  (display error-status)
		  (display " lexemes ago")
		  (newline))))
	  (error message closure symbol input)))))

(define-structure essence-tests (export toy-grammars-tests)
  (open scheme
	enumerated
	test-suites
	essence-grammars
	essence-cps-lr)
  (files (examples toy-grammars-test)
	 (examples toy-grammars)
	 (examples toy-inputs)))

