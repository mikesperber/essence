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


(define-structure essence-cps-lr-generate essence-parser-generate-interface
  (open scheme
	essence-grammars
	essence-cps-lr-genext pgg-specialize
	cogen-gensym cogen-globals
	big-util)
  (begin
    (define (generate-parser grammar lookahead method goal-name)
					; (gensym-ignore-name-stubs!)
      (set-generate-flat-program! #t)
      (set-lambda-is-pure! #f)
      (specialize compute-parser
		  '(compute-parser 0 0 0 0 1)
		  (list grammar lookahead method 0 'input)
		  goal-name)
      (cons
       (grammar-define-enumeration-form grammar)
       (append (filter (lambda (form)	; massive kludge
			 (not (eq? 'define-data (car form))))
		       *support-code*)
	       (get-residual-program))))))

;; Batch version

(define-structure essence-grammar-scratch-package (export *grammar-scratch-package*)
  (open scheme essence-grammars)
  (begin
    (define *grammar-scratch-package* (interaction-environment))))

(define-structure essence-cps-lr-parser-generator essence-main-interface
  (open scheme
	essence-options
	essence-cps-lr-generate essence-lr-spectime
	essence-grammar-scratch-package
	i/o os-strings big-util formats exceptions conditions pp)
  (files (src main)))
