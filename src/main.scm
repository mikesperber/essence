; Copyright (c) 2000-2008 by Michael Sperber and Peter Thiemann. See file COPYING.

(define *options*
  '(("help" "h" help)
    ("states" "s" states) 
    ("pretty-print" "p" "pp" pretty-print)
    ("g" "goal-proc" "goal-procedure" parameter goal-procedure)
    ("m" "method" parameter method)
    ("l" "lookahead" parameter lookahead)
    ("6" "r6rs-library" parameter r6rs-library)
    ("i" "r6rs-import" parameter accumulates r6rs-import)))

(define *usage*
  '("Usage:"
    "Explain me:"
    "essence ( --help | -h )"
    ""
    "Generate a parser:"
    "essence ( -g goal-proc | --goal-proc=goal-proc | --goal-procedure=goal-proc )"
    "        ( -m method | --method=method )"
    "        ( -l lookahead | -lookahead=lookahead )"
    "        ( -s | --states)"
    "        ( -p | --pp --pretty-print)"
    "        ( -6 library-name | --r6rs-library=library-name )"
    "        ( -i library-name | --r6rs-import=library-name )"
    "        input-file grammar-name output-file"
    ""
    "where method must be slr or lr, and lookahead a non-negative number."
    "Defaults are --method=slr and --lookahead=1."))

(define (display-usage)
  (for-each (lambda (line)
	      (display line) (newline))
	    *usage*))

(define (main argv)
  (call-with-current-continuation
   (lambda (exit)

     (guard
      (condition
       (else
	(if (message-condition? condition)
	    (display (condition-message condition) (current-error-port))
	    (display "unknown errror" (current-error-port)))
	(newline (current-error-port))
	(exit 1)))

      (call-with-values
	  (lambda ()
	    (guard
	     (condition
	      (else
	       (if (message-condition? condition)
		   (display (condition-message condition) (current-error-port))
		   (display "unknown errror" (current-error-port)))
	       (newline (current-error-port))
	       (display-usage)
	       (exit 1)))
	     (call-with-values
		 (lambda () (get-options *options* (map os-string->string argv)))
	       (lambda (options arguments)
		 (if (assq 'help options)
		     (begin
		       (display-usage)
		       (exit 0)))
		  
		 (if (not (= 3 (length arguments)))
		     (error "Wrong number of arguments.~%"))
		  
		 (values options arguments)))))

	(lambda (options arguments)
	  (let ((input-file-name (car arguments))
		(grammar-name (string->symbol (cadr arguments)))
		(output-file-name (caddr arguments))
		(r6rs-library
		 (cond
		  ((assq 'r6rs-library options) => cdr)
		  (else #f)))
		(r6rs-imports
		 (cond
		  ((assq 'r6rs-import options) => cdr)
		  (else '())))
		(goal-name
		 (cond
		  ((assq 'goal-procedure options) =>
		   (lambda (stuff)
		     (string->symbol (cdr stuff))))
		  (else 'parse)))
		(lookahead
		 (cond
		  ((assq 'lookahead options) =>
		   (lambda (stuff)
		     (let ((tentative-lookahead (string->number (cdr stuff))))
		       (if (and (integer? tentative-lookahead)
				(not (negative? tentative-lookahead)))
			   tentative-lookahead
			   (error "Invalid lookahead.~%")))))
		  (else 1)))
		(method
		 (cond
		  ((assq 'method options) =>
		   (lambda (stuff)
		     (cond
		      ((string-ci=? "lr" (cdr stuff)) 'lr)
		      ((string-ci=? "slr" (cdr stuff)) 'slr)
		      (else
		       (error "Invalid method.~%")))))
		  (else 'slr))))
	     
	    (load input-file-name *grammar-scratch-package*)
	     
	    (if (assq 'states options)
		(trace-states!))
	    (let* ((grammar (eval grammar-name *grammar-scratch-package*))
		   (parser (generate-parser grammar lookahead method goal-name)))
	      (with-output-to-file output-file-name
		(lambda ()
		  (if r6rs-library
		      (begin
			(display "#!r6rs") (newline)
			(display "(library ") (display r6rs-library) (newline)
			(display "  (export ")
			(display goal-name) (display " ") (display (grammar-enum-name grammar))
			(display ")") (newline)
			(display "  (import")
			(for-each (lambda (imp)
				    (display " ")
				    (display imp))
				  r6rs-imports)
			(display ")") (newline)))

		  (for-each (if (assq 'pretty-print options)
				p
				(lambda (form) (write form) (newline)))
			    parser)
		  
		  (if r6rs-library
		      (begin 
			(display ")") (newline))))))))))))
  0)

