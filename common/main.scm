(define *options*
  '(("help" "h" help)
    ("g" "goal-proc" "goal-procedure" parameter goal-procedure)
    ("m" "method" parameter method)
    ("l" "lookahead" parameter lookahead)))

(define *usage*
  '("Usage:"
    "Explain me:"
    "essence ( --help | -h )"
    ""
    "Generate a parser:"
    "essence ( -g goal-proc | --goal-proc=goal-proc | --goal-procedure=goal-proc )"
    "        ( -m method | --method=method )"
    "        ( -l lookahead | -lookahead=lookahead )"
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

     (with-handler

      (lambda (condition decline)
	(decline)
	(if (error? condition)
	    (apply format (current-error-port) (condition-stuff condition)))
	(exit 1))

      (lambda ()
	(call-with-values
	 (lambda ()
	   (with-handler
	    ;; we expect only errors here ...
	    (lambda (condition decline)
	      (apply format (current-error-port) (condition-stuff condition))
	      (display-usage)
	      (exit 1))
	    (lambda ()
	      (call-with-values
	       (lambda () (get-options *options* argv))
	       (lambda (options arguments)

		 (if (assq 'help options)
		     (begin
		       (display-usage)
		       (exit 0)))
	 
		 (if (not (= 3 (length arguments)))
		     (error "Wrong number of arguments.~%"))

		 (values options arguments))))))

	 (lambda (options arguments)
	   
	   (let ((input-file-name (car arguments))
		 (grammar-name (string->symbol (cadr arguments)))
		 (output-file-name (caddr arguments))
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
	     
	     (let ((grammar (eval grammar-name *grammar-scratch-package*)))
	       (let ((parser (generate-parser grammar lookahead method goal-name)))
		 (with-output-to-file output-file-name
		   (lambda ()
		     (for-each p parser))))))))))))
  0)

