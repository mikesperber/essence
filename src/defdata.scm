; Copyright (c) 2000-2008 by Michael Sperber and Peter Thiemann. See file COPYING.

; This is simpler than the cogen definition and hides more.
; Therefore, it probably should only be used at runtime.

(define-syntax define-data
  (lambda (x r c)
    (let ((clauses (cddr x))
	  (%begin (r 'begin))
	  (%define-record-type (r 'define-record-type)))
      `(,%begin
	,@(map
	   (lambda (ctor-decl)
	     (let* ((ctor-name (car ctor-decl))
		    (ctor-test (string->symbol (string-append
						(symbol->string ctor-name)
						"?")))
		    (ctor-type (string->symbol (string-append
						":"
						(symbol->string ctor-name)))))
	     
	       `(,%define-record-type ,ctor-name ,ctor-type
				      ,ctor-decl
				      ,ctor-test
				      ,@(map (lambda (selector)
					       (list selector selector))
					     (cdr ctor-decl)))))
	   clauses)))))

