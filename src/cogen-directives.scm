; Copyright (c) 2000-2008 by Michael Sperber and Peter Thiemann. See file COPYING.

(define-syntax define-without-memoization
  (syntax-rules ()
    ((define-without-memoization bla ...)
     (define bla ...))))

(define-syntax define-memo
  (syntax-rules ()
    ((define-memo name level more ...)
     (define-syntax name
       (syntax-rules ()
	 ((name x) x))))))

(define-syntax define-primitive
  (syntax-rules ()
    ((define-primitive o t k)
     (begin
       (display "defined primitive ") (display 'o)
       (newline)))))
       