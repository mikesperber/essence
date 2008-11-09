#!r6rs

; Copyright (c) 2000-2008 by Michael Sperber and Peter Thiemann. See file COPYING.

;; Template for parser support code

(library (org s48 essence support)
  (export define-enumeration
	  parse-error
	  input-car input-cdr input-null?)
  (import (rnrs base)
	  (rnrs io ports)
	  (for (rnrs base) expand)
	  (for (rnrs syntax-case) expand)
	  (for (rnrs control) expand))

  (define input-car car)
  (define input-cdr cdr)
  (define input-null? null?)

  (define-syntax define-enumeration
    (lambda (stx)
      (syntax-case stx ()
	((define-enumeration ?name (?rand ...))
	 (do ((i (length #'(?rand ...)) (- i 1))
	      (l '() (cons i l)))
	     ((negative? i)
	      (with-syntax ((((?rand . ?index) ...) (map cons #'(?rand ...) l)))
		#'(define-syntax ?name
                    (syntax-rules (?rand ...)
                      ((?name ?rand) ?index) ...)))))))))

  (define (parse-error message closure error-status recovering? symbol input)
    (if (and recovering?
	     (or (not error-status)
		 (positive? error-status)))
	(let ((p (standard-error-port)))
	  (put-string p message)
	  (if error-status
	      (put-string p (string-append "; last error was "
					   (number->string error-status)
					   " lexemes ago")))
	  (put-string p "\n"))
	(error 'parse-error message closure symbol input)))
)
  