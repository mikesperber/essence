(define (run-cps-parser source-grammar k input)
  (cps-parse-main (source-grammar->grammar source-grammar k)
		  k
		  input))

(define (skip-loadts code)
  (let ((form (car code)))
    (if (and (pair? form)
	     (eq? 'loadt (car form)))
	(skip-loadts (cdr code))
	code)))
		    
; (define (generate-cps-parser-direct source-grammar k goal-function outfile)
;   (call-with-values
;    (lambda ()
;      (source-grammar-split source-grammar k))
;    (lambda ()
;      (lambda (grammar attributions)
;        (let* ((resid-code
; 	       (skip-loadts
; 		(similix
; 		 'parse-main (list source-grammar k '***)
; 		 "cps-lr.sim"
; 		 goal-function)))
; 	 


