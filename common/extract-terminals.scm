(define (file->terminals fname)
  (source-grammar-terminals
   (with-input-from-file fname read)))

(define (extract-terminals fname bname)
  (write `(define ,(string->symbol bname)
	    ',(file->terminals fname))))

