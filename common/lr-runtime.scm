;; LR support code needed at run time
;; ==================================

(define (scan-list->stream g l)
  (make-stream
   (lambda (l)
     (if (null? l)
	 (cons (cons (grammar-eoi g) #f)
	       '())
	 l))				; it just works out that way
   l))

