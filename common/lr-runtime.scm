;; LR support code needed at run time
;; ==================================

(define (scan-list->stream l)
  (make-stream
   (lambda (l)
     (if (not (null? l))
	 l				; it just works out that way	 
	 #f))
   l))

