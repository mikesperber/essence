;; LR support code needed at run time
;; ==================================


(define (translate-input-list terminals input)
  (map (lambda (terminal)
	 (terminal->index terminals terminal))
       input))

(define (make-terminal-defines terminals)
  (map (lambda (terminal)
	 `(define ,terminal ,(terminal->index terminals terminal)))
       terminals))

(define (define-terminals terminals)
  (eval (cons 'begin (make-terminal-defines terminals))
	(interaction-environment)))
       
(define (terminate-input-list input k)
  (append input (make-list k eoi-terminal)))

