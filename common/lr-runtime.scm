;; LR support code needed at run time
;; ==================================

(define (source-grammar-nonterminals g)
  (car g))
(define (source-grammar-terminals g)
  (list-ref g 1))
(define (source-grammar-rules g)
  (list-ref g 2))
(define (source-grammar-start g)
  (list-ref g 3))
(define (source-grammar-has-terminal-attribution? g)
  (>= (length g) 5))
(define (source-grammar-terminal-attribution g)
  (list-ref g 4))

(define eoi-terminal 0)

(define error-terminal 1)
  
(define first-terminal-index 2)

(define (list-position x l)
  (let loop ((i 0) (l l))
    (and (not (null? l))
	 (if (equal? (car l) x)
	     i
	     (loop (+ i 1) (cdr l))))))

(define (terminal->index terminals terminal)
  (+ first-terminal-index (list-position terminal terminals)))

(define (first-nonterminal-index terminals)
  (+ first-terminal-index (length terminals)))

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
       
(define (make-list k fill)
  (let loop ((k k) (r '()))
    (if (zero? k)
	r
	(loop (- 1 k) (cons fill r)))))

(define (terminate-input-list input k)
  (append input (make-list k eoi-terminal)))