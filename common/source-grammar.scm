;; Format of source grammars and symbol representations
;; ====================================================

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
(define (make-eoi-terminal) eoi-terminal)

(define error-terminal 1)
(define (make-error-terminal) error-terminal)

(define first-terminal-index 2)

(define (terminal->index terminals terminal)
  (+ first-terminal-index (list-position terminal terminals)))

(define (first-nonterminal-index terminals)
  (+ first-terminal-index (length terminals)))

;; this doesn't really belong here, does it?

(define (make-list k fill)
  (let loop ((k k) (r '()))
    (if (zero? k)
	r
	(loop (- 1 k) (cons fill r)))))

(define (list-position x l)
  (let loop ((i 0) (l l))
    (and (not (null? l))
	 (if (equal? (car l) x)
	     i
	     (loop (+ i 1) (cdr l))))))