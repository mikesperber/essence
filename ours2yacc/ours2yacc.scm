; ((...(initial binop l1) binop l2) ... binop ln)
(define (foldl binop initial list)
  (let loop ((val initial) (rest list))
    (if (null? rest)
	val
	(loop (binop val (car rest)) (cdr rest)))))

(define (grammar-nonterminals grammar)
  (list-ref grammar 0))

(define (grammar-terminals grammar)
  (list-ref grammar 1))

(define (grammar-productions grammar)
  (list-ref grammar 2))

(define (grammar-start grammar)
  (list-ref grammar 3))

(define production-lhs car)
(define production-rhs cdr)

(define (generate-declarations)
  (display "%{") (newline)
  (display "#include <stdio.h>") (newline)
  (display "#include \"clock.h\"") (newline)
  (display "%}") (newline))

(define (make-token-associations terminals)
  (do ((i 257 (+ 1 i))
       (t terminals (cdr t))
       (l '() (cons (cons (car t) i) l)))
      ((null? t) l)))

(define strange-nonterminal-table
  '((if . my-if)))

(define strange-chars-table
  '((#\+ . #\P)
    (#\- . #\M)
    (#\* . #\T)
    (#\/ . #\O)
    (#\= . #\E)))

(define (nonterminal->token symbol)
  (let ((a (assq symbol strange-nonterminal-table)))
    (if a
	(nonterminal->token (cdr a))
	(let ((s (string-copy (symbol->string symbol))))
	  (do ((l (string-length s))
	       (i 0 (+ 1 i)))
	      ((= i l) s)
	    (let ((a (assv (string-ref s i) strange-chars-table)))
	      (if a
		  (string-set! s i (cdr a)))))))))
	
	 

(define (generate-tokens a)
  (for-each
   (lambda (p)
     (display "%token ")
     (display (nonterminal->token (car p)))
     (display #\space)
     (display (cdr p))
     (newline))
   a))
  
(define (generate-start s)
  (display "%start ")
  (display (nonterminal->token s))
  (newline))

(define (generate-rule p)
  (display (nonterminal->token (car p)))
  (display ": ")
  (for-each (lambda (r)
	      (display (nonterminal->token r))
	      (display #\space))
	    (cdr p))
  (display ";")
  (newline))

(define (generate-rules productions)
  (for-each generate-rule productions))

(define (generate-test-data test associations)
  (for-each
   (lambda (t)
     (display "  ")
     (display (cdr (assq t associations)))
     (display ",")
     (newline))
   test)
  (display "  0")
  (newline))

(define (generate-tests-data tests associations)
  (do ((i 1 (+ 1 i))
       (tests tests (cdr tests)))
      ((null? tests))
    (display "int test")
    (display i)
    (display "[] = {")
    (newline)
    (generate-test-data (car tests) associations)
    (display "};")
    (newline)))

(define (generate-scanner)
  (display "int *p_test;") (newline)
  (display "void init_scanner(int new_test[])") (newline)
  (display "{") (newline)
  (display "  p_test = new_test;") (newline)
  (display "}") (newline)
  (display "int yylex(void)") (newline)
  (display "{") (newline)
  (display "  return(*p_test++);") (newline)
  (display "}") (newline))
  

(define (generate-main tests)
  (display "int main(void)") (newline)
  (display "{") (newline)
  (display "  int i;") (newline)
  (display "  long t1, t2;") (newline)
  (do ((i 1 (+ 1 i))
       (tests tests (cdr tests)))
      ((null? tests))
    (display "  printf(\"Test ") (display i) (display ":\\n\");") (newline)
    (display "  t1 = clock_long();") (newline)
    (display "  for (i = 100000; i; --i) {") (newline)
    (display "    init_scanner(test") (display i) (display ");") (newline)
    (display "    yyparse();") (newline)
    (display "  }") (newline)
    (display "  t2 = clock_long();") (newline)
    (display "  printf(\"Test ") (display i) (display ": %ld msecs (for %d runs)\\n\", clock_diff(t1, t2), 100000);") (newline))
  (display "}") (newline))
    
(define (make-yacc g . tests)
  (let* ((terminals (grammar-terminals g))
	 (nonterminals (grammar-nonterminals g))
	 (productions (grammar-productions g))
	 (start (grammar-start g))
	 (tokens-numbers (make-token-associations terminals)))
    (generate-declarations)
    (generate-tokens tokens-numbers)
    (generate-start start)
    (display "%%") (newline)
    (generate-rules productions)
    (display "%%") (newline)
    (generate-tests-data tests tokens-numbers)
    (generate-scanner)
    (generate-main tests)))


(define (grammar->yacc-file file g . tests)
  (with-output-to-file file
    (lambda ()
      (apply make-yacc (cons g tests)))))