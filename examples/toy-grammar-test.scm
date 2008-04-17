;; Rudimentary test suite

(define-test-suite toy-grammar-tests)

(define-test-case g08 toy-grammar-tests
  (check (parse g08 1 'lr (list->stream i08-1)) => #f)
  (check (parse g08 1 'slr (list->stream i08-1)) => #f)
  (check-exception (parse g08 1 'lr (list->stream i08-2)))
  (check-exception (parse g08 1 'slr (list->stream i08-2)))
  (check-exception (parse g08 1 'lr (list->stream i08-3)))
  (check-exception (parse g08 1 'slr (list->stream i08-3))))

(define-test-case g10 toy-grammar-tests
  (check (parse g10 1 'lr (list->stream i10-1)) => 147)
  (check (parse g10 1 'slr (list->stream i10-1)) => 147)
  (check-exception (parse g10 1 'lr (list->stream i10-2)))
  (check-exception (parse g10 1 'slr (list->stream i10-2)))
  (check-exception (parse g10 1 'lr (list->stream i10-3)))
  (check-exception (parse g10 1 'slr (list->stream i10-3))))

(define-test-case g10-error toy-grammar-tests
  (check (parse g10-error 1 'lr (list->stream i10e-1)) => 147)
  (check (parse g10-error 1 'slr (list->stream i10e-1)) => 147)
  (check (parse g10-error 1 'lr (list->stream i10e-2)) => 28)
  (check (parse g10-error 1 'slr (list->stream i10e-2)) => 28)
  (check-exception (parse g10-error 1 'lr (list->stream i10e-3)))
  (check-exception (parse g10-error 1 'slr (list->stream i10e-3))))

(define-test-case g14 toy-grammar-tests
  (check (parse g14 1 'lr (list->stream i14-1)) => #f)
  (check (parse g14 1 'slr (list->stream i14-1)) => #f))
