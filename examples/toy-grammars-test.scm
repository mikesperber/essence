; Copyright (c) 2000-2008 by Michael Sperber and Peter Thiemann. See file COPYING.

;; Rudimentary test suite

(define-test-suite toy-grammars-tests)

(define-test-case g08 toy-grammars-tests
  (check (parse g08 1 'lr 0 i08-1) => #f)
  (check (parse g08 1 'slr 0 i08-1) => #f)
  (check-exception (parse g08 1 'lr 0 i08-2))
  (check-exception (parse g08 1 'slr 0 i08-2))
  (check-exception (parse g08 1 'lr 0 i08-3))
  (check-exception (parse g08 1 'slr 0 i08-3)))

(define-test-case g10 toy-grammars-tests
  (check (parse g10 1 'lr 0 i10-1) => 147)
  (check (parse g10 1 'slr 0 i10-1) => 147)
  (check-exception (parse g10 1 'lr 0 i10-2))
  (check-exception (parse g10 1 'slr 0 i10-2))
  (check-exception (parse g10 1 'lr 0 i10-3))
  (check-exception (parse g10 1 'slr 0 i10-3)))

(define-test-case g10-k=2 toy-grammars-tests
  (check (parse g10 2 'lr 0 i10-1) => 147)
  (check (parse g10 2 'slr 0 i10-1) => 147)
  (check-exception (parse g10 2 'lr 0 i10-2))
  (check-exception (parse g10 2 'slr 0 i10-2))
  (check-exception (parse g10 2 'lr 0 i10-3))
  (check-exception (parse g10 2 'slr 0 i10-3)))

(define-test-case g10-error toy-grammars-tests
  (check (parse g10-error 1 'lr 0 i10e-1) => 147)
  (check (parse g10-error 1 'slr 0 i10e-1) => 147)
  (check (parse g10-error 1 'lr 0 i10e-2) => 28)
  (check (parse g10-error 1 'slr 0 i10e-2) => 28)
  (check-exception (parse g10-error 1 'lr 0 i10e-3))
  (check-exception (parse g10-error 1 'slr 0 i10e-3)))

(define-test-case g14 toy-grammars-tests
  (check (parse g14 1 'lr 0 i14-1) => #f)
  (check (parse g14 1 'slr 0 i14-1) => #f))
