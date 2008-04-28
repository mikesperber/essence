;; This depends on packages.scm

(define-structure essence-cps-lr-generate essence-parser-generate-interface
  (open scheme
	essence-cps-lr-genext pgg-specialize
	cogen-gensym cogen-globals
	big-util)
  (begin
    (define (generate-parser grammar lookahead method goal-name)
					; (gensym-ignore-name-stubs!)
      (set-generate-flat-program! #t)
      (set-lambda-is-pure! #f)
      (specialize compute-parser
		  '(compute-parser 0 0 0 0 1)
		  (list grammar lookahead method 0 'input)
		  goal-name)
      (append (filter (lambda (form)	; massive kludge
			(not (eq? 'define-data (car form))))
		      *support-code*)
	      (get-residual-program)))))

;; Batch version

(define-structure essence-grammar-scratch-package (export *grammar-scratch-package*)
  (open scheme essence-grammars)
  (begin
    (define *grammar-scratch-package* (interaction-environment))))

(define-structure essence-cps-lr-parser-generator essence-main-interface
  (open scheme
	essence-options
	essence-cps-lr-generate
	essence-grammar-scratch-package
	i/o os-strings big-util formats exceptions conditions pp)
  (files (src main)))
