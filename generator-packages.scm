;; This depends on packages.scm

(define-module (make-parser-generate genext-structures)

  (define-structure generate-structures parser-generate-interface
    (open scheme
	  genext-structures pgg-specialize
	  cogen-gensym cogen-globals
	  big-util)
    (begin
      (define (generate-parser grammar lookahead method goal-name)
	; (gensym-ignore-name-stubs!)
	(set-generate-flat-program! #t)
	(set-lambda-is-pure! #f)
	(specialize compute-parser
		    '(compute-parser 0 0 0 1)
		    (list grammar lookahead method 'input)
		    goal-name)
	(append (filter (lambda (form)	; massive kludge
			  (not (eq? 'define-data (car form))))
			*support-code*)
		(get-residual-program)))))

  generate-structures)

(def cps-lr-generate (make-parser-generate cps-lr-genext))

;; Batch version

(define-structure grammar-scratch-package (export *grammar-scratch-package*)
  (open scheme grammar)
  (begin
    (define *grammar-scratch-package* (interaction-environment))))

(define-module (make-parser-generator generate-structures)

  (define-structure generator-structures main-interface
    (open scheme options
	  generate-structures
	  grammar-scratch-package
	  i/o os-strings big-util formats exceptions conditions pp)
    (files (common main)))
  
  generator-structures)

(def cps-lr-parser-generator (make-parser-generator cps-lr-generate))
