include Makefile.config

.PHONY: cps direct examples common benchmarks

direct:
	cd direct; $(MAKE)
direct_gambit:
	cd direct; $(MAKE) gambit

examples: examples_direct
examples_direct: direct
	cd examples; $(MAKE) direct
examples_gambit:
	cd examples; $(MAKE) gambit


benchmarks: examples common
benchmarks_gambit: examples_gambit common_gambit direct_gambit
	cd benchmarks; $(MAKE) gambit

common:
	cd common; $(MAKE)
common_gambit:
	cd common; $(MAKE) gambit

gambit_examples: gambit_examples_direct
gambit_examples_direct: direct
	cd examples; $(MAKE) gambit_examples

