include ../Makefile.config

gambit: g10-bench-gambit g12-bench-gambit

gambit_link = ../common/r5rs-gambit-gambit.c \
	../common/big-gambit-gambit.c ../common/tiny-format-gambit.c \
	../common/source-grammar-gambit.c ../common/lr-runtime-gambit.c \
	../direct/parse-result-gambit.c \
	../examples/%-direct-lr-gambit.c \
	%-bench-terminals-gambit.c bench-gambit.c  \
	gambit-bench-gambit.c

%-bench-gambit: $(gambit_link:.c=.o) %-bench-gambit_.o
	$(GAMBIT_CC) $^ $(LDFLAGS) $(GAMBIT_LDFLAGS) -o $@
	strip $@

%-bench-terminals.scm: ../examples/%.dat
	../common/extract-terminals $< bench-terminals > $@

.PRECIOUS: %-bench-gambit_.c
%-bench-gambit_.c: $(gambit_link)
	$(GSC) -link $^
	mv gambit-bench-gambit_.c $@

