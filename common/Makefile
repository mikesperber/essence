include ../Makefile.config

all:
.PHONY: all gambit

gambit: $(runtime_support:.scm=-gambit.o) \
	extract-terminals \
	r5rs-gambit-gambit.o \
	big-gambit-gambit.o \
	tiny-format-gambit.o

extract-terminals-batch.scm: extract-terminals.scm
	(echo "(extract-terminals (vector-ref ##argv 1) (vector-ref ##argv 2))"; \
	 cat $<; \
	) > extract-terminals-batch.scm
extract-terminals-batch-gambit_.c: source-grammar-gambit.c \
	extract-terminals-batch-gambit.c
	$(GSC) -link $^
extract-terminals: extract-terminals-batch-gambit_.o \
	extract-terminals-batch-gambit.o \
	source-grammar-gambit.o
	$(GAMBIT_CC)  $^ $(LDFLAGS) $(GAMBIT_LDFLAGS) -o $@
	strip $@