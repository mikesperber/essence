SHELL = /bin/sh

prefix = /afs/informatik.uni-tuebingen.de/home/sperber/temp/essence-1.0
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
sharedir = $(prefix)/share
incdir = $(prefix)/include
docdir = $(prefix)/doc
mandir = $(prefix)/man/man$(manext)

TEMPDIR = /tmp

INSTALL = /bin/installbsd -c -gPUstaff
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

RUNNABLE = essence

TAR = tar # must be GNU tar, probably
ZCAT = zcat
SCHEME48 = scheme48
PGG_DIRECTORY = ../cps-mcogen
PGG_PACKAGES = $(PGG_DIRECTORY)/genext-packages.scm \
	$(PGG_DIRECTORY)/pgg-packages.scm
GENEXT_BASE = genext
GENEXT_VERSION = 1.0
GENEXT_DIRECTORY = genext
GENEXT_PACKAGES = $(GENEXT_DIRECTORY)/genext-packages.scm
PGG_HEAPSIZE = 8000000
GENEXT_HEAPSIZE = 16000000

# In theory, you can change these:
PARSER_BASE_DIRECTORY = cps
PARSER_VARIANT = cps-lr
LOOKAHEAD_SOURCE = common/lookahead-trie.scm

PARSER_BASE = $(PARSER_BASE_DIRECTORY)/$(PARSER_VARIANT)

PARSER_GENEXT_SUPPORT_SOURCE_FILES = \
	$(PARSER_BASE).scm \
	common/the-trick.scm \
	common/lookahead.scm \
	$(LOOKAHEAD_SOURCE) \
	common/pure.scm \
	common/memo.scm

PARSER_SUPPORT_SOURCE_FILES = \
	common/stream.scm \
	common/grammar.scm \
	common/lr-spectime.scm

BATCH_SUPPORT_SOURCE_FILES = \
	common/command-line.scm \
	common/main.scm

PARSER_LIB_SOURCE_FILES = common/stream.scm

PARSER_CONFIG_FILES = \
	interfaces.scm \
	packages.scm

PARSER_GENEXT_SOURCE_FILES = \
	$(PARSER_BASE)-genext.scm

PARSER_GENEXT_CONFIG_FILES = \
	$(PARSER_CONFIG_FILES) \
	$(GENEXT_PACKAGES) \
	$(PARSER_BASE)-genext.config.scm \
	generator-packages.scm

PARSER_SHARE_DIRECTORIES = \
	common \
	$(PARSER_BASE_DIRECTORY) \
	$(GENEXT_DIRECTORY)

IMAGE = essence.image

BATCH_IMAGE = essence-batch.image

default: batch

install: install-directories install-script install-share install-images

install-images: install-image install-batch

install-share:
	for f in $(PARSER_LIB_SOURCE_FILES); do 			\
		$(INSTALL_DATA)  $$f $(sharedir)/$$f || exit 1;		\
	done &&								\
	for f in $(PARSER_SUPPORT_SOURCE_FILES); do 			\
		$(INSTALL_DATA) $$f $(sharedir)/$$f || exit 1;		\
	done &&								\
	for f in $(PARSER_GENEXT_SUPPORT_SOURCE_FILES); do 		\
		$(INSTALL_DATA) $$f $(sharedir)/$$f || exit 1;		\
	done &&								\
	for f in $(PARSER_GENEXT_SOURCE_FILES); do 			\
		$(INSTALL_DATA) $$f $(sharedir)/$$f || exit 1;		\
	done &&								\
	for f in $(PARSER_GENEXT_CONFIG_FILES); do 			\
		$(INSTALL_DATA) $$f $(sharedir)/$$f || exit 1;		\
	done &&								\
	for f in $(GENEXT_DIRECTORY)/*; do				\
		$(INSTALL_DATA) $$f $(sharedir)/$$f || exit 1;		\
	done &&								\
	for f in $(BATCH_SUPPORT_SOURCE_FILES); do 			\
		$(INSTALL_DATA) $$f $(sharedir)/$$f || exit 1;		\
	done

install-script:
	script=$(bindir)/$(RUNNABLE) &&					\
	echo '#!/bin/sh'	>$$script &&				\
	echo 'exec $(SCHEME48) -i $(libdir)/$(BATCH_IMAGE) -h $(GENEXT_HEAPSIZE) -a $$@' \
	     >>$$script &&						\
	chmod +x $$script

$(RUNNABLE): batch
	script=$(RUNNABLE) &&						\
	echo '#!/bin/sh'	>$$script &&				\
	echo "lib=`pwd`"	>>$$script &&				\
	echo 'exec $(SCHEME48) -i $$lib/$(BATCH_IMAGE) -h $(GENEXT_HEAPSIZE) -a $$@' \
	     >>$$script &&						\
	chmod +x $$script

install-directories:
	mkdir -p $(prefix) &&						\
	mkdir -p $(exec_prefix) &&					\
	mkdir -p $(bindir) &&						\
	mkdir -p $(libdir) &&						\
	mkdir -p $(sharedir) &&						\
	for f in $(PARSER_SHARE_DIRECTORIES); do			\
		mkdir -p $(sharedir)/$$f || exit 1;			\
	done &&								\
	mkdir -p $(incdir)

clean:
	rm -f $(BATCH_IMAGE) $(IMAGE)

pristine: clean
	rm -rf $(GENEXT_DIRECTORY)

batch: $(BATCH_IMAGE)

$(BATCH_IMAGE): $(IMAGE) \
	common/main.scm
	(echo ',batch on';						\
	 echo ",open $(PARSER_VARIANT)-parser-generator";		\
	 echo ",build main $(BATCH_IMAGE)";				\
	 echo ',exit'							\
	)  | $(SCHEME48) -h $(GENEXT_HEAPSIZE) -i $(IMAGE)

install-batch:
	rm -f $(TEMPDIR)/$(BATCH_IMAGE) &&				\
	(echo ',batch on';						\
	 echo ",open $(PARSER_VARIANT)-parser-generator";		\
	 echo ",build main $(TEMPDIR)/$(BATCH_IMAGE)";			\
	 echo ',exit'							\
	)  | $(SCHEME48) -h $(GENEXT_HEAPSIZE) -i $(libdir)/$(IMAGE) &&	\
	$(INSTALL_DATA) $(TEMPDIR)/$(BATCH_IMAGE) $(libdir) &&		\
	rm $(TEMPDIR)/$(BATCH_IMAGE)

image: $(IMAGE)

$(IMAGE): $(PARSER_BASE)-genext.scm $(PARSER_BASE)-genext.config.scm \
	$(PARSER_GENEXT_CONFIG_FILES)
	(echo ',batch on';						\
	 echo ",translate =essence/ `pwd`/";				\
	 echo -n ',config ,load ';					\
	 for config in $(PARSER_GENEXT_CONFIG_FILES); do		\
		echo -n =essence/$$config;				\
	 done;								\
	 echo;								\
	 echo ",load-package $(PARSER_VARIANT)";			\
	 echo ",load-package $(PARSER_VARIANT)-generate";		\
	 echo ",dump $(TEMPDIR/$(IMAGE)";				\
	 echo ',exit'							\
	) | $(SCHEME48) -h $(GENEXT_HEAPSIZE)

install-image: genext \
	$(PARSER_GENEXT_CONFIG_FILES)
	rm -f $(TEMPDIR)/$(IMAGE) &&					\
	(echo ',batch on';						\
	 echo ",translate =essence/ `pwd`/";				\
	 for config in $(PARSER_GENEXT_CONFIG_FILES); do		\
		echo ",config ,load =essence/$$config";			\
	 done;								\
	 echo ",load-package $(PARSER_VARIANT)";			\
	 echo ",load-package $(PARSER_VARIANT)-generate";		\
	 echo ",translate =essence/ $(sharedir)/";			\
	 echo ",dump $(TEMPDIR)/$(IMAGE)";				\
	 echo ',exit'							\
	) | $(SCHEME48) -h $(GENEXT_HEAPSIZE) &&			\
	$(INSTALL_DATA) $(TEMPDIR)/$(IMAGE) $(libdir) &&		\
	rm $(TEMPDIR)/$(IMAGE)

genext: $(PARSER_BASE)-genext.scm $(PARSER_BASE)-genext.config.scm

$(PARSER_BASE)-genext.scm $(PARSER_BASE)-genext.config.scm: \
	$(PARSER_GENEXT_SUPPORT_SOURCE_FILES) \
	$(PARSER_SUPPORT_SOURCE_FILES)
	(echo ",config ,load $(PGG_PACKAGES)";	\
	 echo ',batch on';						\
	 echo ',open pgg';						\
	 echo ',open cogen-globals';					\
	 echo ',batch off';						\
	 echo "(begin							\
                 (cogen-driver '(\"$(PARSER_BASE).scm\"			\
		  \"common/the-trick.scm\"				\
		  \"$(LOOKAHEAD_SOURCE)\"				\
		  \"common/pure.scm\"					\
		  \"common/memo.scm\")					\
	          '(parse 0 0 0 1)					\
	          '(goal compute-parser)				\
	          '(open signals stream					\
		         grammar lr-spectime)				\
	          \"$(PARSER_BASE)-genext.scm\")				\
	         #t)";							\
	 echo ',exit'							\
	) | $(SCHEME48) -h $(PGG_HEAPSIZE)

import-genext:
	rm -rf $(GENEXT_DIRECTORY); \
	cd $(PGG_DIRECTORY); \
	$(MAKE) $(GENEXT_BASE)-$(GENEXT_VERSION).tar.gz
	mkdir $(GENEXT_DIRECTORY); \
	$(ZCAT) $(PGG_DIRECTORY)/$(GENEXT_BASE)-$(GENEXT_VERSION).tar.gz | (cd $(GENEXT_DIRECTORY); $(TAR) xvf -)
