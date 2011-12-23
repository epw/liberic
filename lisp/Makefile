ROOT = /usr/local

all:

install:
	mkdir -p $(ROOT)/share/common-lisp/source/eric
	cp eric.lisp eric.asd $(ROOT)/share/common-lisp/source/eric/.
	ln -s $(ROOT)/share/common-lisp/source/eric/eric.asd \
		$(ROOT)/share/common-lisp/systems/eric.asd
