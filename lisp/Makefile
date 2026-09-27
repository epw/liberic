ROOT = /usr/local

all:

%install:
	mkdir -p $(ROOT)/share/common-lisp/source/eric

install-eric: %install
	cp eric.lisp eric.asd $(ROOT)/share/common-lisp/source/eric/.
	ln -sf $(ROOT)/share/common-lisp/source/eric/eric.asd \
		$(ROOT)/share/common-lisp/systems/eric.asd

install-eric-test: %install
	cp eric-test.lisp eric-test.asd $(ROOT)/share/common-lisp/source/eric/.
	ln -sf $(ROOT)/share/common-lisp/source/eric/eric-test.asd \
		$(ROOT)/share/common-lisp/systems/eric-test.asd

install-strptime: %install
	cp strptime.lisp strptime.asd $(ROOT)/share/common-lisp/source/eric/.
	ln -sf $(ROOT)/share/common-lisp/source/eric/strptime.asd \
		$(ROOT)/share/common-lisp/systems/strptime.asd

install: install-eric install-eric-test install-strptime

