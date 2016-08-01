NAME = obeanstalk
OCAMLBUILD = ocamlbuild -use-ocamlfind -package threads -lflags -thread

INSTALL_TARGETS = beanstalk.cma beanstalk.cmxa beanstalk.cmi beanstalk.a \
				  beanstalk.mli

INSTALL = $(addprefix _build/lib/, $(INSTALL_TARGETS))

TARGETS = lib/obean.native lib_test/test_pure.native lib_test/test_obs.native \
		  examples/job_create.native examples/deadline_soon.native

default: dev

dev:
	make build
	make tags

build:
	$(OCAMLBUILD) $(INSTALL_TARGETS)
	$(OCAMLBUILD) $(TARGETS)

opam-install:
	make clean
	make build
	make install

opam-remove:
	make uninstall

all:
	make build
	make doc
	make test

test:
	./test_pure.native

doc:
	$(OCAMLBUILD) obeanstalk.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

install:
	ocamlfind install $(NAME) META $(INSTALL)

uninstall:
	ocamlfind remove $(NAME)

ncat:
	rlwrap ncat -C 0.0.0.0 11300

.PHONY: build all build default install uninstall tags dev opam-install opam-remove
