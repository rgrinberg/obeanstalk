NAME = obeanstalk
OCAMLBUILD = ocamlbuild -use-ocamlfind
TARGETS = lib/obean.native lib_test/test_pure.native lib_test/test_obs.native \
		  examples/job_create.native

default: all

build:
	$(OCAMLBUILD) $(TARGETS)
	# otags -I ~/.opam/4.00.1+short-types/lib/type_conv/ -I ~/.opam/4.00.1+short-types/lib/sexplib -pa pa_type_conv.cma -pa pa_sexp_conv.cma ./lib ./lib_test -r -vi

job_create:
	./dist/build/job_create/job_create

all:
	make build
	make test

test:
	./test_pure.native

clean:
	$(OCAMLBUILD) -clean
	
.PHONY: build all build default
